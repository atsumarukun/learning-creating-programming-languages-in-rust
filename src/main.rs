use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::recognize,
    error::ParseError,
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
};
use std::io::{BufReader, BufWriter, Read, Write};

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, Output = O, Error = E>,
) -> impl Parser<&'src str, Output = O, Error = E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

fn number(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (r, v) = space_delimited(recognize_float).parse(input)?;
    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))
    .parse(input)
}

fn ident(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (r, res) = space_delimited(identifier).parse(input)?;
    Ok((r, Expression::Ident(res)))
}

fn parens(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    space_delimited(delimited(tag("("), expr, tag(")"))).parse(input)
}

fn term(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    alt((number, ident, parens)).parse(input)
}

fn expr(input: &'_ str) -> IResult<&'_ str, Expression<'_>> {
    let (input, init) = term(input)?;

    fold_many0(
        pair(space_delimited(char('+')), term),
        move || init.clone(),
        |acc, (_, val): (char, Expression)| Expression::Add(Box::new(acc), Box::new(val)),
    )
    .parse(input)
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum OpCode {
    LoadLiteral,
    Copy,
    Add,
}

macro_rules! impl_op_from {
    ($($op:ident), *) => {
        impl From<u8> for OpCode {
            #[allow(non_upper_case_globals)]
            fn from(o: u8) -> Self {
                $(const $op: u8 = OpCode::$op as u8;)*

                match o {
                    $($op => Self::$op,)*
                    _ => panic!("Opcode \"{:02X}\" unrecognized!", 0),
                }
            }
        }
    };
}
impl_op_from!(LoadLiteral, Copy, Add);

#[repr(C)]
struct Instruction {
    op: OpCode,
    arg0: u8,
}

impl Instruction {
    fn new(op: OpCode, arg0: u8) -> Self {
        Self { op, arg0 }
    }

    fn serialize(&self, writer: &mut impl Write) -> Result<(), std::io::Error> {
        writer.write_all(&[self.op as u8, self.arg0])?;
        Ok(())
    }

    fn deserialize(reader: &mut impl Read) -> Result<Self, std::io::Error> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf)?;
        Ok(Self::new(buf[0].into(), buf[1]))
    }
}

fn serialize_size(sz: usize, writer: &mut impl Write) -> std::io::Result<()> {
    writer.write_all(&(sz as u32).to_le_bytes())
}

struct Compiler {
    literals: Vec<f64>,
    instructions: Vec<Instruction>,
    target_stack: Vec<usize>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            literals: vec![],
            instructions: vec![],
            target_stack: vec![],
        }
    }

    fn add_literal(&mut self, value: f64) -> u8 {
        let ret = self.literals.len();
        self.literals.push(value);
        ret as u8
    }

    fn add_inst(&mut self, op: OpCode, arg0: u8) -> usize {
        let inst = self.instructions.len();
        self.instructions.push(Instruction { op, arg0 });
        inst
    }

    fn add_copy_inst(&mut self, stack_idx: usize) -> usize {
        let inst = self.add_inst(
            OpCode::Copy,
            (self.target_stack.len() - stack_idx - 1) as u8,
        );
        self.target_stack.push(0);
        inst
    }

    fn write_literals(&self, writer: &mut impl Write) -> std::io::Result<()> {
        serialize_size(self.literals.len(), writer)?;
        for value in &self.literals {
            writer.write_all(&value.to_le_bytes())?;
        }
        Ok(())
    }

    fn write_instructions(&self, writer: &mut impl Write) -> std::io::Result<()> {
        serialize_size(self.instructions.len(), writer)?;
        for instruction in &self.instructions {
            instruction.serialize(writer).unwrap();
        }
        Ok(())
    }

    fn compile_expr(&mut self, ex: &Expression) -> usize {
        match ex {
            Expression::NumLiteral(num) => {
                let id = self.add_literal(*num);
                self.add_inst(OpCode::LoadLiteral, id);
                self.target_stack.push(id as usize);
                self.target_stack.len() - 1
            }
            Expression::Ident("pi") => {
                let id = self.add_literal(std::f64::consts::PI);
                self.add_inst(OpCode::LoadLiteral, id);
                self.target_stack.push(id as usize);
                self.target_stack.len() - 1
            }
            Expression::Ident(id) => panic!("Unknown identifer {id:?}"),
            Expression::Add(lhs, rhs) => {
                let lhs = self.compile_expr(lhs);
                let rhs = self.compile_expr(rhs);
                self.add_copy_inst(lhs);
                self.add_copy_inst(rhs);
                self.target_stack.pop();
                self.add_inst(OpCode::Add, 0);
                // self.target_stack.push(usize::MAX);
                self.target_stack.len() - 1
            }
        }
    }

    fn disasm(&self, writer: &mut impl Write) -> std::io::Result<()> {
        use OpCode::*;
        writeln!(writer, "Literals [{}]", self.literals.len())?;
        for (i, con) in self.literals.iter().enumerate() {
            writeln!(writer, " [{i}] {}", *con)?;
        }

        writeln!(writer, "Instructions [{}]", self.instructions.len())?;
        for (i, inst) in self.instructions.iter().enumerate() {
            match inst.op {
                LoadLiteral => writeln!(
                    writer,
                    " [{i}] {:?} {} ({:?})",
                    inst.op, inst.arg0, self.literals[inst.arg0 as usize]
                )?,
                Copy => writeln!(writer, " [{i}] {:?} {}", inst.op, inst.arg0)?,
                Add => writeln!(writer, " [{i}] Add")?,
            }
        }
        Ok(())
    }
}

fn deserialize_size(reader: &mut impl Read) -> std::io::Result<usize> {
    let mut buf = [0u8; std::mem::size_of::<u32>()];
    reader.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf) as usize)
}

struct ByteCode {
    literals: Vec<f64>,
    instructions: Vec<Instruction>,
}

impl ByteCode {
    fn new() -> Self {
        Self {
            literals: vec![],
            instructions: vec![],
        }
    }

    fn read_literals(&mut self, reader: &mut impl Read) -> std::io::Result<()> {
        let num_literals = deserialize_size(reader)?;
        for _ in 0..num_literals {
            let mut buf = [0u8; std::mem::size_of::<f64>()];
            reader.read_exact(&mut buf)?;
            self.literals.push(f64::from_le_bytes(buf));
        }
        Ok(())
    }

    fn read_instructions(&mut self, reader: &mut impl Read) -> std::io::Result<()> {
        let num_instructions = deserialize_size(reader)?;
        for _ in 0..num_instructions {
            let instruction = Instruction::deserialize(reader)?;
            self.instructions.push(instruction);
        }
        Ok(())
    }

    fn interpret(&self) -> Option<f64> {
        let mut stack = vec![];

        for instruction in &self.instructions {
            match instruction.op {
                OpCode::LoadLiteral => stack.push(self.literals[instruction.arg0 as usize]),
                OpCode::Add => {
                    let rhs = stack.pop().expect("Stack underflow");
                    let lhs = stack.pop().expect("Stack underflow");
                    stack.push(lhs + rhs);
                }
                OpCode::Copy => stack.push(stack[stack.len() - instruction.arg0 as usize - 1]),
            }
        }

        stack.pop()
    }
}

fn read_program(reader: &mut impl Read) -> std::io::Result<ByteCode> {
    let mut bytecode = ByteCode::new();
    bytecode.read_literals(reader)?;
    bytecode.read_instructions(reader)?;
    Ok(bytecode)
}

fn write_program(
    source: &str,
    writer: &mut impl Write,
    out_file: &str,
    disasm: bool,
) -> std::io::Result<()> {
    let mut compiler = Compiler::new();
    let (_, ex) =
        expr(source).map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_owned()))?;

    compiler.compile_expr(&ex);

    if disasm {
        compiler.disasm(&mut std::io::stdout())?;
    }

    compiler.write_literals(writer).unwrap();
    compiler.write_instructions(writer).unwrap();
    println!(
        "Written {} literals and {} instructions to {out_file:?}",
        compiler.literals.len(),
        compiler.instructions.len()
    );

    Ok(())
}

fn main() -> std::io::Result<()> {
    let mut args = std::env::args();
    args.next();
    match args.next().as_ref().map(|s| s as &str) {
        Some("-c") => {
            if let Some(source) = args.next() {
                let out_file = "bytecode.bin";
                let writer = std::fs::File::create(out_file).unwrap();
                let mut writer = BufWriter::new(writer);
                let mut disasm = false;
                if let Some(op) = args.next() {
                    if op == "-d" {
                        disasm = true;
                    }
                }
                write_program(&source, &mut writer, out_file, disasm)?;
            }
        }
        Some("-r") => {
            let reader = std::fs::File::open("bytecode.bin")?;
            let mut reader = BufReader::new(reader);
            if let Ok(bytecode) = read_program(&mut reader) {
                let result = bytecode.interpret();
                println!("result: {result:?}");
            }
        }
        _ => println!("Please specify -c or -r as an argument"),
    }
    Ok(())
}
