use failure::{format_err, Error};
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq, Default)]
struct Registers([usize; 4]);

#[derive(Fail, Debug)]
#[fail(display = "Could not parse registers. ({})", cause)]
pub struct ParseRegistersError {
    cause: String,
}

#[derive(Debug, Eq, PartialEq)]
struct Sample {
    before: Registers,
    instruction: Instruction,
    after: Registers
};

impl FromStr for Registers {
    type Err = ParseRegistersError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let start = s.find('[').ok_or(ParseRegistersError {
            cause: String::from("Could not find ["),
        })?;

        let end = s.find(']').ok_or(ParseRegistersError {
            cause: String::from("Could not find ["),
        })?;

        let between = &s[start + 1..end];

        let numbers: Vec<usize> = between
            .split_terminator(',')
            .map(|x| x.trim().parse::<usize>().unwrap())
            .collect();

        Ok(Registers::new([
            numbers[0], numbers[1], numbers[2], numbers[3],
        ]))
    }
}

impl Registers {
    fn new(values: [usize; 4]) -> Registers {
        Registers(values)
    }

    fn get(&self, Register(reg): &Register) -> Result<usize, Error> {
        if *reg > 3 {
            Err(format_err!("Only 4 registers are available. The maximum input index is therefore 3, but yours is {}", reg))
        } else {
            Ok(self.0[*reg])
        }
    }

    fn set(&mut self, Register(reg): &Register, value: usize) -> Result<(), Error> {
        if *reg > 3 {
            Err(format_err!("Only 4 registers are available. The maximum input index is therefore 3, but yours is {}", reg))
        } else {
            self.0[*reg] = value;
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Register(usize);

#[derive(Debug, PartialEq, Eq)]
struct Immediate(usize);

#[derive(Debug)]
struct Operation {
    output_register: Register,
    kind: OperationKind,
}

#[derive(Debug)]
enum OperationKind {
    Addr(Register, Register),
    Addi(Register, Immediate),
    Mulr(Register, Register),
    Muli(Register, Immediate),
    Banr(Register, Register),
    Bani(Register, Immediate),
    Borr(Register, Register),
    Bori(Register, Immediate),
    Setr(Register),
    Seti(Immediate),
    Gtir(Immediate, Register),
    Gtri(Register, Immediate),
    Gtrr(Register, Register),
    Eqir(Immediate, Register),
    Eqri(Register, Immediate),
    Eqrr(Register, Register),
}

#[derive(Debug, PartialEq, Eq)]
struct Instruction {
    op_code: usize,
    input_a: usize,
    input_b: usize,
    output: Register,
}

#[derive(Fail, Debug)]
#[fail(display = "Could not parse instruction")]
pub struct ParseInstructionError;

impl FromStr for Instruction {
    type Err = ParseInstructionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let numbers: Vec<usize> = s
            .split_whitespace()
            .map(|x| x.trim().parse::<usize>().unwrap())
            .collect();

        Ok(Instruction {
            op_code: numbers[0],
            input_a: numbers[1],
            input_b: numbers[2],
            output: Register(numbers[3]),
        })
    }
}

impl Operation {
    fn execute(&self, regs: &mut Registers) -> Result<(), Error> {
        use self::OperationKind::*;

        let result = match &self.kind {
            Addr(a, b) => regs.get(a)? + regs.get(b)?,
            Addi(a, Immediate(b)) => regs.get(a)? + b,
            Mulr(a, b) => regs.get(a)? * regs.get(b)?,
            Muli(a, Immediate(b)) => regs.get(a)? * b,
            Banr(a, b) => regs.get(a)? & regs.get(b)?,
            Bani(a, Immediate(b)) => regs.get(a)? & b,
            Borr(a, b) => regs.get(a)? | regs.get(b)?,
            Bori(a, Immediate(b)) => regs.get(a)? | b,
            Setr(a) => regs.get(a)?,
            Seti(Immediate(a)) => *a,
            Gtir(Immediate(a), b) => {
                if *a > regs.get(b)? {
                    1
                } else {
                    0
                }
            }
            Gtri(a, Immediate(b)) => {
                if regs.get(a)? > *b {
                    1
                } else {
                    0
                }
            }
            Gtrr(a, b) => {
                if regs.get(a)? > regs.get(b)? {
                    1
                } else {
                    0
                }
            }
            Eqir(Immediate(a), b) => {
                if *a > regs.get(b)? {
                    1
                } else {
                    0
                }
            }
            Eqri(a, Immediate(b)) => {
                if regs.get(a)? > *b {
                    1
                } else {
                    0
                }
            }
            Eqrr(a, b) => {
                if regs.get(a)? > regs.get(b)? {
                    1
                } else {
                    0
                }
            }
        };

        regs.set(&self.output_register, result)
    }
}

pub fn run(input: &str) -> Result<String, Error> {
    Ok(String::from("foo"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_exectues_addr() {
        let op = Operation {
            output_register: Register(3),
            kind: OperationKind::Addr(Register(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 2]));
    }

    #[test]
    fn it_exectues_addi() {
        let op = Operation {
            output_register: Register(3),
            kind: OperationKind::Addi(Register(0), Immediate(5)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 6]));
    }

    #[test]
    fn it_exectues_mulr() {
        let op = Operation {
            output_register: Register(3),
            kind: OperationKind::Mulr(Register(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 1]));
    }

    #[test]
    fn it_exectues_muli() {
        let op = Operation {
            output_register: Register(3),
            kind: OperationKind::Muli(Register(0), Immediate(5)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 5]));
    }

    #[test]
    fn it_exectues_banr() {
        let op = Operation {
            output_register: Register(3),
            kind: OperationKind::Banr(Register(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 1]));
    }

    #[test]
    fn it_exectues_bani() {
        let op = Operation {
            output_register: Register(3),
            kind: OperationKind::Bani(Register(0), Immediate(0)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 0]));
    }

    #[test]
    fn it_parses_registers() {
        let str = String::from("[ 1,2,3,4 ]");

        assert_eq!(
            str.parse::<Registers>().unwrap(),
            Registers::new([1, 2, 3, 4])
        );
    }

    #[test]
    fn it_parses_instruction() {
        let str = String::from("1 2 3 4");

        assert_eq!(
            str.parse::<Instruction>().unwrap(),
            Instruction {
                op_code: 1,
                input_a: 2,
                input_b: 3,
                output: Register(4)
            }
        );
    }
}
