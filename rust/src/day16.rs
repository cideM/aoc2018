use failure::{format_err, Error};
use std::collections::{HashMap, HashSet};
use std::str::FromStr;

const OPERATION_NAMES: [&str; 16] = [
    "addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri",
    "gtrr", "eqir", "eqri", "eqrr",
];

#[derive(Debug, Eq, PartialEq, Default, Clone)]
struct Sample {
    before: Registers,
    instruction: Instruction,
    after: Registers,
}

#[derive(Fail, Debug)]
#[fail(display = "Could not parse samples. ({})", cause)]
pub struct ParseSampleError {
    cause: String,
}

#[derive(Eq, PartialEq)]
enum SampleParseState {
    Before,
    Instruction,
    After,
}

#[derive(Eq, PartialEq, Debug)]
struct Samples(Vec<Sample>);

impl FromStr for Samples {
    type Err = ParseSampleError;

    // Each sample consists of three consecutive lines. The first one (starting
    // with "Before") contains registers, followed by an instruction, followed
    // by another line with registers ("After"). I go line by line, keeping
    // track of where we are by advancing the parse state. Every time we've
    // parsed an "After" line, we assemble the Sample and push it to the results
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut state = SampleParseState::Before;
        let mut samples: Vec<Sample> = Vec::new();

        let mut current: Sample = Default::default();

        for line in s.lines() {
            if state == SampleParseState::Before && line.starts_with("Before") {
                state = SampleParseState::Instruction;

                let before = line.parse::<Registers>().map_err(|err| ParseSampleError {
                    cause: format!("Could not parse before. {}", err),
                })?;

                current.before = before;
                continue;
            }

            if state == SampleParseState::Instruction {
                state = SampleParseState::After;

                let instruction = line.parse::<Instruction>().unwrap();

                current.instruction = instruction;
                continue;
            }

            if state == SampleParseState::After && line.starts_with("After") {
                state = SampleParseState::Before;

                let after = line.parse::<Registers>().map_err(|err| ParseSampleError {
                    cause: format!("Could not parse after. {}", err),
                })?;

                current.after = after;

                samples.push(current.clone());

                continue;
            }
        }

        Ok(Samples(samples))
    }
}

#[derive(Debug, Eq, PartialEq, Default, Clone)]
struct Registers([usize; 4]);

#[derive(Fail, Debug)]
#[fail(display = "Could not parse registers. ({})", cause)]
pub struct ParseRegistersError {
    cause: String,
}

impl FromStr for Registers {
    type Err = ParseRegistersError;

    // Get the numbers between [ and ]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let start = s.find('[').ok_or(ParseRegistersError {
            cause: String::from("Could not find ["),
        })?;

        let end = s.find(']').ok_or(ParseRegistersError {
            cause: String::from("Could not find ]"),
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

    fn get(&self, Register(reg): Register) -> Result<usize, Error> {
        if reg > 3 {
            Err(format_err!("Only 4 registers are available. The maximum input index is therefore 3, but yours is {}", reg))
        } else {
            Ok(self.0[reg])
        }
    }

    fn set(&mut self, Register(reg): Register, value: usize) -> Result<(), Error> {
        if reg > 3 {
            Err(format_err!("Only 4 registers are available. The maximum input index is therefore 3, but yours is {}", reg))
        } else {
            self.0[reg] = value;
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
struct Register(usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Immediate(usize);

#[derive(Debug)]
struct Operation {
    output_register: Register,
    kind: OpKind,
}

impl Operation {
    fn execute(&self, regs: &mut Registers) -> Result<(), Error> {
        use self::OpKind::*;

        let result = match self.kind {
            Addr(a, b) => regs.get(a)? + regs.get(b)?,
            Addi(a, Immediate(b)) => regs.get(a)? + b,
            Mulr(a, b) => regs.get(a)? * regs.get(b)?,
            Muli(a, Immediate(b)) => regs.get(a)? * b,
            Banr(a, b) => regs.get(a)? & regs.get(b)?,
            Bani(a, Immediate(b)) => regs.get(a)? & b,
            Borr(a, b) => regs.get(a)? | regs.get(b)?,
            Bori(a, Immediate(b)) => regs.get(a)? | b,
            Setr(a) => regs.get(a)?,
            Seti(Immediate(a)) => a,
            Gtir(Immediate(a), b) => {
                if a > regs.get(b)? {
                    1
                } else {
                    0
                }
            }
            Gtri(a, Immediate(b)) => {
                if regs.get(a)? > b {
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
                if a == regs.get(b)? {
                    1
                } else {
                    0
                }
            }
            Eqri(a, Immediate(b)) => {
                if regs.get(a)? == b {
                    1
                } else {
                    0
                }
            }
            Eqrr(a, b) => {
                if regs.get(a)? == regs.get(b)? {
                    1
                } else {
                    0
                }
            }
        };

        regs.set(self.output_register, result)
    }

    fn to_operation_name(&self) -> OperationName {
        match self.kind {
            OpKind::Addr(_, _) => OperationName::new("addr"),
            OpKind::Addi(_, _) => OperationName::new("addi"),
            OpKind::Mulr(_, _) => OperationName::new("mulr"),
            OpKind::Muli(_, _) => OperationName::new("muli"),
            OpKind::Banr(_, _) => OperationName::new("banr"),
            OpKind::Bani(_, _) => OperationName::new("bani"),
            OpKind::Borr(_, _) => OperationName::new("borr"),
            OpKind::Bori(_, _) => OperationName::new("bori"),
            OpKind::Gtir(_, _) => OperationName::new("gtir"),
            OpKind::Gtri(_, _) => OperationName::new("gtri"),
            OpKind::Gtrr(_, _) => OperationName::new("gtrr"),
            OpKind::Eqir(_, _) => OperationName::new("eqir"),
            OpKind::Eqri(_, _) => OperationName::new("eqri"),
            OpKind::Eqrr(_, _) => OperationName::new("eqrr"),
            OpKind::Seti(_) => OperationName::new("seti"),
            OpKind::Setr(_) => OperationName::new("setr"),
        }
    }
}

#[derive(Debug)]
enum OpKind {
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

#[derive(Debug, PartialEq, Eq, Default, Clone)]
struct Instruction {
    op_code: usize,
    input_a: usize,
    input_b: usize,
    output: Register,
}

#[derive(Fail, Debug)]
#[fail(display = "Could not parse instructions. {}", cause)]
pub struct ParseInstructionError {
    cause: String,
}

impl Instruction {
    // Operations are type safe in the sense that you can't just put numbers
    // into slots that expect Register and Immediate values, even though
    // essentially all of them are numbers. The advantage is that you don't
    // accidentally put a number supposed to be a Register into a slot expecting
    // an Immediate. The downside is that op_kind of an operation isn't just the
    // kind, it's the kind *plus the input parameters*. Therefore I also use
    // string keys for operations and map those keys to operations and vice
    // versa. This is very relevant for the map from instruction op code to
    // operation kind in part 2.
    fn to_operation(&self, op_name: &OperationName) -> Result<Operation, Error> {
        let &Instruction {
            input_a,
            input_b,
            output,
            ..
        } = self;

        let op_kind = match op_name.0.as_ref() {
            "addr" => Some(OpKind::Addr(Register(input_a), Register(input_b))),
            "addi" => Some(OpKind::Addi(Register(input_a), Immediate(input_b))),
            "mulr" => Some(OpKind::Mulr(Register(input_a), Register(input_b))),
            "muli" => Some(OpKind::Muli(Register(input_a), Immediate(input_b))),
            "banr" => Some(OpKind::Banr(Register(input_a), Register(input_b))),
            "bani" => Some(OpKind::Bani(Register(input_a), Immediate(input_b))),
            "borr" => Some(OpKind::Borr(Register(input_a), Register(input_b))),
            "bori" => Some(OpKind::Bori(Register(input_a), Immediate(input_b))),
            "setr" => Some(OpKind::Setr(Register(input_a))),
            "seti" => Some(OpKind::Seti(Immediate(input_a))),
            "gtir" => Some(OpKind::Gtir(Immediate(input_a), Register(input_b))),
            "gtri" => Some(OpKind::Gtri(Register(input_a), Immediate(input_b))),
            "gtrr" => Some(OpKind::Gtrr(Register(input_a), Register(input_b))),
            "eqir" => Some(OpKind::Eqir(Immediate(input_a), Register(input_b))),
            "eqri" => Some(OpKind::Eqri(Register(input_a), Immediate(input_b))),
            "eqrr" => Some(OpKind::Eqrr(Register(input_a), Register(input_b))),
            _ => None,
        };

        if let Some(op_kind) = op_kind {
            Ok(Operation {
                kind: op_kind,
                output_register: output,
            })
        } else {
            return Err(format_err!("Unknown operation name {:?}", op_name));
        }
    }
}

impl FromStr for Instruction {
    type Err = ParseInstructionError;

    // This is realy just parsing 1,2,3,4. Pretty verbose :|
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.split_whitespace()
            .map(|x| {
                x.trim()
                    .parse::<usize>()
                    .map_err(|err| ParseInstructionError {
                        cause: format!("{:?}", err),
                    })
            })
            .collect::<Result<Vec<usize>, ParseInstructionError>>()
            .map(|ns| Instruction {
                op_code: ns[0],
                input_a: ns[1],
                input_b: ns[2],
                output: Register(ns[3]),
            })
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
struct OperationName(String);

impl OperationName {
    fn new<S: Into<String>>(s: S) -> OperationName {
        OperationName(s.into())
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Program {
    instructions: Vec<Instruction>,
}

impl Program {
    fn run(&self, regs: &mut Registers, map: &OpCodeMap) -> Result<(), Error> {
        for ins in self.instructions.iter() {
            if let Some(op_name) = map.0.get(&ins.op_code) {
                let op = ins.to_operation(&op_name)?;
                op.execute(regs)?;
            } else {
                return Err(format_err!(
                    "No operation for instruction with op_code {}",
                    ins.op_code
                ));
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
struct OpCodeMap(HashMap<usize, OperationName>);

// TODO: Sample should prob have something like sample.execute_operations(ops: Vec<Operations>)
impl OpCodeMap {
    fn from_samples(samples: &Samples) -> Option<OpCodeMap> {
        let mut map: HashMap<usize, OperationName> = HashMap::new();
        let mut known: HashSet<OperationName> = HashSet::new();

        for sample in samples.0.iter() {
            let Sample {
                before,
                instruction,
                after,
            } = sample;

            if map.contains_key(&instruction.op_code) {
                continue;
            };

            let operations: Option<Vec<Operation>> = OPERATION_NAMES
                .into_iter()
                .map(|&op_name| instruction.to_operation(&OperationName::new(op_name)).ok())
                .collect();

            let unknown_operations_matching_before_after: Vec<OperationName> = operations?
                .into_iter()
                .filter(|op| {
                    // Restore the "before" of the given sample, since
                    // execute mutates it
                    let mut before = before.clone();
                    op.execute(&mut before).unwrap();

                    before == *after && !known.contains(&op.to_operation_name())
                })
                .map(|op| op.to_operation_name())
                .collect();

            if unknown_operations_matching_before_after.len() == 1 {
                map.insert(
                    instruction.op_code,
                    unknown_operations_matching_before_after[0].clone(),
                );
                known.insert(unknown_operations_matching_before_after[0].clone());
            }
        }

        if map.len() != OPERATION_NAMES.len() {
            None
        } else {
            Some(OpCodeMap(map))
        }
    }
}

impl FromStr for Program {
    type Err = ParseInstructionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| {
                l.parse::<Instruction>()
                    .map_err(|err| ParseInstructionError {
                        cause: format!("{:?}", err),
                    })
            })
            .collect::<Result<Vec<Instruction>, ParseInstructionError>>()
            .map(|ins| Program { instructions: ins })
    }
}

pub fn run(input: &str) -> Result<String, Error> {
    let lines = input.lines().collect::<Vec<&str>>();

    let lines = lines
        .rsplitn(2, |line| line.starts_with("After"))
        .collect::<Vec<&[&str]>>();

    let input_p1 = lines[1];
    let input_p2 = lines[0];

    let samples = input_p1.join("\n").parse::<Samples>()?;

    // Part 1
    let mut samples_matching_before_after: usize = 0;

    for sample in samples.0.iter() {
        let Sample {
            before,
            instruction,
            after,
        } = sample;

        let operations: Result<Vec<Operation>, Error> = OPERATION_NAMES
            .iter()
            .map(|&op_name| instruction.to_operation(&OperationName::new(op_name)))
            .collect();

        let operations_matching_before_after: Vec<Operation> = operations?
            .into_iter()
            .filter(|op| {
                // Restore the "before" of the given sample, since
                // execute mutates it
                let mut before = before.clone();

                op.execute(&mut before).unwrap();

                before == *after
            })
            .collect();

        if operations_matching_before_after.len() >= 3 {
            samples_matching_before_after += 1;
        }
    }

    let program = input_p2.join("\n").parse::<Program>()?;

    if let Some(map) = OpCodeMap::from_samples(&samples) {
        let mut regs = Registers([0, 0, 0, 0]);

        program.run(&mut regs, &map)?;

        Ok(format!(
            "Part 1: {}, part 2: {:?}",
            samples_matching_before_after, regs
        ))
    } else {
        Err(format_err!("Could not create map from op code to op name"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_exectues_addr() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Addr(Register(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 2]));
    }

    #[test]
    fn it_exectues_addi() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Addi(Register(0), Immediate(5)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 6]));
    }

    #[test]
    fn it_exectues_mulr() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Mulr(Register(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 1]));
    }

    #[test]
    fn it_exectues_muli() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Muli(Register(0), Immediate(5)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 5]));
    }

    #[test]
    fn it_exectues_banr() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Banr(Register(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 1]));
    }

    #[test]
    fn it_exectues_bani() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Bani(Register(0), Immediate(0)),
        };

        let mut regs: Registers = Registers::new([1, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([1, 1, 0, 0]));
    }

    #[test]
    fn it_exectues_borr() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Borr(Register(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([0, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([0, 1, 0, 1]));
    }

    #[test]
    fn it_exectues_bori() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Bori(Register(0), Immediate(1)),
        };

        let mut regs: Registers = Registers::new([0, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([0, 1, 0, 1]));
    }

    #[test]
    fn it_exectues_setr() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Setr(Register(1)),
        };

        let mut regs: Registers = Registers::new([0, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([0, 1, 0, 1]));
    }

    #[test]
    fn it_exectues_seti() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Seti(Immediate(5)),
        };

        let mut regs: Registers = Registers::new([0, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([0, 1, 0, 5]));
    }

    #[test]
    fn it_exectues_gtir() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Gtir(Immediate(0), Register(1)),
        };

        let mut regs: Registers = Registers::new([0, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([0, 1, 0, 0]));
    }

    #[test]
    fn it_exectues_gtir_when_condition_true() {
        let op = Operation {
            output_register: Register(3),
            kind: OpKind::Gtir(Immediate(5), Register(1)),
        };

        let mut regs: Registers = Registers::new([0, 1, 0, 0]);
        op.execute(&mut regs).unwrap();

        assert_eq!(regs, Registers::new([0, 1, 0, 1]));
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

    #[test]
    fn it_parses_samples() {
        let str = String::from(
            "
Before: [1, 1, 0, 0]
2 1 0 2
After:  [1, 1, 1, 0]

Before: [3, 3, 2, 1]
8 3 2 2
After:  [3, 3, 1, 1]


6 0 0 3
9 3 2 3    
        ",
        );

        assert_eq!(
            str.parse::<Samples>().unwrap(),
            Samples(vec![
                Sample {
                    before: Registers([1, 1, 0, 0]),
                    instruction: Instruction {
                        op_code: 2,
                        input_a: 1,
                        input_b: 0,
                        output: Register(2)
                    },
                    after: Registers([1, 1, 1, 0]),
                },
                Sample {
                    before: Registers([3, 3, 2, 1]),
                    instruction: Instruction {
                        op_code: 8,
                        input_a: 3,
                        input_b: 2,
                        output: Register(2)
                    },
                    after: Registers([3, 3, 1, 1]),
                }
            ])
        )
    }

    #[test]
    fn it_parses_program() {
        let str = String::from(
            "
6 0 0 3
9 3 2 3    
        ",
        );

        assert_eq!(
            str.parse::<Program>().unwrap(),
            Program {
                instructions: vec![
                    Instruction {
                        op_code: 6,
                        input_a: 0,
                        input_b: 0,
                        output: Register(3)
                    },
                    Instruction {
                        op_code: 9,
                        input_a: 3,
                        input_b: 2,
                        output: Register(3)
                    }
                ]
            }
        )
    }
}
