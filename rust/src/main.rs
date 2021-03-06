use aoc_lib::{day14, day16, day19, day4, day5, day6, day8};
use clap::{App, Arg};
use failure::Error;
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::prelude::*;
mod types;
use self::types::DayProg;

fn main() -> Result<(), Error> {
    let matches = App::new("aoc")
        .version("1.0")
        .author("Florian B. <yuuki@protonmail.com>")
        .about("Wintersday spirit!")
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .value_name("FILE")
                .help("File from which to read data for AoC exercise")
                .takes_value(true)
                .conflicts_with("stdin"),
        )
        .arg(
            Arg::with_name("stdin")
                .short("s")
                .long("stdin")
                .help("Read from stdin")
                .conflicts_with("file"),
        )
        .arg(
            Arg::with_name("day")
                .short("d")
                .takes_value(true)
                .help("Which Advent of Code day to run"),
        )
        .get_matches();

    let mut input = String::new();

    if matches.is_present("stdin") {
        let stdin = io::stdin();
        let mut handle = stdin.lock();

        handle.read_to_string(&mut input)?;
    } else {
        let fp = matches.value_of("file").unwrap();
        let mut f = File::open(fp)?;

        f.read_to_string(&mut input)?;
    }

    let day4_prog = DayProg {
        name: "day4",
        run: day4::run,
    };

    let day5_prog = DayProg {
        name: "day5",
        run: day5::run,
    };

    let day6_prog = DayProg {
        name: "day6",
        run: day6::run,
    };

    let day8_prog = DayProg {
        name: "day8",
        run: day8::run,
    };

    let day14_prog = DayProg {
        name: "day14",
        run: day14::run,
    };

    let day16_prog = DayProg {
        name: "day16",
        run: day16::run,
    };

    let day19_prog = DayProg {
        name: "day19",
        run: day19::run,
    };

    let mut day_progs = HashMap::new();

    day_progs.insert("4", day4_prog);
    day_progs.insert("5", day5_prog);
    day_progs.insert("6", day6_prog);
    day_progs.insert("8", day8_prog);
    day_progs.insert("14", day14_prog);
    day_progs.insert("16", day16_prog);
    day_progs.insert("19", day19_prog);

    if let Some(d) = matches.value_of("day") {
        let prog = day_progs.get(d).expect("No program for that day! :(");
        println!("Solution: {}", (prog.run)(&input)?);
    };

    Ok(())
}
