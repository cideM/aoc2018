use clap::{App, Arg, SubCommand};

fn main() {
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
                .conflicts_with("stdin")
        )
        .arg(
            Arg::with_name("stdin")
                .short("s")
                .long("stdin")
                .help("Read from stdin")
                .conflicts_with("file")
        )
        .arg(
            Arg::with_name("day")
                .short("d")
                .takes_value(true)
                .help("Which Advent of Code day to run"),
        )
        .get_matches();
}
