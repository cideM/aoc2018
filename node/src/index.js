const { Command, flags } = require("@oclif/command");
const emoji = require("node-emoji");
const days = require("./days");
const fs = require("fs");

class AoC extends Command {
  async run() {
    const { flags } = this.parse(AoC);

    const day = days.find(d => d.id === flags.day);
    const data = flags.file
      ? fs.readFileSync(flags.file, "utf8")
      : fs.readFileSync(0, "utf8");

    if (day) {
      this.log(`
      Merry Wintersday! ${emoji.get("christmas_tree")}

      ${day.run(data)}`);
    } else {
      this.error(`Couldn't find day ${flags.day}! ${emoji.get("cry")}`, {
        exit: 0
      });
    }
  }
}

AoC.flags = {
  day: flags.string({
    required: true,
    char: "d",
    description: "Which day to run"
  }),
  stdin: flags.boolean({
    char: "s",
    description: "Whether to read data for exercise from stdin",
    exclusive: ["file"],
  }),
  file: flags.string({
    char: "f",
    description: "File from which to read data for exercise."
  }),
  version: flags.version({ char: "v" }),
  help: flags.help({ char: "h" })
};

AoC.description = `Advent of Code 2018! ${emoji.get("santa")}
Run each day with data from either stdin or a file

$ aoc -day 1 --stdin "Your data"
$ aoc -day 1 --file ./path/to/data

https://adventofcode.com/
`;

module.exports = AoC;
