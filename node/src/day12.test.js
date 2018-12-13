const { parse, evolve, pad, count } = require("./day12");
const test = require("ava");

test("parse", t => {
  const data = `initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #`;

  t.deepEqual(parse(data), {
    initial: "#..#.#..##......###...###",
    rules: [
      "...##",
      "..#..",
      ".#...",
      ".#.#.",
      ".#.##",
      ".##..",
      ".####",
      "#.#.#",
      "#.###",
      "##.#.",
      "##.##",
      "###..",
      "###.#",
      "####."
    ]
  });
});

test("evolve", t => {
  const testLine = "...#..#.#..##......###...###...";
  const rules = [
    "...##",
    "..#..",
    ".#...",
    ".#.#.",
    ".#.##",
    ".##..",
    ".####",
    "#.#.#",
    "#.###",
    "##.#.",
    "##.##",
    "###..",
    "###.#",
    "####."
  ];

  const expected = "...#...#....#.....#..#..#..#...";

  t.deepEqual(evolve(rules, testLine), expected);
});

test("count", t => {
  const testLine = ".#....##....#####...#######....#.#..##."
  const zeroIndex = 3

  t.deepEqual(count(testLine, zeroIndex), 325);
});
