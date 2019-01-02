const { parse } = require("./day17");
const test = require("ava");

test("parse", t => {
  const input = `x=495, y=2..4
y=7, x=499..501
x=10..12, y=3..4`;

  t.deepEqual(parse(input), [
    {
      x: 495,
      y: 2
    },
    {
      x: 495,
      y: 3
    },
    {
      x: 495,
      y: 4
    },
    {
      x: 499,
      y: 7
    },
    {
      x: 500,
      y: 7
    },
    {
      x: 501,
      y: 7
    },
    {
      x: 10,
      y: 3
    },
    {
      x: 10,
      y: 4
    },
    {
      x: 11,
      y: 3
    },
    {
      x: 11,
      y: 4
    },
    {
      x: 12,
      y: 3
    },
    {
      x: 12,
      y: 4
    }
  ]);
});
