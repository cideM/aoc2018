const {
  getNextState,
  TILE_STATES,
  evolve,
  evolveMin,
  printGrid
} = require("./day18");
const test = require("ava");

const makeTestGrid = str =>
  str
    .trim()
    .split("\n")
    .map(x => x.trim().split(""));

test("getNextState: open -> open", t => {
  const data = `
    .#.
    ...
    .|.
  `;

  const grid = makeTestGrid(data);

  t.deepEqual(getNextState(grid, 1, 1), TILE_STATES.OPEN);
});

test("getNextState: open -> tree", t => {
  const data = `
    .#.
    |.|
    .|.
  `;

  const grid = makeTestGrid(data);

  t.deepEqual(getNextState(grid, 1, 1), TILE_STATES.TREE);
});

test("getNextState: tree -> tree", t => {
  const data = `
    .#.
    |||
    .|.
  `;

  const grid = makeTestGrid(data);

  t.deepEqual(getNextState(grid, 1, 1), TILE_STATES.TREE);
});

test("getNextState: tree -> yard", t => {
  const data = `
    .#.
    ||#
    .##
  `;

  const grid = makeTestGrid(data);

  t.deepEqual(getNextState(grid, 1, 1), TILE_STATES.YARD);
});

test("getNextState: yard -> yard", t => {
  const data = `
    .#.
    |#.
    .||
  `;

  const grid = makeTestGrid(data);

  t.deepEqual(getNextState(grid, 1, 1), TILE_STATES.YARD);
});

test("getNextState: yard -> open", t => {
  const data = `
    .#.
    .#.
    ...
  `;

  const grid = makeTestGrid(data);

  t.deepEqual(getNextState(grid, 1, 1), TILE_STATES.OPEN);
});

test("getNextState: border tile; yard -> open", t => {
  const data = `
    .#.
    ...
  `;

  const grid = makeTestGrid(data);

  t.deepEqual(getNextState(grid, 1, 0), TILE_STATES.OPEN);
});

test("evolve", t => {
  const data = `
.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.
  `;

  const expected = `
.......##.
......|###
.|..|...#.
..|#||...#
..##||.|#|
...#||||..
||...|||..
|||||.||.|
||||||||||
....||..|.
  `.trim();

  const grid = makeTestGrid(data);
  const step1 = evolve(grid);
  t.deepEqual(printGrid(step1), printGrid(makeTestGrid(expected)));

  const step2 = evolve(step1);
  const expected2 = `
.......#..
......|#..
.|.|||....
..##|||..#
..###|||#|
...#|||||.
|||||||||.
||||||||||
||||||||||
.|||||||||
  `.trim();

  t.deepEqual(printGrid(step2), printGrid(makeTestGrid(expected2)));
});

test("evolveMin", t => {
  const data = `
.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.
  `;

  const expected = `
.||##.....
||###.....
||##......
|##.....##
|##.....##
|##....##|
||##.####|
||#####|||
||||#|||||
||||||||||
    `.trim();

  const grid = makeTestGrid(data);
  const result = evolveMin(grid, 10);
  t.deepEqual(printGrid(result), printGrid(makeTestGrid(expected)));
});
