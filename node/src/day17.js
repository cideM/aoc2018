const _ = require("lodash");

const printGrid = grid => {
  grid.forEach(row => console.log(row.join("")));
};

const FREE_TILE = " ";

const combineNumberAndRange = (number, range) => {
  const out = [];

  for (let a = Number(range[0]); a <= Number(range[1]); a++) {
    out.push([number, a]);
  }

  return out;
};

const combineRanges = (range1, range2) => {
  const [min1, max1] = range1;
  const [min2, max2] = range2;
  const out = [];

  for (let a = min1; a <= max1; a++) {
    for (let b = min2; b <= max2; b++) {
      out.push([a, b]);
    }
  }

  return out;
};

const parseKeyValue = text => {
  const [key, value] = text.trim().split("=");

  if (value.includes("..")) {
    const [min, max] = value.split("..");

    return {
      coord: key,
      range: [min, max]
    };
  }

  return {
    coord: key,
    number: value
  };
};

const parse = text => {
  const result = [];

  for (const line of text.split("\n")) {
    const [first, second] = line.split(",");
    if (!first || !second) break;

    const pair1 = parseKeyValue(first);
    const pair2 = parseKeyValue(second);

    const x = pair1.coord === "x" ? pair1 : pair2;
    const y = pair1.coord === "y" ? pair1 : pair2;

    if (x.number && y.number) {
      result.push({ x: Number(x.number), y: Number(y.number) });
    } else if (x.number && y.range) {
      combineNumberAndRange(x.number, y.range).forEach(([x, y]) =>
        result.push({ x: Number(x), y: Number(y) })
      );
    } else if (x.range && y.number) {
      combineNumberAndRange(y.number, x.range).forEach(([y, x]) =>
        result.push({ x: Number(x), y: Number(y) })
      );
    } else if (x.range && y.range) {
      combineRanges(x.range, y.range).forEach(([x, y]) =>
        result.push({ x: Number(x), y: Number(y) })
      );
    }
  }

  return result;
};

const makeGrid2 = (width, height, defaultValue = FREE_TILE) => {
  const grid = [];

  for (let y = 0; y <= height; y++) {
    if (!grid[y]) grid[y] = [];

    for (let x = 0; x <= width; x++) {
      grid[y][x] = defaultValue;
    }
  }

  return grid;
};

const fillLeft = (grid, pos) => {
  let x = pos.x;
  let y = pos.y;

  // As long as we're on top of solid ground or settled water, add more settled
  // water
  while (grid[y][x] !== "#") {
    grid[y][x] = "~";
    x--;
  }
};

const fillRight = (grid, pos) => {
  let x = pos.x;
  let y = pos.y;

  // As long as we're on top of solid ground or settled water, add more settled
  // water
  while (grid[y][x] !== "#") {
    grid[y][x] = "~";
    x++;
  }
};

const isBasin = (grid, pos) => {
  let xl = pos.x - 1;
  let xr = pos.x + 1;

  let hasWallLeft;
  let hasWallRight;

  while (true) {
    if (grid[pos.y][xl] === FREE_TILE) {
      hasWallLeft = false;
      break;
    }

    if (grid[pos.y][xl] === "#") {
      hasWallLeft = true;
      break;
    }

    xl--;
  }

  while (true) {
    if (grid[pos.y][xr] === FREE_TILE) {
      hasWallRight = false;
      break;
    }

    if (grid[pos.y][xr] === "#") {
      hasWallRight = true;
      break;
    }

    xr++;
  }

  return hasWallLeft && hasWallRight;
};

const fill = (grid, position) => {
  const { x, y } = position;

  if (y >= grid.length - 1) {
    grid[y][x] = "|";
    return;
  }

  // Down
  if (grid[y + 1][x] === FREE_TILE) {
    grid[y + 1][x] = "|";
    // Recurse so that all downstream tiles are in their final state before
    // moving on. If the tile below this one is a dead end, that row will be
    // filled by the time we move on and we can then flow left and right.
    fill(grid, { x, y: y + 1 });
  }

  // Left
  if ("#~".includes(grid[y + 1][x]) && grid[y][x - 1] === FREE_TILE) {
    grid[y][x - 1] = "|";
    fill(grid, { x: x - 1, y });
  }

  // Right
  if ("#~".includes(grid[y + 1][x]) && grid[y][x + 1] === FREE_TILE) {
    grid[y][x + 1] = "|";
    fill(grid, { x: x + 1, y });
  }

  // If we're in a basin (can't flow down, wallsleft and right), fill the
  // current row in the basin. This function returns, and the parent function
  // continues.
  if ("#~".includes(grid[y + 1][x]) && isBasin(grid, position)) {
    fillLeft(grid, position);
    fillRight(grid, position);
    grid[y][x] = "~";
  }
};

module.exports = {
  id: "17",
  parse,
  run: input => {
    const origin = { x: 500, y: 0 };

    const parsed = parse(input);
    const withOrigin = parsed.concat(origin);

    const minY = Math.min(...parsed.map(({ y }) => y));
    // const minX = Math.min(...parsed.map(({ x }) => x));
    const width = Math.max(...withOrigin.map(({ x }) => x)) + 5;
    const height = Math.max(...withOrigin.map(({ y }) => y));

    const grid = makeGrid2(width, height);

    // Add walls
    parsed.forEach(({ x, y }) => {
      grid[y][x] = "#";
    });

    fill(grid, origin);

    // const gridCorrected = grid.map(row => row.slice(minX));

    // printGrid(gridCorrected);

    const waterTiles = _.sum(
      _.flatMap(grid, (row, y) =>
        row.map(cell => (y >= minY && "|+v><".includes(cell) ? 1 : 0))
      )
    );

    const settled = _.sum(
      _.flatMap(grid, (row, y) =>
        row.map(cell => (y >= minY && "~".includes(cell) ? 1 : 0))
      )
    );

    return `Water tiles: ${waterTiles}; Settled: ${settled}; total: ${waterTiles +
      settled}`;
  }
};
