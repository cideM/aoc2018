const _ = require("lodash");

const makeKey = (x, y) => `${x}.${y}`;

const printGrid = (flowing, settled, options = {}) => {
  const { grid, origin } = options;
  const withSymbols = grid.map((row, y) =>
    row.map((tile, x) => {
      const key = makeKey(x, y);

      if (x === origin.x && y === origin.y) return "+";
      if (flowing.has(key)) return "|";
      if (settled.has(key)) return "~";
      return tile;
    })
  );

  withSymbols.forEach(row => console.log(row.join("")));
};

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

const coordsFromKey = key => {
  const [x, y] = key.split(".");

  return {
    x: Number(x),
    y: Number(y)
  };
};

const waterCanFlow = (water, options = {}) => {
  const { grid, flowing, settled } = options;
  const { x, y } = water;

  const down = { x, y: y + 1 };
  const left = { x: x - 1, y };
  const right = { x: x + 1, y };

  const coordsOfFreeTile = [down, left, right].find(coords => {
    const { x: _x, y: _y } = coords;
    const newKey = makeKey(_x, _y);

    return (
      !flowing.has(newKey) &&
      !settled.has(newKey) &&
      grid[_y] &&
      grid[_y][_x] === "."
    );
  });

  return coordsOfFreeTile;
};

const canSettle = (water, options = {}) => {
  const { settled, grid, flowing } = options;
  const { x, y } = water;

  const left = makeKey(x - 1, y);
  const right = makeKey(x + 1, y);

  return (
    (!flowing.has(left) && !flowing.has(right) && settled.has(left)) ||
    settled.has(right) ||
    grid[water.y][x - 1] === "#" ||
    grid[water.y][x + 1] === "#"
  );
};

const flow = (flowing, settled, options = {}) => {
  const { grid, origin } = options;

  // Check if origin can produce water
  const belowOrigin = makeKey(origin.x, origin.y + 1);

  if (!flowing.has(belowOrigin) && !settled.has(belowOrigin)) {
    flowing.add(belowOrigin);
    return;
  }

  for (const currentKey of flowing) {
    const { x, y } = coordsFromKey(currentKey);

    const down = { x, y: y + 1 };

    const hasReachedEnd = down.y === grid.length;

    // Be like water
    // if (hasReachedEnd) {
    //   flowing.delete(currentKey);
    // }

    // Has some neighbor that's still flowing, so expect this tile of water to
    // eventually move once that neighbor has moved as well
    if (flowing.has(makeKey(down.x, down.y))) continue;

    const freeTileCoords = waterCanFlow(coordsFromKey(currentKey), {
      grid,
      flowing,
      settled
    });

    if (!hasReachedEnd && freeTileCoords) {
      // Move water tile to free tile
      flowing.add(makeKey(freeTileCoords.x, freeTileCoords.y));
      flowing.delete(currentKey);

      return;
    }

    // No neighbor in flow, no free tile, so this water is now settled
    if (
      !hasReachedEnd &&
      canSettle(coordsFromKey(currentKey), { settled, grid, flowing })
    ) {
      flowing.delete(currentKey);
      settled.add(currentKey);
      return;
    }
  }
};

const makeGrid = (points, origin) => {
  const pointsWithOrigin = points.concat(origin);
  const maxX = Math.max(...pointsWithOrigin.map(({ x }) => x));
  const maxY = Math.max(...pointsWithOrigin.map(({ y }) => y));

  const asSet = new Set(pointsWithOrigin.map(({ x, y }) => makeKey(x, y)));

  const grid = [];

  for (let y = 0; y <= maxY; y++) {
    if (!grid[y]) grid[y] = [];

    for (let x = 0; x <= maxX; x++) {
      grid[y][x] = asSet.has(makeKey(x, y)) ? "#" : ".";
    }
  }

  grid[origin.y][origin.x] = "+";

  return grid;
};

module.exports = {
  id: "17",
  parse,
  run: input => {
    const parsed = parse(input);
    const origin = { x: 500, y: 0 };
    const grid = makeGrid(parsed, origin);
    const options = { grid, origin };

    let flowing = new Set();
    let settled = new Set();
    let lastFlowing = flowing;

    let i = 0;

    do {
      // lastFlowing = new Set([...flowing]);

      flow(
        flowing,
        settled,
        options
      );
      i++;
      console.log(flowing.size + settled.size);
    } while (i < 5000000);

    const waterTiles = flowing.size + settled.size;

    printGrid(flowing, settled, options);

    return `Water tiles: ${waterTiles}`;
  }
};
