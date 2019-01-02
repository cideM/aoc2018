const combineNumberAndRange = (number, range) => {
  const out = [];

  for (let a = range[0]; a <= range[1]; a++) {
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

const makeKey = (x, y) => `${x}.${y}`;

const coordsFromKey = key => {
  const [x, y] = key.split(".");

  return {
    x: Number(x),
    y: Number(y)
  };
};

const flow = ({ grid, origin, flowing, settled }) => {
  // Check if origin can produce water
  const belowOrigin = makeKey(origin.x, origin.y);

  if (!flowing[belowOrigin] && !settled[belowOrigin]) {
    return {
      flowing: {
        ...flowing,
        [belowOrigin]: true
      }
    };
  }

  Object.keys(flowing).forEach(currentKey => {
    const { x, y } = coordsFromKey(currentKey);

    const down = { x, y: y + 1 };
    const left = { x: x - 1, y };
    const right = { x: x + 1, y };

    [down, left, right].forEach(coords => {
      const newKey = makeKey(coords);

      if (!flowing[newKey] && !settled[newKey] && grid[y][x] === ".") {
        const newFlowing = {
          ...flowing,
          [newKey]: true
        };

        delete newFlowing[currentKey];

        return {
          flowing: newFlowing
        };
      }
      // settled
      // default
    });
  });
};

const makeGrid = (points, origin) => {
  const asObj = {};
  const pointsWithOrigin = points.concat(origin);

  for (const value of pointsWithOrigin) {
    asObj[makeKey(value.x, value.y)] = true;
  }

  const maxX = Math.max(...pointsWithOrigin.map(({ x }) => x));
  const maxY = Math.max(...pointsWithOrigin.map(({ y }) => y));

  const grid = [];

  for (let y = 0; y <= maxY; y++) {
    if (!grid[y]) grid[y] = [];

    for (let x = 0; x <= maxX; x++) {
      grid[y][x] = asObj[makeKey(x, y)] ? "#" : ".";
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
    const grid = makeGrid(parsed, { x: 500, y: 0 });
    return "foo";
  }
};
