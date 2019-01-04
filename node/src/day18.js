const _ = require("lodash");

const parse = input => {
  return input
    .trim()
    .split("\n")
    .map(row => row.trim().split(""));
};

const TILE_STATES = {
  OPEN: ".",
  TREE: "|",
  YARD: "#"
};

const getDimensions = grid => {
  const width = grid[0].length - 1;
  const height = grid.length - 1;

  return {
    width,
    height
  };
};

const getCell = _.curry((grid, x, y) => {
  const { width, height } = getDimensions(grid);

  if (x < 0 || x > width || y < 0 || y > height) return null;

  return grid[y][x];
});

const getNextState = (grid, x, y) => {
  const getCellFromGrid = getCell(grid);

  const topLeft = getCellFromGrid(x - 1, y - 1);
  const top = getCellFromGrid(x, y - 1);
  const topRight = getCellFromGrid(x + 1, y - 1);
  const left = getCellFromGrid(x - 1, y);
  const right = getCellFromGrid(x + 1, y);
  const bottomLeft = getCellFromGrid(x - 1, y + 1);
  const bottom = getCellFromGrid(x, y + 1);
  const bottomRight = getCellFromGrid(x + 1, y + 1);
  const adjacent = [
    topLeft,
    top,
    topRight,
    left,
    right,
    bottomLeft,
    bottom,
    bottomRight
  ].filter(Boolean);

  const current = grid[y][x];

  let nextState;

  switch (current) {
    case TILE_STATES.OPEN: {
      nextState =
        adjacent.filter(x => x === TILE_STATES.TREE).length >= 3
          ? TILE_STATES.TREE
          : current;
      break;
    }
    case TILE_STATES.TREE: {
      nextState =
        adjacent.filter(x => x === TILE_STATES.YARD).length >= 3
          ? TILE_STATES.YARD
          : current;
      break;
    }
    case TILE_STATES.YARD: {
      nextState =
        adjacent.find(x => x === TILE_STATES.YARD) &&
        adjacent.find(x => x === TILE_STATES.TREE)
          ? TILE_STATES.YARD
          : TILE_STATES.OPEN;
      break;
    }
    default: {
      throw new Error(`Unknown tile. Coords: ${x} ${y}. Content: ${current}`);
    }
  }

  return nextState;
};

const evolve = grid => {
  const copy = _.cloneDeep(grid);

  return grid.map((row, y) =>
    row.map((cell, x) => {
      const nextState = getNextState(copy, x, y);
      return nextState;
    })
  );
};

const printGrid = grid => {
  return grid.map(row => row.join("")).join("\n");
};

const evolveMin = (initialGrid, min) => {
  let grid = initialGrid;

  for (let a = 0; a < min; a++) {
    grid = evolve(grid);
  }

  return grid;
};

const getResources = grid => {
  let trees = 0;
  let yards = 0;

  for (const row of grid) {
    for (const cell of row) {
      if (cell === TILE_STATES.YARD) {
        yards++;
        continue;
      }

      if (cell === TILE_STATES.TREE) {
        trees++;
        continue;
      }
    }
  }

  return { trees, yards };
};

const findPattern = initialGrid => {
  const past = [];
  let grid = initialGrid;
  let step = 0;

  while (true) {
    const printed = printGrid(grid);
    past.push([printed, step]);
    grid = evolve(grid);
    step++;
    const printedNext = printGrid(grid);
    // console.log("============");
    // console.log(printedNext);

    // This is now the updated  grid
    const found = past.find(x => x[0] === printedNext);
    if (found) {
      return {
        found,
        afterSteps: step
      };
    }
  }
};

module.exports = {
  id: "18",
  getNextState,
  evolve,
  printGrid,
  evolveMin,
  TILE_STATES,
  run: input => {
    const grid = parse(input);

    const after10 = evolveMin(grid, 10);

    const { trees, yards } = getResources(after10);

    const {
      found: [_, startsRepeatingAt],
      afterSteps: stopsRepeatingAt
    } = findPattern(grid);

    const finalSteps = 1000000000;

    // We're repeating a sequence of stops until a certain step count. We want
    // to know, at that final step count, where in the sequence we are. It's
    // like a wrapping array in that we can use % to always get a valid step
    // count from the sequence. The only quirk is since array index is 0 based
    // but the steps start 1, that in case we're at position 0 in the sequence,
    // that equals the *last* step in the sequence, not the first.
    const stepsEqualToFinal =
      (finalSteps - startsRepeatingAt) % (stopsRepeatingAt - startsRepeatingAt);

    // Above mentioned correction
    const corrected =
      stepsEqualToFinal === 0 ? stopsRepeatingAt : stepsEqualToFinal;

    // We then run the simulation for (steps until it starts repeating) + (step
    // in repeat sequence needed to simulate desired step count)
    const { trees: trees2, yards: yards2 } = getResources(
      evolveMin(grid, startsRepeatingAt + corrected)
    );

    return `Resources after 10 minutes:
      - ${trees} trees
      - ${yards} yards

      Total: ${trees * yards}

      Resources after ${finalSteps} minutes:
      - ${trees2} trees
      - ${yards2} yards

      Total: ${trees2 * yards2}
    `;
  }
};
