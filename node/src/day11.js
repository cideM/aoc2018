const gridSize = 300;

const powerLevel = (serialNum, { x, y }) => {
  const rackID = x + 10;

  return (Math.floor(((rackID * y + serialNum) * rackID) / 100) % 10) - 5;
};

const makeGrid = (serialNum, size) => {
  let grid = [];

  for (let y = 0; y < size; y++) {
    grid[y] = [];

    for (let x = 0; x < size; x++) {
      const cell = { x: x + 1, y: y + 1 };
      cell.power = powerLevel(serialNum, cell);

      grid[y][x] = cell;
    }
  }

  return grid;
};

const getMissingCellsInSquare = (grid, cell, squareSize) => {
  const { x, y } = cell;
  const xBase = x - 1;
  const yBase = y - 1; // x and y for cell are always 1 higher than index in matrix

  // Return empty array if square is not fully in grid
  if (!grid[xBase + squareSize] || !grid[xBase][yBase + squareSize]) return [];

  let cells = [];

  // Skip the last otherwise we're pushing that twice
  for (let _y = yBase; _y < yBase + squareSize; _y++) {
    cells.push(grid[_y][xBase + squareSize]);
  }

  for (let _x = xBase; _x <= xBase + squareSize; _x++) {
    cells.push(grid[yBase + squareSize][_x]);
  }

  return cells;
};

const run = (serialNum, size, maxSquares) => {
  // Only cache last iteration
  let cache = {};
  const grid = makeGrid(serialNum, size);

  let originBestSquare = null;
  let fuelBestSquare = null;
  let bestSquareSize = null;

  for (let s = 0; s < maxSquares; s++) {
    process.stdout.clearLine();
    process.stdout.cursorTo(0);
    process.stdout.write(`${Math.round((s / maxSquares) * 100)}%`);

    for (const row of grid) {
      for (const cell of row) {
        const { power, x, y } = cell;

        // exit early if square would be larger than grid
        if (x + s > size || y + s > size) continue;

        const cacheKey = JSON.stringify(cell);

        let newFuel = 0;

        if (cache[cacheKey]) {
          newFuel += cache[cacheKey];
        } else {
          newFuel += power;
        }

        // Skip self
        const missing = s > 0 ? getMissingCellsInSquare(grid, cell, s) : [];
        const fuelInMissing = missing.reduce((xs, x) => xs + x.power, 0);

        const finalFinal = newFuel + fuelInMissing;

        cache[cacheKey] = finalFinal;

        if (!fuelBestSquare || finalFinal > fuelBestSquare) {
          originBestSquare = cell;
          fuelBestSquare = finalFinal;
          bestSquareSize = s;
        }
      }
    }
  }

  return {
    originBestSquare,
    fuelBestSquare,
    bestSquareSize: bestSquareSize + 1 // squareSize of 2 in this script means x, x + 1, x + 2
  };
};

module.exports = {
  id: "11",
  run: data => {
    const serialNum = Number(data);

    console.log(data)
    return JSON.stringify(run(serialNum, 300, 300));
  }
};
