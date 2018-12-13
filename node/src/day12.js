// This is ugly but it works

const parse = data => {
  const lines = data.split("\n");

  const initial = lines[0].split(" ")[2];

  const rules = lines
    .slice(2)
    .filter(line => line[line.length - 1] === "#")
    .map(line => line.split(" ")[0]);

  return { rules, initial };
};

const pad = (len, str) => {
  const padding = new Array(len).fill(".").join("");
  const padded = padding + str + padding;

  return padded;
};

const evolve = (rules, line) => {
  const newLine = [];

  for (let i = 0; i < line.length; i++) {
    const slice = line.slice(i - 2, i + 3);

    newLine.push(rules.some(rule => slice === rule) ? "#" : ".");
  }

  return newLine.join("");
};

const count = (line, zeroIndex) => {
  const left = line
    .slice(0, zeroIndex)
    .split("")
    .reverse();
  const right = line.slice(zeroIndex).split("");

  const leftCount = left
    .map((char, index) => (char === "#" ? index + 1 : 0))
    .reduce((sum, x) => x + sum, 0);
  const rightCount = right
    .map((char, index) => (char === "#" ? index : 0))
    .reduce((sum, x) => x + sum, 0);

  return rightCount - leftCount;
};

const paddingBuffer = 4;

const evoluationIter = function*(rules, initial) {
  const padded = pad(20, initial);

  let zero = padded.indexOf("#");

  let last = padded;

  for (let i = 1; i < Infinity; i++) {
    const result = evolve(rules, last);

    const spaceLeft = result.indexOf("#");
    const spaceRight = result.length - result.lastIndexOf("#");

    if (spaceLeft < paddingBuffer || spaceRight < paddingBuffer) {
      last = pad(paddingBuffer, result);
      zero += paddingBuffer;
    } else {
      last = result;
    }

    yield { state: last, count: count(last, zero), age: i };
  }
};

module.exports = {
  parse,
  evolve,
  pad,
  count,
  id: "12",
  run: data => {
    const { rules, initial } = parse(data);

    const garden = evoluationIter(rules, initial);

    let after20;

    for (const generation of garden) {
      if (generation.age === 20) {
        after20 = generation.count;
        break;
      }
    }

    const garden2 = evoluationIter(rules, initial);

    let plantsAddedPerYear = [];
    let addedLast = 0;
    let lastPlantCount = 0;
    let lastAverage = 0;

    let plantsUntilSteady = 0
    let generationWhenSteady = 0
    let steadyAverage = 0

    // Should try to create a declarative, functional version of this. We keep track of how many plants we add in an
    // arbitrary time period. Once that amount seems to remain steady, we take the plants we have until then,
    // extrapolate how many plants we'll add until the given future generation (by just multiplying generations left
    // with our average). The sum of those two values is the number of plants at that future point in time.

    for (const generation of garden2) {
      addedLast = generation.count - lastPlantCount;
      lastPlantCount += addedLast;
      plantsAddedPerYear.push(addedLast);

      if (generation.age > 0 && generation.age % 500 === 0) {
        const averageAddedLast500 = Math.round(
          plantsAddedPerYear.reduce((xs, x) => x + xs, 0) / plantsAddedPerYear.length
        );

        if (lastAverage !== averageAddedLast500) lastAverage = averageAddedLast500
        else {
          plantsUntilSteady = generation.count
          steadyAverage = averageAddedLast500
          generationWhenSteady = generation.age
          break;
        }
      }
    }

    const generationsLeft = 50000000000 - generationWhenSteady
    const plantsToBeAdded = generationsLeft * steadyAverage

    const plantsAtEndpoint = plantsUntilSteady + plantsToBeAdded

    return JSON.stringify({ after20, plantsAtEndpoint })
  }
};
