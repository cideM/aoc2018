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
  // console.log(zeroIndex)
  const left = line
    .slice(0, zeroIndex)
    .split("")
    .reverse();
  const right = line.slice(zeroIndex).split("");
  // console.log("l",left.join(""))
  // console.log("r",right.join(""))

  const leftCount = left
    .map((char, index) => (char === "#" ? index + 1 : 0))
    .reduce((sum, x) => x + sum, 0);
  const rightCount = right
    .map((char, index) => (char === "#" ? index : 0))
    .reduce((sum, x) => x + sum, 0);

  return rightCount - leftCount;
};

const evoluationIter = function*(rules, initial) {
  const padded = pad(30, initial);
  const zero = padded.indexOf("#");

  let last = padded;

  for (let i = 1; i < Infinity; i++) {
    // console.log(last)
    last = evolve(rules, last);
    yield { state: last, count: count(last, zero), age: i };
  }

  // console.log("last", last)
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

    let seen = [];
    let startsRepeatingAfter
    let repeatsAfter

    for (const generation of garden2) {
      startsRepeatingAfter = generation.state === initial;

      if (generation.age > 0 && generation.age % 50000 === 0) console.log(generation.age, generation.count)
      if (generation.age === 1000000) break

      if (startsRepeatingAfter !== undefined) {
        // break;
      }

      seen.push(generation);
    }


    const projectionAge = 50000000000

    console.log(startsRepeatingAfter)

    const repeatSequence = seen.slice(startsRepeatingAfter.age - 1)

    const missing = projectionAge - (startsRepeatingAfter.age - 1)
    const indexInRepeat = missing % repeatSequence.length
    console.log(repeatSequence.map(seq => seq.count))

    const plantsAtProjectionAge = repeatSequence[indexInRepeat - 1].count

    return `after 20: ${after20}, repeats after: ${JSON.stringify(
      startsRepeatingAfter
    )} has ${JSON.stringify(plantsAtProjectionAge)} at projected final`;
  }
};


// 1 2 3 4 5 6 7 8 9 10
// 0 1 2 3 4 5 6 7 8 9 10
// 1 2 3 4 5 6 4 5 6 4 5

// repeatsAfter index 3
// repeat length 3
// repeat sequence 4 5 6
// missing 7
// index in repat 7 % 3 = 1 - 1 -> 4

