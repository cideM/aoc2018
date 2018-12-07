const { parseLine, parseData, runDeps, runDepsWithWorkers } = require("./day7");
const test = require("ava");

test("parseLine", t => {
  const line = "Step A must be finished before step B can begin.";

  t.deepEqual(parseLine(line), { required: "A", step: "B" });
});

test("parseData", t => {
  const data = `Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.`;

  t.deepEqual(parseData(data), {
    A: ["C"],
    B: ["A"],
    D: ["A"],
    E: ["B", "D", "F"],
    F: ["C"],
    C: []
  });
});

test("runDeps", t => {
  const deps = {
    A: ["C"],
    B: ["A"],
    D: ["A"],
    E: ["B", "D", "F"],
    F: ["C"],
    C: []
  };

  t.deepEqual(runDeps(deps), ["C", "A", "B", "D", "F", "E"]);
});

test("runDepsWithWorkers", t => {
  const deps = {
    A: ["C"],
    B: ["A"],
    D: ["A"],
    E: ["B", "D", "F"],
    F: ["C"],
    C: []
  };

  t.deepEqual(runDepsWithWorkers(deps, 2), 258);
});
