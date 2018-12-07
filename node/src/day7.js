const parseLine = l => {
  const xs = l.split(" ");

  const required = xs.slice(1)[0];
  const step = xs.slice(7)[0];

  return {
    required,
    step
  };
};

const parseData = lines =>
  lines.split("\n").reduce((acc, l) => {
    const { required, step } = parseLine(l);

    return {
      ...acc,
      [required]: acc[required] || [],
      [step]: (acc[step] || []).concat(required)
    };
  }, {});

const runDeps = deps => {
  const go = (pending = Object.keys(deps), completed = []) => {
    if (pending.length === 0) return completed;

    // For each pending step, check if all its requirements are fullfilled. Sort
    // them alphabetically.
    const nextSteps = pending
      .filter(p => deps[p].every(r => completed.includes(r)))
      .sort();
    const next = nextSteps[0];

    return go(pending.filter(p => p !== next), completed.concat(next));
  };

  return go();
};

const workCosts = [
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "j",
  "k",
  "l",
  "m",
  "n",
  "o",
  "p",
  "q",
  "r",
  "s",
  "t",
  "u",
  "v",
  "w",
  "x",
  "y",
  "z"
];

const getCost = char => 60 + workCosts.indexOf(char.toLowerCase()) + 1;

const stepsStillPending = (steps, completedSteps, runningTasks) =>
  steps.filter(
    step =>
      !completedSteps.includes(step) && runningTasks.every(t => t.step !== step)
  );

const runDepsWithWorkers = (deps, workerCount) => {
  const go = state => {
    let currentCompletedSteps = state.completed;
    let currentRunningTasks = [];

    // Add tasks that are now (state.time) completed - as indicated by their
    // completion time (doneAt) - to the completed tasks.
    for (const task of state.runningTasks) {
      if (task.doneAt === state.time) currentCompletedSteps.push(task.step);
      else currentRunningTasks.push(task);
    }

    // Steps (!) that are not done and not running
    const currentPendingSteps = stepsStillPending(
      state.pending,
      currentCompletedSteps,
      currentRunningTasks
    );

    if (currentPendingSteps.length === 0 && currentRunningTasks.length === 0)
      return state.time;

    // Closes over currentCompletedSteps and deps and extracted just for readability
    const depsAreDone = step =>
      deps[step].every(r => currentCompletedSteps.includes(r));

    const availableWorkers = workerCount - currentRunningTasks.length;

    // These tasks can now be worked on since their dependencies are done. Only
    // take as many as we have idle workers.
    const tasksToStart = currentPendingSteps
      .filter(depsAreDone)
      .sort()
      .slice(0, availableWorkers)
      // Turn the steps (string) into tasks { doneAt: number, step: string }
      .map(step => ({
        step,
        doneAt: state.time + getCost(step)
      }));

    const nextRunningTasks = currentRunningTasks.concat(tasksToStart);
    const nextPendingSteps = stepsStillPending(
      currentPendingSteps,
      currentCompletedSteps,
      nextRunningTasks
    );

    const nextState = {
      ...state,
      idleWorkers: workerCount - nextRunningTasks.length,
      completed: currentCompletedSteps,
      runningTasks: nextRunningTasks,
      time: state.time + 1,
      pending: nextPendingSteps
    };

    return go(nextState);
  };

  return go({
    completed: [],
    pending: Object.keys(deps),
    idleWorkers: workerCount,
    time: 0,
    runningTasks: [] // { doneAt: 123, step: "A"}
  });
};

module.exports = {
  id: "7",
  parseLine,
  parseData,
  runDeps,
  runDepsWithWorkers,
  run: data => {
    const deps = parseData(data);

    const p1 = runDeps(deps).join("");
    const p2 = runDepsWithWorkers(deps, 5);
    return `p1: ${p1} p2: ${p2}`;
  }
};
