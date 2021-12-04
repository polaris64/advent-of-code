import re

def parse_rule(rule_str):
    matches = re.search(r"^Step ([A-Z]+) must be finished before step ([A-Z]+)", rule_str)
    if matches is not None and len(matches.groups()) == 2:
        return (matches.groups()[0], matches.groups()[1])
    else:
        return None

def get_rules():
    with open("input") as f:
        rules = [parse_rule(x) for x in f.read().split("\n") if len(x.strip()) > 0]
        rules.sort(key=lambda x: x[0])
        return rules

def get_available_rules(rules, steps):
    avail = set()
    for rule in rules:
        master = rule[0]
        slaves = set()
        for r2 in rules:
            if r2[1] == master:
                slaves.add(r2[1])
        if len(slaves) == 0:
            avail.add(master)
            break
    avail = list(avail)
    avail.sort()
    return avail

def filter_rules(rules, steps):
    return [x for x in rules if len([y for y in steps if x[0] in y]) == 0]

def append_final_step(rules, steps):
    masters = set()
    for rule in rules:
        masters.add(rule[0])
    for rule in rules:
        if rule[1] not in masters:
            steps.append(rule[1])
            masters.add(rule[1])

def next_task(rules, steps, done, started):
    for step in steps:
        if step in done or step in started:
            continue
        required = set()
        for rule in rules:
            if rule[1] == step:
                required.add(rule[0])
        if len(required.difference(done)) == 0:
            return (step, True)
    return (None, False)

def get_length(task, addition):
    return ord(task.upper()) - 64 + addition

def perform_task(rules, steps, num_workers, additional_time):
    workers = [None for x in range(num_workers)]
    done = False
    started   = set()
    completed = set()
    ticks = 0
    while not done:
        for (idx, worker) in enumerate(workers):
            if worker is not None:
                if ticks >= worker[0] + get_length(worker[1], additional_time):
                    completed.add(worker[1])
                    workers[idx] = None

            if workers[idx] is None:
                to_start = next_task(rules, steps, completed, started)
                if to_start[1]:
                    started.add(to_start[0])
                    workers[idx] = (ticks, to_start[0])

        if len(completed) == len(steps):
            done = True
        else:
            ticks += 1
    return ticks

def run():
    initial_rules = get_rules()
    rules = get_rules()
    steps = list()
    while len(rules) > 0:
        steps.append(get_available_rules(rules, steps))
        rules = filter_rules(rules, steps)
    steps = [item for sublist in steps for item in sublist]
    append_final_step(initial_rules, steps)

    final_steps = "".join(steps)
    print("The correct order of steps is: {}".format(final_steps))

    final_time = perform_task(initial_rules, steps, 5, 60)
    print("The time taken for 5 workers to do all steps is: {}s".format(final_time))

    return (final_steps, final_time)

def run_example():
    rules_str = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n"
    rules = [parse_rule(x) for x in rules_str.split("\n") if len(x.strip()) > 0]
    steps = list()
    while len(rules) > 0:
        steps.append(get_available_rules(rules, steps))
        rules = filter_rules(rules, steps)
    steps = [item for sublist in steps for item in sublist]
    rules = [parse_rule(x) for x in rules_str.split("\n") if len(x.strip()) > 0]
    append_final_step(rules, steps)

    final_steps = "".join(steps)
    print("The correct order of steps is: {}".format(final_steps))

    final_time = perform_task(rules, steps, 2, 0)
    print("The time taken for 2 workers to do all steps is: {}s".format(final_time))

    return (final_steps, final_time)

def test_example():
    assert run_example() == ("CABDFE", 15)

def test_full():
    # 937s is wrong according to site
    assert run() == ("LAPFCRGHVZOTKWENBXIMSUDJQY", 937)

if __name__ == "__main__":
    #run_example()
    run()
