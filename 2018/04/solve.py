from functools import reduce

import re
import time

def get_time(item):
    matches = re.match(r"^\[([^\]]+)\]", item)
    if matches is not None:
        date_str = matches.groups()[0]
        return time.strptime(date_str, "%Y-%m-%d %H:%M")
    return None

def process_item(item_str):
    ts = get_time(item_str)

    # e.g. "[1518-11-21 00:03] Guard #2521 begins shift"
    matches = re.search(r"Guard\s+(#\d+)", item_str)
    if matches is not None:
        return {"type": "NEW_GUARD", "ts": ts, "id": matches.groups()[0]}

    # e.g. "[1518-11-22 00:54] wakes up"
    matches = re.search(r"wakes up", item_str)
    if matches is not None:
        return {"type": "WAKE_UP", "ts": ts}

    # e.g. "[1518-11-22 00:54] falls asleep"
    matches = re.search(r"falls asleep", item_str)
    if matches is not None:
        return {"type": "FALL_ASLEEP", "ts": ts}

    return None

def get_log():
    with open("input") as f:
        items = [x for x in f.read().split("\n") if len(x) > 0]
        items.sort(key=get_time)
        return [x for x in [process_item(x) for x in items] if x is not None]

def replay_log(log):
    guards = dict()
    curr_guard = None
    for item in log:

        # Set the current guard
        if item["type"] == "NEW_GUARD":
            curr_guard = item["id"]
            if curr_guard not in guards.keys():
                guards[curr_guard] = []

        # Set the current fall-alseep time
        if item["type"] == "FALL_ASLEEP":
            if curr_guard is not None:
                curr_fa_time = item["ts"]
            else:
                print("WARNING: WAKE_UP event but no current guard")
                
        # Append a list of asleep minutes for the current guard
        if item["type"] == "WAKE_UP":
            if curr_guard is None:
                print("WARNING: WAKE_UP event but no current guard")
            elif curr_fa_time is None:
                print("WARNING: WAKE_UP event but no fall asleep time recorded")
            else:
                guards[curr_guard].append(
                    list(range(curr_fa_time.tm_min, item["ts"].tm_min))
                )
                curr_fa_time = None

    return guards

def get_guard_times():
    return replay_log(get_log())

def get_minutes_slept_for_guard(times):
    return reduce(lambda a, x: a + len(x), times, 0)

def guard_sleeping_for_max_minutes(guard_times):

    # Get the maximum value for get_minutes_slept_for_guard() in guard_times
    # and extract the guard ID
    guard_id = max(

        # Iterable is each (gid, times)
        guard_times.items(),

        # Use get_minutes_slept_for_guard() with the "times" for each element
        # to find the max
        key=lambda x: get_minutes_slept_for_guard(x[1])
    )[0]

    # Build a dict of minute numbers => number of times recorded sleeping for
    # that minute
    mins = dict()
    for entry in guard_times[guard_id]:
        for minute in entry:
            if minute in mins.keys():
                mins[minute] += 1
            else:
                mins[minute] = 1

    return {
        "id":    guard_id,
        "count": get_minutes_slept_for_guard(guard_times[guard_id]),
        "mins":  mins,
    }

def find_most_common_asleep_minute(guard_times):

    # Build a list of minutes [0..60), all set to 0
    mins = [0 for x in range(60)]

    # Go through all guard times and save each minute's total number of asleep
    # times.
    for (gid, data) in guard_times.items():
        for entry in data:
            for minute in entry:
                mins[minute] += 1

    # Return the minute number which is the list index that contains the
    # highest value (-1 ?)
    return mins.index(max(mins)) - 1

def get_guard_sleeping_most_at_minute(guard_times, minute):

    # Build a dict of guard ID => counter (default 0)
    guards = {x:0 for x in guard_times.keys()}

    for (gid, data) in guard_times.items():
        for entry in data:

            # If this entry (list of asleep minutes) contains the target
            # minute, increase guard's counter
            if minute in entry:
                guards[gid] += 1

    # Return (guard ID, minutes asleep) for maximum entry in "guards"
    return max(guards.items(), key=lambda x: x[1])


guard_times = get_guard_times()

# Get details of guard sleeping for most minutes
guard = guard_sleeping_for_max_minutes(guard_times)

# Get minute record with most times recorded asleep (min number, times
# recorded) and extract the minute number
guard_most_slept_minute = max(
    guard["mins"].items(),
    key=lambda x: x[1]
)[0]

print("The guard who slept most is {} ({} mins total)".format(guard["id"], guard["count"]))
print("This guard was most commonly sleeping on minute {}".format(guard_most_slept_minute))
print(
    "The answer to stage one is therefore: {}".format(
        int(guard["id"].replace("#", "")) * guard_most_slept_minute
    )
)

# Get minute that is most commonly recorded as having a sleeping guard
most_slept_minute = find_most_common_asleep_minute(guard_times)
print("\nThe most asleep minute was: {}".format(most_slept_minute))

# Find the guard that is most commonly asleep at that minute
lazy_guard = get_guard_sleeping_most_at_minute(guard_times, most_slept_minute)
print(
    "The guard most frequently asleep on this minute ({} times) is: {}".format(
        lazy_guard[1],
        lazy_guard[0]
    )
)
print(
    "The answer to stage two is therefore: {}".format(
        int(lazy_guard[0].replace("#", "")) * most_slept_minute
    )
)
