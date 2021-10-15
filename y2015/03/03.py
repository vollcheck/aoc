from typing import List, Any


def load_input(filename: str = "input.txt", method: str = "readlines") -> Any:
    with open(filename, "r") as f:
        assert hasattr(f, method), "Wrong method on file handler!"
        return getattr(f, method)()


north = "^"
south = "v"
east = ">"
west = "<"


def calculate_moves(steps: List[str]) -> int:
    places = set()
    w = h = 0
    places.add((w, h))
    for step in steps:
        if step == "^":
            h += 1
        elif step == "v":
            h -= 1
        elif step == ">":
            w += 1
        elif step == "<":
            w -= 1

        places.add((w, h))

    return len(places)


def calculate_moves2(steps: List[str]) -> int:
    santa_steps = steps[::2]
    robot_steps = [steps[s] for s in range(1, len(steps), 2)]

    places = set()

    for steps_coll in [santa_steps, robot_steps]:
        w = h = 0
        places.add((w, h))
        for step in steps_coll:
            if step == "^":
                h += 1
            elif step == "v":
                h -= 1
            elif step == ">":
                w += 1
            elif step == "<":
                w -= 1

            places.add((w, h))

    return len(places)


data = load_input(method="read")
print(calculate_moves2(data))
