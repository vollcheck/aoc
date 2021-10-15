from typing import List


def load_input(filename: str = "input.txt") -> List[str]:
    with open(filename, 'r') as f:
        return f.read()


def calculate_floor(instructions: str) -> int:
    status = 0
    for i in instructions:
        if i == "(":
            status += 1
        elif i == ")":
            status -= 1

    return status


# second part
def calculate_floor2(instructions: str) -> int:
    status = 0
    instr_no = 0
    for i in instructions:
        instr_no += 1
        if i == "(":
            status += 1
        elif i == ")":
            status -= 1

        if status == -1:
            return instr_no


instrs = load_input("input.txt")
print(instrs)
print(calculate_floor2(instrs))
