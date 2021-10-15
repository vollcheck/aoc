from typing import Any, List


def load_input(filename: str = "input.txt", method: str = "readlines") -> Any:
    with open(filename, "r") as f:
        assert hasattr(f, method), "Wrong method on file handler!"
        return getattr(f, method)()


def calculate_surface(boxes: List[str]) -> int:
    boxes = [(int(n) for n in box.split("x")) for box in boxes]

    result = 0
    for box in boxes:
        l, w, h = box
        surfaces = [2*l*w, 2*w*h, 2*h*l]
        addition = min(surfaces) / 2
        result += sum(surfaces) + addition
    return result


def calculate_surface2(boxes: List[str]) -> int:
    boxes = [(int(n) for n in box.split("x")) for box in boxes]

    result = 0
    for box in boxes:
        l, w, h = box
        bow = l * w * h
        surfaces = [l, w, h]
        surfaces.remove(max(surfaces))
        wrapping = sum(i+i for i in surfaces)
        result += bow + wrapping
    return result


data = load_input()
calculate_surface2(data)
