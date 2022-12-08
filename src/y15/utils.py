from typing import Union, List, Optional


def load_input(filename: str = "input.txt", method: str = "readlines") -> Optional[Union[List[str], str]]:
    with open(filename, "r") as f:
        assert hasattr(f, method), "Wrong method on file handler!"
        return getattr(f, method)()
