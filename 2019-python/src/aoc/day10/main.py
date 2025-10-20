import os

from .part1 import part1
from .part2 import part2


def main() -> None:
    data_path = os.path.join(os.path.dirname(__file__), "data.txt")
    with open(data_path) as file:
        grid = file.read().strip("\n").split("\n")

    print(f"PART1: {part1(grid)}")
    print(f"PART2: {part2(grid)}")
