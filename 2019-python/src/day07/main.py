import os

from .part1 import part1
from .part2 import part2


def main() -> None:
    data_path = os.path.join(os.path.dirname(__file__), "data.txt")
    with open(data_path) as file:
        data_str = file.read()
    instruction_strings = data_str.strip("\n").split(",")
    program = [int(s) for s in instruction_strings]

    part1_answer = part1(program)
    print(f"PART1: {part1_answer}")
    part2_answer = part2(program)
    print(f"PART2: {part2_answer}")


if __name__ == "__main__":
    main()
