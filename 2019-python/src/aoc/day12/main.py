from .part1 import part1
from .part2 import part2


def main() -> None:
    # data_path = os.path.join(os.path.dirname(__file__), "data.txt")
    # with open(data_path) as file:
    #     lines = file.read().strip("\n").split("\n")

    # program = [int(num_str) for num_str in num_strings]

    # print(lines)

    print(f"PART1: {part1()}")
    print(f"PART2: {part2()}")
    # print(f"PART2: {part2(program)}")
