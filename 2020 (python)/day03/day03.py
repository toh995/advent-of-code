import functools
from typing import List

def main():
    lines: List[str] = get_data()

    # part 1
    # find the number of trees with x_delta=3 and y_delta=1
    part1_answer: int = find_num_trees(lines, x_delta=3, y_delta=1)
    print(f"Part 1 answer: {part1_answer}")

    # part 2
    # find the number of trees for various slopes, then multiply the numbers
    # together
    slopes = [
        (1, 1),
        (3, 1),
        (5, 1),
        (7, 1),
        (1, 2),
    ]

    factors = [
        find_num_trees(lines, x_delta, y_delta) for x_delta, y_delta in slopes
    ]

    part2_answer: int = functools.reduce(lambda x, y: x*y, factors)
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def find_num_trees(lines: List[str], x_delta: int, y_delta: int) -> int:
    """
    Find the number of trees that you would run into.

    lines is a list of strings.
    x_delta is the change in the x coordinate.
    y_delta is the change in the y coordinate.
    """
    line_length: int = len(lines[0])
    limit: int = int(len(lines) / y_delta)
    count: int = 0

    for i in range(limit):
        x: int = (x_delta * i) % line_length
        y: int = (y_delta * i)

        if lines[y][x] == "#":
            count += 1

    return count


if __name__ == "__main__":
    main()
