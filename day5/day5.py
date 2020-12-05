import math
from typing import List

def main():
    boarding_passes: List[str] = get_data()

    seat_ids = map(compute_seat_id, boarding_passes)
    seat_ids = list(seat_ids)

    part1_answer = max(seat_ids)
    print(f"Part 1 answer: {part1_answer}")

    seat_ids = sorted(seat_ids)
    for i in range(len(seat_ids)):
        lower = seat_ids[i]
        upper = seat_ids[i+1]

        if (lower + 1 != upper):
            part2_answer = upper - 1
            print(f"Part 2 answer: {part2_answer}")
            break


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def compute_seat_id(boarding_pass: str) -> int:
    row = compute_row(boarding_pass)
    column = compute_column(boarding_pass)
    return (row * 8) + column


def compute_row(boarding_pass: str) -> int:
    row_string = boarding_pass[:7]
    bottom = 0
    top = 127

    for char in row_string:
        midpoint = bottom + ((top - bottom) / 2)

        if char == "F":
            top = midpoint
        elif char == "B":
            bottom = midpoint

    if row_string[-1] == "F":
        return math.ceil(bottom)
    elif row_string[-1] == "B":
        return math.floor(top)


def compute_column(boarding_pass: str) -> int:
    column_string = boarding_pass[-3:]
    bottom = 0
    top = 7

    for char in column_string:
        midpoint = bottom + ((top - bottom) / 2)

        if char == "L":
            top = midpoint
        elif char == "R":
            bottom = midpoint

    if column_string[-1] == "L":
        return math.ceil(bottom)
    elif column_string[-1] == "R":
        return math.floor(top)


if __name__ == "__main__":
    main()
