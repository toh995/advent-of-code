import copy
import itertools
from typing import List, Tuple

EMPTY: str = "L"
OCCUPIED: str = "#"
FLOOR: str = "."


def main():
    lines: List[str] = get_data()

    # matrix is a list of lists
    matrix: List[List[str]] = [list(x) for x in lines]

    part1_answer = compute_answer(matrix, part=1)
    print(f"Part 1 answer: {part1_answer}")

    part2_answer = compute_answer(matrix, part=2)
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def compute_answer(matrix: List[List[str]], part: int) -> int:
    """
    "matrix" represents the current layout of seats.
    "part" tells us whether we are on part 1 or part 2.
    """
    curr_matrix = matrix
    curr_count = compute_num_occupied_seats(curr_matrix)

    while True:
        new_matrix = compute_new_matrix(curr_matrix, part)
        new_count = compute_num_occupied_seats(new_matrix)

        if curr_count == new_count:
            break
        else:
            curr_matrix = new_matrix
            curr_count = new_count

    return curr_count


def compute_num_occupied_seats(matrix: List[List[str]]) -> int:
    sum = 0
    for row in matrix:
        for seat in row:
            if seat == OCCUPIED:
                sum += 1
    return sum


def compute_new_matrix(matrix: List[List[str]], part: int) -> List[List[str]]:
    new_matrix = copy.deepcopy(matrix)

    for x in range(len(matrix)):
        for y in range(len(matrix[0])):
            new_seat = ""

            if part == 1:
                new_seat = compute_new_seat1(matrix, (x,y))
            elif part == 2:
                new_seat = compute_new_seat2(matrix, (x,y))

            new_matrix[x][y] = new_seat

    return new_matrix


def compute_new_seat1(matrix: List[List[str]],
                      start_point: Tuple[int, int]) -> str:
    x, y = start_point
    seat = matrix[x][y]

    adjacent_seats = get_adjacent_seats(matrix, start_point, go_long=False)
    occupied_seats = [x for x in adjacent_seats if x == OCCUPIED]
    num_occupied = len(occupied_seats)

    if (seat == EMPTY) and (num_occupied == 0):
        return OCCUPIED
    elif (seat == OCCUPIED) and (num_occupied >= 4):
        return EMPTY
    else:
        return seat


def compute_new_seat2(matrix: List[List[str]],
                      start_point: Tuple[int, int]) -> str:
    x, y = start_point
    seat = matrix[x][y]

    adjacent_seats = get_adjacent_seats(matrix, start_point, go_long=True)
    occupied_seats = [x for x in adjacent_seats if x == OCCUPIED]
    num_occupied = len(occupied_seats)

    if (seat == EMPTY) and (num_occupied == 0):
        return OCCUPIED
    elif (seat == OCCUPIED) and (num_occupied >= 5):
        return EMPTY
    else:
        return seat


def get_adjacent_seats(matrix: List[List[str]], start_point: Tuple[int, int],
                       go_long: bool) -> List[str]:
    """
    returns a list of seats adjacent to the one specified by start_point.

    go_long tells us whether we travel long distances to find the next seat,
    or just travel one seat away from start_point.
    """
    # calculate the cartesian product
    slopes = itertools.product(
        {-1, 0, 1},
        {-1, 0, 1},
    )
    slopes = list(slopes)

    slopes.remove((0, 0))

    ret = []

    for slope in slopes:
        seat = get_adjacent_seat(matrix, start_point, slope, go_long)
        ret.append(seat)

    return ret


def get_adjacent_seat(matrix: List[List[str]], start_point: Tuple[int, int],
                      slope: Tuple[int, int], go_long: bool) -> str:
    x, y = start_point
    y_delta, x_delta = slope

    x_min = y_min = 0
    x_max = len(matrix) - 1
    y_max = len(matrix[0]) - 1

    while True:
        x += x_delta
        y += y_delta

        if (x < x_min) or (x > x_max) \
                or (y < y_min) or (y > y_max):
            return ""

        seat = matrix[x][y]

        if not go_long:
            return seat

        if seat in [OCCUPIED, EMPTY]:
            return seat


if __name__ == "__main__":
    main()
