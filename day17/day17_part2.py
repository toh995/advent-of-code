import copy
import itertools
from typing import List

ACTIVE: str = "#"
INACTIVE: str = "."


def main():
    grid, lengths = get_grid()

    part2_answer = part2(grid, lengths)
    print(f"Part 2 answer: {part2_answer}")


def get_grid():
    grid = {}

    with open("data.txt") as file:
        file_string = file.read()

    rows = file_string.split("\n")

    # compute lengths
    x_length = int((len(rows[0]) - 1) / 2)
    y_length = int((len(rows) - 1) / 2)
    z_length = 0
    w_length = 0

    x_min = -x_length
    y_min = -y_length

    for i in range(len(rows)):
        row = rows[i]

        for j in range(len(row)):
            cube = row[j]
            grid[(x_min + j, y_min + i, 0, 0)] = cube

    return grid, (x_length, y_length, z_length, w_length)


def part2(grid, lengths):
    grid = copy.deepcopy(grid)
    lengths = copy.deepcopy(lengths)

    max_cycles = 6
    for _ in range(max_cycles):
        grid, lengths = execute_cycle(grid, lengths)

    return count_active_cubes(grid)


def count_active_cubes(grid):
    all_cubes = grid.values()

    active_cubes = filter(
        lambda cube: cube == ACTIVE,
        all_cubes
    )
    active_cubes = list(active_cubes)

    return len(active_cubes)


def execute_cycle(grid, lengths):
    x_length, y_length, z_length, w_length = lengths

    # compute new grid
    new_grid = copy.deepcopy(grid)

    points = itertools.product(
        range(-x_length - 1, x_length + 2),
        range(-y_length - 1, y_length + 2),
        range(-z_length - 1, z_length + 2),
        range(-w_length - 1, w_length + 2),
    )

    for point in points:
        new_cube = compute_new_cube(grid, point)
        new_grid[point] = new_cube

    # compute new lengths
    new_lengths = tuple(
        map(
            lambda n: n + 1,
            lengths,
        )
    )

    return new_grid, new_lengths


def compute_new_cube(grid, point):
    current_cube: str

    if point in grid:
        current_cube = grid[point]
    else:
        current_cube = INACTIVE

    neighbors: List[str] = get_neighbors(grid, point)
    num_active_neighbors = len([x for x in neighbors if x == ACTIVE])

    ret: str
    if current_cube == ACTIVE:
        if num_active_neighbors in [2, 3]:
            ret = ACTIVE
        else:
            ret = INACTIVE

    elif current_cube == INACTIVE:
        if num_active_neighbors == 3:
            ret = ACTIVE
        else:
            ret = INACTIVE

    return ret


def get_neighbors(grid, start_point):
    ret = []

    delta_tuples = itertools.product(
        {-1, 0, 1},
        {-1, 0, 1},
        {-1, 0, 1},
        {-1, 0, 1},
    )

    delta_tuples = list(delta_tuples)
    delta_tuples.remove((0, 0, 0, 0))

    for delta_tuple in delta_tuples:
        neighbor = get_neighbor(grid, start_point, delta_tuple)
        ret.append(neighbor)

    return ret


def get_neighbor(grid, start_point, delta_tuple):
    x, y, z, w = start_point
    x_delta, y_delta, z_delta, w_delta = delta_tuple

    new_point = (x+x_delta, y+y_delta, z+z_delta, w+w_delta)

    if new_point not in grid:
        return INACTIVE
    else:
        return grid[new_point]


if __name__ == "__main__":
    main()
