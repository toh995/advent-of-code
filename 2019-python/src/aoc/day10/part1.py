from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Final

type Coord = tuple[int, int]

type Grid = list[str]


ASTEROID: Final = "#"
EMPTY: Final = "."


@dataclass(frozen=True, kw_only=True)
class Slope:
    di: int
    dj: int

    def __neg__(self) -> Slope:
        return Slope(
            di=-self.di,
            dj=-self.dj,
        )


def part1(grid: Grid):
    asteroid_coords = find_asteroid_coords(grid)
    return max(
        (
            count_detectable(start_coord, asteroid_coords)
            for start_coord in asteroid_coords
        ),
    )


def find_asteroid_coords(grid: Grid) -> list[Coord]:
    assert grid and grid[0]
    ret: list[Coord] = []
    num_rows = len(grid)
    num_cols = len(grid[0])
    for i in range(num_rows):
        for j in range(num_cols):
            if grid[i][j] == ASTEROID:
                ret.append((i, j))
    return ret


def count_detectable(
    start_coord: Coord,
    all_coords: list[Coord],
    # grid: Grid,
) -> int:
    slopes = {
        compute_slope(start_coord, other_coord)
        for other_coord in all_coords
        if other_coord != start_coord
    }
    return len(slopes)


def compute_slope(from_coord: Coord, to_coord: Coord) -> Slope:
    i1, j1 = from_coord
    i2, j2 = to_coord
    di = i2 - i1
    dj = j2 - j1
    gcd = math.gcd(di, dj)
    return Slope(
        di=di // gcd,
        dj=dj // gcd,
    )


def move(coord: Coord, slope: Slope) -> Coord:
    i, j = coord
    return (
        i + slope.di,
        j + slope.dj,
    )


def is_in_bounds[T](coord: Coord, grid: Grid) -> bool:
    assert grid and grid[0]
    num_rows = len(grid)
    num_cols = len(grid[0])
    i, j = coord
    return 0 <= i < num_rows and 0 <= j < num_cols
