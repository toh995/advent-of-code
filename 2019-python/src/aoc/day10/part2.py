import itertools
import math
from collections import defaultdict
from collections.abc import Iterator
from dataclasses import dataclass
from enum import Enum, auto, unique
from functools import cached_property
from operator import attrgetter
from typing import Final

from .part1 import count_detectable

type Coord = tuple[int, int]
type Grid = list[str]

ASTEROID: Final = "#"
EMPTY: Final = "."


@unique
class Region(Enum):
    UP = auto()
    TOP_RIGHT = auto()
    RIGHT = auto()
    BOTTOM_RIGHT = auto()
    DOWN = auto()
    BOTTOM_LEFT = auto()
    LEFT = auto()
    TOP_LEFT = auto()


CLOCKWISE_REGIONS: Final = [
    Region.UP,
    Region.TOP_RIGHT,
    Region.RIGHT,
    Region.BOTTOM_RIGHT,
    Region.DOWN,
    Region.BOTTOM_LEFT,
    Region.LEFT,
    Region.TOP_LEFT,
]


@dataclass(frozen=True, kw_only=True)
class Slope:
    di: int
    dj: int

    @cached_property
    def magnitude(self) -> float:
        if self.dj == 0:
            return math.inf
        else:
            return abs(self.di / self.dj)

    @cached_property
    def going_up(self) -> bool:
        return self.di < 0

    @cached_property
    def going_down(self) -> bool:
        return self.di > 0

    @cached_property
    def going_left(self) -> bool:
        return self.dj < 0

    @cached_property
    def going_right(self) -> bool:
        return self.dj > 0

    @cached_property
    def region(self) -> Region:
        if self.going_up and not self.going_left and not self.going_right:
            return Region.UP
        elif self.going_up and self.going_right:
            return Region.TOP_RIGHT
        elif self.going_right and not self.going_up and not self.going_down:
            return Region.RIGHT
        elif self.going_down and self.going_right:
            return Region.BOTTOM_RIGHT
        elif self.going_down and not self.going_left and not self.going_right:
            return Region.DOWN
        elif self.going_down and self.going_left:
            return Region.BOTTOM_LEFT
        elif self.going_left and not self.going_down and not self.going_up:
            return Region.LEFT
        elif self.going_up and self.going_left:
            return Region.TOP_LEFT
        else:
            raise RuntimeError()


def part2(grid: Grid):
    asteroid_coords = find_asteroid_coords(grid)
    origin_coord = max(
        asteroid_coords,
        key=lambda coord: count_detectable(coord, asteroid_coords),
    )
    iter_ = vaporize_iter(asteroid_coords, origin_coord)
    for _ in range(199):
        _ = next(iter_)
    last_coord = next(iter_)
    y, x = last_coord
    return (100 * x) + y


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


def vaporize_iter(coords: list[Coord], origin_coord: Coord) -> Iterator[Coord]:
    region_to_slopes = defaultdict[Region, list[Slope]](list)
    slope_to_coords = defaultdict[Slope, list[Coord]](list)
    for coord in coords:
        if coord == origin_coord:
            continue
        slope = compute_slope(origin_coord, coord)
        slope_to_coords[slope].append(coord)
        region_to_slopes[slope.region].append(slope)

    # uniq all slopes
    for k, v in region_to_slopes.items():
        region_to_slopes[k] = list(set(v))

    # sort
    region_to_slopes[Region.TOP_RIGHT].sort(key=attrgetter("magnitude"), reverse=True)
    region_to_slopes[Region.BOTTOM_RIGHT].sort(key=attrgetter("magnitude"))
    region_to_slopes[Region.BOTTOM_LEFT].sort(key=attrgetter("magnitude"), reverse=True)
    region_to_slopes[Region.TOP_LEFT].sort(key=attrgetter("magnitude"))

    for slope, coords_list in slope_to_coords.items():
        coords_list.sort(
            reverse=True,
            key=lambda coord: abs(math.dist(coord, origin_coord)),
        )

    # loop through
    for k in itertools.cycle(CLOCKWISE_REGIONS):
        v = region_to_slopes[k]
        for slope in v:
            coords = slope_to_coords[slope]
            if not coords:
                continue
            yield coords.pop()


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
