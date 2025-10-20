from __future__ import absolute_import

from aoc.day10.main import Slope, compute_slope, count_detectable, find_asteroid_coords


def test_negate_slope():
    slope = Slope(di=3, dj=4)
    assert -slope == Slope(di=-3, dj=-4)


def test_find_asteroid_coords():
    grid = [
        ".#..#",
        ".....",
        "#####",
        "....#",
        "...##",
    ]
    result = find_asteroid_coords(grid)
    result_set = set(result)
    assert len(result) == len(result_set)
    assert result_set == {
        (0, 1),
        (0, 4),
        (2, 0),
        (2, 1),
        (2, 2),
        (2, 3),
        (2, 4),
        (3, 4),
        (3, 4),
        (4, 3),
        (4, 4),
    }


def test_compute_slope():
    assert compute_slope((0, 0), (6, 8)) == Slope(di=3, dj=4)


def test_count_detectable():
    grid = [
        ".#..#",
        ".....",
        "#####",
        "....#",
        "...##",
    ]
    all_coords = [
        (0, 1),
        (0, 4),
        (2, 0),
        (2, 1),
        (2, 2),
        (2, 3),
        (2, 4),
        (3, 4),
        (3, 4),
        (4, 3),
        (4, 4),
    ]
    assert count_detectable((0, 1), all_coords, grid) == 7
