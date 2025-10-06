from collections.abc import Iterator
from dataclasses import dataclass
from typing import Literal, TypeIs

type Direction = Literal["U", "D", "L", "R"]
type Coord = tuple[int, int]


@dataclass(frozen=True)
class Instruction:
    direction: Direction
    magnitude: int


def main() -> None:
    with open("data.txt") as file:
        line1, line2 = file.readlines()
    instructions1 = list(parse_instructions(line1))
    instructions2 = list(parse_instructions(line2))

    part1_answer = part1(instructions1, instructions2)
    print(f"PART1: {part1_answer}")
    part2_answer = part2(instructions1, instructions2)
    print(f"PART2: {part2_answer}")


def part1(
    instructions1: list[Instruction],
    instructions2: list[Instruction],
) -> int:
    coords1 = enumerate_coords_from((0, 0), instructions1)
    coords2 = enumerate_coords_from((0, 0), instructions2)
    intersection_coords = set(coords1) & set(coords2)
    distances = (manhattan_distance(coord, (0, 0)) for coord in intersection_coords)
    return min(distances)


def part2(
    instructions1: list[Instruction],
    instructions2: list[Instruction],
) -> int:
    coords1 = enumerate_coords_from((0, 0), instructions1)
    coords2 = enumerate_coords_from((0, 0), instructions2)
    intersection_coords = set(coords1) & set(coords2)
    steps1 = count_steps((0, 0), intersection_coords, instructions1)
    steps2 = count_steps((0, 0), intersection_coords, instructions2)
    # compute final counts
    return min(
        (steps1[coord] + steps2[coord] for coord in intersection_coords),
    )


def enumerate_coords_from(
    origin: Coord,
    instructions: list[Instruction],
) -> Iterator[Coord]:
    curr_coord = origin
    for instruction in instructions:
        x, y = curr_coord
        match instruction:  # pyright: ignore[reportMatchNotExhaustive]
            case Instruction("U", magnitude):
                yield from ((x, y + dy) for dy in range(1, magnitude + 1))
                curr_coord = (x, y + magnitude)
            case Instruction("D", magnitude):
                yield from ((x, y - dy) for dy in range(1, magnitude + 1))
                curr_coord = (x, y - magnitude)
            case Instruction("L", magnitude):
                yield from ((x - dx, y) for dx in range(1, magnitude + 1))
                curr_coord = (x - magnitude, y)
            case Instruction("R", magnitude):
                yield from ((x + dx, y) for dx in range(1, magnitude + 1))
                curr_coord = (x + magnitude, y)


def manhattan_distance(coord1: Coord, coord2: Coord) -> int:
    x1, y1 = coord1
    x2, y2 = coord2
    return abs(x1 - x2) + abs(y1 - y2)


def count_steps(
    origin_coord: Coord,
    target_coords: set[Coord],
    instructions: list[Instruction],
) -> dict[Coord, int]:
    target_coords = target_coords.copy()
    if not target_coords:
        return dict()
    ret = dict[Coord, int]()
    num_steps = 0
    for coord in enumerate_coords_from(origin_coord, instructions):
        num_steps += 1
        if coord in target_coords:
            ret[coord] = num_steps
            target_coords.remove(coord)
            if not target_coords:
                break
    return ret


def parse_instructions(line: str) -> Iterator[Instruction]:
    for s in line.split(","):
        direction, magnitude_str = s[0], s[1:]
        if is_direction(direction):
            yield Instruction(direction, int(magnitude_str))
        else:
            raise ValueError("malformed!")


def is_direction(x: object) -> TypeIs[Direction]:
    return isinstance(x, str) and x in "UDLR"


if __name__ == "__main__":
    main()
