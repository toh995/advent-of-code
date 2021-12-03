import math
from typing import List, Tuple

def main():
    lines = get_data()

    # part 1
    part1_answer = part1(lines)
    print(f"Part 1 answer: {part1_answer}")

    # part 2
    part2_answer = part2(lines)
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def part1(lines: List[str]) -> float:
    x = 0
    y = 0
    angle = 0

    for line in lines:
        command = line[0]
        value = int(line[1:])

        if command == "N":
            y += value
        elif command == "S":
            y -= value
        elif command == "E":
            x += value
        elif command == "W":
            x -= value
        elif command == "L":
            angle += value
        elif command == "R":
            angle -= value
        elif command == "F":
            radians = math.radians(angle)
            x += (value * math.cos(radians))
            y += (value * math.sin(radians))

    return abs(x) + abs(y)


def part2(lines: List[str]) -> int:
    ship_x = 0
    ship_y = 0
    way_x = 10
    way_y = 1

    for line in lines:
        command = line[0]
        value = int(line[1:])

        if command == "N":
            way_y += value
        elif command == "S":
            way_y -= value
        elif command == "E":
            way_x += value
        elif command == "W":
            way_x -= value
        elif command == "L":
            way_x, way_y = rotate_left((way_x, way_y), value)
        elif command == "R":
            value = 360 - value
            way_x, way_y = rotate_left((way_x, way_y), value)
        elif command == "F":
            ship_x += (way_x * value)
            ship_y += (way_y * value)

    return abs(ship_x) + abs(ship_y)


def rotate_left(point: Tuple[int, int], degrees: int) -> Tuple[int, int]:
    x, y = point

    if degrees == 90:
        return (-y, x)
    if degrees == 180:
        return (-x, -y)
    if degrees == 270:
        return (y, -x)


if __name__ == "__main__":
    main()
