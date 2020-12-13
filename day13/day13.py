from typing import List

from sympy.ntheory.modular import crt


def main():
    lines = get_data()

    # part 1
    minimum = int(lines[0])
    bus_ids = lines[1].split(",")
    bus_ids = [int(x) for x in bus_ids if x.isdigit()]

    part1_answer = part1(bus_ids, minimum)
    print(f"Part 1 answer: {part1_answer}")

    # part 2
    bus_ids = lines[1].split(",")

    part2_answer = part2(bus_ids)
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def part1(bus_ids: List[int], minimum: int) -> int:
    earliest_times = [
        earliest_time_for_bus(bus_id, minimum) for bus_id in bus_ids
    ]
    earliest_time = min(earliest_times)
    index = earliest_times.index(earliest_time)

    bus_id = bus_ids[index]

    return bus_id * (earliest_time - minimum)


def part2(bus_ids: List[str]) -> int:
    divisors = []
    remainders = []

    for index, bus_id in enumerate(bus_ids):
        if bus_id.isdigit():
            bus_id = int(bus_id)

            divisors.append(bus_id)
            remainders.append(-index)

    # chinese remainder theorem from sympy
    answer, _ = crt(divisors, remainders)

    return answer


def earliest_time_for_bus(bus_id: int, minimum: int) -> int:
    counter = 0
    while True:
        counter += bus_id
        if counter >= minimum:
            return counter


if __name__ == "__main__":
    main()
