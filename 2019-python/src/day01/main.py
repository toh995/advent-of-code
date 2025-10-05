from functools import cache


@cache
def compute_fuel(mass: int) -> int:
    return (mass // 3) - 2


@cache
def compute_total(fuel: int) -> int:
    if fuel <= 0:
        return 0
    else:
        child = compute_fuel(fuel)
        return fuel + compute_total(child)


if __name__ == "__main__":
    with open("data.txt") as file:
        lines = file.readlines()
    nums = [int(line) for line in lines]

    # PART 1
    part1_answer = sum(compute_fuel(num) for num in nums)
    print(f"PART 1: {part1_answer}")

    # PART 2
    part2_answer = sum(compute_total(compute_fuel(num)) for num in nums)
    print(f"PART 2: {part2_answer}")
