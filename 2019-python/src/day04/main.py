from collections.abc import Iterator
from itertools import pairwise


def main() -> None:
    part1_answer = part1()
    print(f"PART1: {part1_answer}")
    part2_answer = part2()
    print(f"PART2: {part2_answer}")


def part1() -> int:
    nums = range(356261, 846303 + 1)
    nums = filter(is_valid1, nums)
    return sum(1 for _ in nums)


def is_valid1(n: int) -> bool:
    digits = list(get_digits(n))
    has_adjacent = any(d1 == d2 for d1, d2 in pairwise(digits))
    non_decreasing = all(d1 <= d2 for d1, d2 in pairwise(digits))
    return has_adjacent and non_decreasing


def part2() -> int:
    nums = range(356261, 846303 + 1)
    nums = filter(is_valid2, nums)
    return sum(1 for _ in nums)


def is_valid2(n: int) -> bool:
    digits = list(get_digits(n))
    non_decreasing = all(d1 <= d2 for d1, d2 in pairwise(digits))
    return non_decreasing and has_consecutive(digits, 2)


def has_consecutive[T](items: list[T], k: int) -> bool:
    prev = None
    count = 0
    for item in items:
        if item == prev:
            count += 1
        else:
            if count == k:
                return True
            else:
                prev = item
                count = 1
    return count == k


def get_digits(n: int) -> Iterator[int]:
    if n == 0:
        return
    yield from get_digits(n // 10)
    yield n % 10


if __name__ == "__main__":
    main()
