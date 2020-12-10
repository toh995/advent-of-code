import copy
import functools
from typing import List, Tuple

def main():
    numbers = get_data()
    numbers = [int(x) for x in numbers]
    numbers = sorted(numbers)

    # part 1
    part1_answer = part1(numbers)
    print(f"Part 1 answer: {part1_answer}")

    # part 2
    numbers = tuple(numbers)
    part2_answer = part2(numbers, start_num=0)
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def part1(numbers: List[int]) -> int:
    """
    NOTE: we assume that the "numbers" list is already sorted from lowsest
    to highest, when supplied to this function.
    """
    numbers = copy.deepcopy(numbers)

    # prepend 0 to the list
    numbers.insert(0, 0)

    # append max number plus 3 to the list (i.e. the device voltage)
    max_num = max(numbers)
    numbers.append(max_num + 3)

    # begin calculation
    differences = []
    for i in range(1, len(numbers)):
        new_val = numbers[i] - numbers[i-1]
        differences.append(new_val)

    diff1 = [x for x in differences if x == 1]
    diff3 = [x for x in differences if x == 3]

    return len(diff1) * len(diff3)


@functools.lru_cache(maxsize=None)
def part2(numbers: Tuple[int], start_num: int) -> int:
    """
    recursive function that calculates how many arrangements of adapters
    we can have.

    assume that the "numbers" tuple is already sorted when supplied to the
    function. the tuple should be sorted from lowest to highest number.

    also, notice that the "lru_cache" decorator is applied to this function.
    this essentially memoizes the function, which dramatically improves
    performance.
    """
    numbers = copy.deepcopy(numbers)

    if len(numbers) in [0,1]:
        return 1

    else:
        # sum will be the return value here
        sum = 0
        n = 2 if len(numbers) == 2 else 3

        for i in range(n):
            next_num = numbers[i]
            difference = next_num - start_num

            if difference in [1,2,3]:
                sum += part2(numbers[i+1:], start_num=next_num)

        return sum


if __name__ == "__main__":
    main()
