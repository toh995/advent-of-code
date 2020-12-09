import copy
import itertools
from typing import List

def main():
    numbers: list = get_data()
    numbers: list = [int(x) for x in numbers]

    # part 1
    index = 25
    while sum_exists(numbers, index):
        index += 1

    print(f"Part 1 answer: {numbers[index]}")

    # part 2
    target: int = numbers[index]
    continguous_set: List[int] = find_contiguous_set(numbers, target)

    num1 = max(continguous_set)
    num2 = min(continguous_set)

    part2_answer = num1 + num2

    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def sum_exists(numbers: List[int], index: int) -> bool:
    numbers = copy.deepcopy(numbers)

    next_number: int = numbers[index]
    sub_list: List[int] = numbers[index-25:index]

    # iterate through all possible 2-tuples in sub_list
    for num1, num2 in itertools.combinations(sub_list, 2):
        if (num1 + num2) == next_number:
            return True

    return False


def find_contiguous_set(numbers: List[int], target: int) -> List[int]:
    numbers = copy.deepcopy(numbers)

    for start_index in range(len(numbers)):
        sub_list = []
        i = start_index

        while sum(sub_list) < target:
            sub_list.append(numbers[i])
            i += 1

        if sum(sub_list) == target:
            return sub_list

    return []


if __name__ == "__main__":
    main()
