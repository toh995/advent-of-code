import copy
from typing import List, Tuple, Optional

def main():
    numbers: List[int] = get_numbers()

    # part 1
    # find two numbers that add up to 2020 and return their product
    num1, num2 = find_addends(numbers, target_sum=2020)
    print(f"Part 1 answer: {num1 * num2}")

    # part 2
    # find three numbers that add up to 2020 and return their product
    for num1 in numbers:
        target_sum = 2020 - num1
        new_numbers = copy.deepcopy(numbers)
        new_numbers.remove(num1)

        addends = find_addends(new_numbers, target_sum=target_sum)
        if isinstance(addends, tuple):
            num2, num3 = addends
            print(f"Part 2 answer: {num1 * num2 * num3}")
            break


def get_numbers() -> List[int]:
    with open("data.txt") as file:
        numbers = file.read()

    numbers = numbers.split("\n")
    numbers = [int(x) for x in numbers]
    numbers = sorted(numbers)
    return numbers


def find_addends(numbers: List[int],
                 target_sum: int) -> Optional[Tuple[int, int]]:
    """
    Given a list of integers and a target_sum, find two integers in the list
    that add up to target_sum.

    Returns a tuple of the two addends, or None if couldn't be identified.
    """
    test_numbers = [x for x in numbers if x < target_sum/2]

    for num1 in test_numbers:
        num2 = target_sum - num1

        if num2 in numbers:
            return (num1, num2)

    return None


if __name__ == "__main__":
    main()
