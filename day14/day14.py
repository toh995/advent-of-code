import copy
import re
import itertools
from typing import List


def main():
    lines = get_data()

    memory_dict: dict = {}
    mask: str

    for line in lines:
        if line.startswith("mask"):
            mask = line.split(" = ")[1]
        else:
            key, value = line.split(" = ")
            result = compute_result(value, mask)
            memory_dict[key] = result

    answer = sum(memory_dict.values())
    print(answer)

    part2_answer = part2(lines)
    print(part2_answer)


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def compute_result(value, mask):
    # convert value to a binary string
    value = int(value)
    value = format(value, "b")

    amount_to_add = 36 - len(value)
    value = ("0" * amount_to_add) + value

    result_list = list(value)

    for i in range(len(mask)):
        if mask[i] in ["1","0"]:
            result_list[i] = mask[i]
    result = "".join(result_list)
    return int(result, 2)


def part2(lines: List[str]):
    memory_dict: dict = {}
    mask: str

    for line in lines:
        if line.startswith("mask"):
            mask = line.split(" = ")[1]
        else:
            key, value = line.split(" = ")
            key = re.search("\[(\d+)\]", key).groups()[0]
            key = int(key)
            value = int(value)

            addresses = compute_addresses(key, mask)

            for address in addresses:
                memory_dict[address] = value

    return sum(memory_dict.values())


def compute_addresses(key: str, mask: str) -> str:
    # convert value to a binary string
    key = int(key)
    key = format(key, "b")

    amount_to_add = 36 - len(key)
    key = ("0" * amount_to_add) + key

    result_list = list(key)
    x_indexes = []

    for i in range(len(mask)):
        mask_digit = mask[i]

        if mask_digit == "0":
            continue
        elif mask_digit == "1":
            result_list[i] = mask_digit
        elif mask_digit == "X":
            x_indexes.append(i)

    addresses = []

    sets = [{"0","1"} for _ in range(len(x_indexes))]
    for tup in itertools.product(*sets):
        new_address = copy.deepcopy(result_list)

        for i in range(len(tup)):
            x_index = x_indexes[i]
            new_address[x_index] = tup[i]

        new_address = "".join(new_address)
        addresses.append(new_address)

    return addresses


if __name__ == "__main__":
    main()
