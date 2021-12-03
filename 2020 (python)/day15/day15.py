from typing import List, Dict


def main():
    starting_list = [9,6,0,10,18,2,1]

    part1_answer = compute_final_value(starting_list, n=2020)
    part2_answer = compute_final_value(starting_list, n=30000000)

    print(f"Part 1 answer: {part1_answer}")
    print(f"Part 2 answer: {part2_answer}")


def compute_final_value(starting_list: List[int], n: int) -> int:
    start_length = len(starting_list)

    # reference_dict is a dictionary, where the keys represent a spoken number,
    # and the values represent the last index at which the number was spoken.
    reference_dict: Dict[int, int] = {}

    for index in range(start_length - 1):
        num = starting_list[index]
        reference_dict[num] = index

    last_number = starting_list[-1]
    for index in range(start_length, n):
        print(f"n={n}, index={index}")

        if last_number not in reference_dict.keys():
            reference_dict[last_number] = index - 1
            last_number = 0

        else:
            last_index = reference_dict[last_number]
            reference_dict[last_number] = index - 1
            last_number = (index - 1) - last_index

    return last_number


if __name__ == "__main__":
    main()
