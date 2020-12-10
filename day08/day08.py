import copy
from typing import Optional, List

def main():
    instructions_list: List[str] = get_data()

    part1_answer = part1(instructions_list)
    part2_answer = part2(instructions_list)

    print(f"Part 1 answer: {part1_answer}")
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n")


def part1(instructions_list: List[str]) -> int:
    instructions_list = copy.deepcopy(instructions_list)
    ret = compute_accumulator(instructions_list)
    return ret


def part2(instructions_list: List[str]) -> Optional[int]:
    # the value of ret may be changed in the ensuing for loop
    ret: Optional[int] = None

    for i in range(len(instructions_list)):
        instruction: str = instructions_list[i]

        commands: set = {"nop", "jmp"}

        current_command: str = instruction[:3]
        if current_command in commands:
            new_command: str = next(iter(commands - {current_command}))

            new_instruction = instruction.replace(current_command, new_command)
            new_instructions_list = copy.deepcopy(instructions_list)
            new_instructions_list[i] = new_instruction

            if boots_correctly(new_instructions_list):
                ret = compute_accumulator(new_instructions_list)
                break

    return ret


def compute_accumulator(instructions_list: List[str]) -> int:
    instructions_list = copy.deepcopy(instructions_list)

    # accum is the return value
    accum = 0
    index = 0
    index_set = set()

    while index not in index_set:
        instruction = instructions_list[index]
        index_set.add(index)

        command, number = instruction.split(" ")
        number = int(number)

        if command == "acc":
            accum += number
            index += 1

        if command == "jmp":
            index += number

        if command == "nop":
            index += 1

        if index == len(instructions_list) - 1:
            break

    return accum


def boots_correctly(instructions_list: List[str]) -> bool:
    instructions_list = copy.deepcopy(instructions_list)

    index = 0
    index_set = set()

    ret = False

    while index not in index_set:
        instruction = instructions_list[index]
        index_set.add(index)

        command, number = instruction.split(" ")
        number = int(number)

        if command == "acc":
            index += 1

        if command == "jmp":
            index += number

        if command == "nop":
            index += 1

        if index == len(instructions_list) - 1:
            ret = True
            break

    return ret


if __name__ == "__main__":
    main()
