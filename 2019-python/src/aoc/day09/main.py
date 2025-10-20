import os
from collections import defaultdict
from typing import Final

type Index = int

# OPCODES
ADD: Final = 1
MULT: Final = 2
SAVE: Final = 3
OUTPUT: Final = 4
JUMP_IF_TRUE: Final = 5
JUMP_IF_FALSE: Final = 6
LESS_THAN: Final = 7
EQUAL: Final = 8
RELATIVE_BASE_OFFSET: Final = 9
HALT: Final = 99

# MODES
POINTER: Final = 0
LITERAL: Final = 1
RELATIVE: Final = 2


def main() -> None:
    data_path = os.path.join(os.path.dirname(__file__), "data.txt")
    with open(data_path) as file:
        num_strings = file.read().strip("\n").split(",")

    program = [int(num_str) for num_str in num_strings]

    part1_answer = part1(program)
    print(f"PART1: {part1_answer}")
    part2_answer = part2(program)
    print(f"PART2: {part2_answer}")


def part1(program: list[int]):
    return execute_program(program, input_=1)


def part2(program: list[int]):
    return execute_program(program, input_=2)


def execute_program(program: list[int], input_: int) -> int:
    program_dict = defaultdict[Index, int](lambda: 0)
    for i, val in enumerate(program):
        program_dict[i] = val

    i = 0
    relative_base = 0
    outputs: list[int] = []

    while True:
        instruction = program_dict[i]

        # get the last 2 digits
        opcode = instruction % 100

        if opcode in {ADD, MULT, LESS_THAN, EQUAL}:
            operand1 = resolve_operand(program_dict, relative_base, i, 1)
            operand2 = resolve_operand(program_dict, relative_base, i, 2)
            save_location = get_param_memory_address(program_dict, relative_base, i, 3)
            if opcode == ADD:
                program_dict[save_location] = operand1 + operand2
            elif opcode == MULT:
                program_dict[save_location] = operand1 * operand2
            elif opcode == LESS_THAN:
                program_dict[save_location] = int(operand1 < operand2)
            elif opcode == EQUAL:
                program_dict[save_location] = int(operand1 == operand2)
            i += 4
        elif opcode in {JUMP_IF_TRUE, JUMP_IF_FALSE}:
            operand1 = resolve_operand(program_dict, relative_base, i, 1)
            operand2 = resolve_operand(program_dict, relative_base, i, 2)
            if operand1 == 0:
                i = operand2 if opcode == JUMP_IF_FALSE else i + 3
            else:
                i = operand2 if opcode == JUMP_IF_TRUE else i + 3
        elif opcode == RELATIVE_BASE_OFFSET:
            operand1 = resolve_operand(program_dict, relative_base, i, 1)
            relative_base += operand1
            i += 2
        elif opcode == SAVE:
            save_location = get_param_memory_address(program_dict, relative_base, i, 1)
            program_dict[save_location] = input_
            i += 2
        elif opcode == OUTPUT:
            output = resolve_operand(program_dict, relative_base, i, 1)
            outputs.append(output)
            i += 2
        elif opcode == HALT:
            return outputs[-1]
        else:
            raise ValueError(f"unknown opcode {opcode} at index {i}")


def resolve_operand(
    program_dict: dict[Index, int],
    relative_base: int,
    instruction_ptr: int,
    k: int,
) -> int:
    """
    resolves the k'th operand after the instruction_ptr
    k is 1-indexed
    EXAMPLES:
        TODO: build examples
    """
    i = get_param_memory_address(program_dict, relative_base, instruction_ptr, k)
    return program_dict[i]


def get_param_memory_address(
    program_dict: dict[Index, int],
    relative_base: int,
    instruction_ptr: int,
    k: int,
) -> int:
    """
    resolves the k'th operand after the instruction_ptr
    k is 1-indexed
    EXAMPLES:
        TODO: build examples
    """
    assert k >= 1

    # get the mode
    instruction_digits = str(program_dict[instruction_ptr])
    try:
        mode = int(instruction_digits[-2 - k])
    except IndexError:
        mode = POINTER

    # based on the mode, resolve the final value
    if mode == LITERAL:
        return instruction_ptr + k
    elif mode == POINTER:
        return program_dict[instruction_ptr + k]
    elif mode == RELATIVE:
        return program_dict[instruction_ptr + k] + relative_base
    else:
        raise ValueError("invalid mode")


if __name__ == "__main__":
    main()
