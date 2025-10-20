from typing import Final

ADD: Final = 1
MULT: Final = 2
SAVE: Final = 3
OUTPUT: Final = 4
HALT: Final = 99

POINTER: Final = 0
LITERAL: Final = 1


def part1(program: list[int]) -> int:
    return execute_instructions(program, input_=1)


def execute_instructions(program: list[int], input_: int) -> int:
    program = program.copy()
    i = 0

    while True:
        instruction = program[i]
        digit_str = str(instruction)

        opcode = int(digit_str[-2:])

        if opcode == ADD or opcode == MULT:
            operand1_mode = int(digit_str[-3]) if len(digit_str) >= 3 else POINTER
            operand2_mode = int(digit_str[-4]) if len(digit_str) >= 4 else POINTER

            operand1 = (
                program[i + 1] if operand1_mode == LITERAL else program[program[i + 1]]
            )
            operand2 = (
                program[i + 2] if operand2_mode == LITERAL else program[program[i + 2]]
            )

            save_location = program[i + 3]
            if opcode == ADD:
                program[save_location] = operand1 + operand2
            elif opcode == MULT:
                program[save_location] = operand1 * operand2
            i += 4
        elif opcode == SAVE:
            save_location = program[i + 1]
            program[save_location] = input_
            i += 2
        elif opcode == OUTPUT:
            operand1_mode = int(digit_str[-3]) if len(digit_str) >= 3 else POINTER
            operand1 = (
                program[i + 1] if operand1_mode == LITERAL else program[program[i + 1]]
            )
            output = operand1
            if program[i + 2] == HALT:
                return output
            elif output != 0:
                raise ValueError(f"bad output {output} at instruction index {i}")
            i += 2
        else:
            raise ValueError(f"unknown opcode {opcode} at index {i}")
