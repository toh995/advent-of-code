from typing import Final

ADD: Final = 1
MULT: Final = 2
SAVE: Final = 3
OUTPUT: Final = 4
JUMP_IF_TRUE: Final = 5
JUMP_IF_FALSE: Final = 6
LESS_THAN: Final = 7
EQUAL: Final = 8
HALT: Final = 99

POINTER: Final = 0
LITERAL: Final = 1


def part2(program: list[int]) -> int:
    return execute_instructions(program, input_=5)


def execute_instructions(program: list[int], input_: int) -> int:
    program = program.copy()
    i = 0

    while True:
        instruction = program[i]

        # get the last 2 digits
        opcode = instruction % 100

        if opcode in {ADD, MULT, LESS_THAN, EQUAL}:
            operand1 = resolve_operand(program, i, 1)
            operand2 = resolve_operand(program, i, 2)
            save_location = program[i + 3]
            if opcode == ADD:
                program[save_location] = operand1 + operand2
            elif opcode == MULT:
                program[save_location] = operand1 * operand2
            elif opcode == LESS_THAN:
                program[save_location] = int(operand1 < operand2)
            elif opcode == EQUAL:
                program[save_location] = int(operand1 == operand2)
            i += 4
        elif opcode in {JUMP_IF_TRUE, JUMP_IF_FALSE}:
            operand1 = resolve_operand(program, i, 1)
            operand2 = resolve_operand(program, i, 2)
            if operand1 == 0:
                i = operand2 if opcode == JUMP_IF_FALSE else i + 3
            else:
                i = operand2 if opcode == JUMP_IF_TRUE else i + 3
        elif opcode == SAVE:
            save_location = program[i + 1]
            program[save_location] = input_
            i += 2
        elif opcode == OUTPUT:
            output = resolve_operand(program, i, 1)
            if program[i + 2] == HALT:
                return output
            elif output != 0:
                raise ValueError(f"bad output {output} at instruction index {i}")
            else:
                i += 2
        else:
            raise ValueError(f"unknown opcode {opcode} at index {i}")


def resolve_operand(program: list[int], instruction_ptr: int, k: int) -> int:
    """
    resolves the k'th operand after the instruction_ptr
    k is 1-indexed
    EXAMPLES:
        TODO: build examples
    """
    assert k >= 1

    # get the mode
    instruction_digits = str(program[instruction_ptr])
    try:
        mode = int(instruction_digits[-2 - k])
    except IndexError:
        mode = POINTER

    # based on the mode, resolve the final value
    if mode == LITERAL:
        return program[instruction_ptr + k]
    elif mode == POINTER:
        return program[program[instruction_ptr + k]]
    else:
        raise ValueError("invalid mode")
