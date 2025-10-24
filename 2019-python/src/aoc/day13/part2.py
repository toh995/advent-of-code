from __future__ import annotations

from collections import defaultdict
from collections.abc import Iterator
from enum import Enum, unique
from typing import Final

type Coord = tuple[int, int]
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


@unique
class Tile(Enum):
    EMPTY = 0
    WALL = 1
    BLOCK = 2
    HORIZONTAL_PADDLE = 3
    BALL = 4


def part2(program: list[int]) -> int:
    program = program.copy()
    program[0] = 2

    score = 0
    input_ = [0]
    paddle_x = 0
    ball_x = 0

    program_executor = init_program_executor(program, input_)

    while True:
        try:
            x = next(program_executor)
            y = next(program_executor)
            third_item = next(program_executor)
        except StopIteration:
            break
        if (x, y) == (-1, 0):
            score = third_item
            continue
        else:
            match Tile(third_item):
                case Tile.HORIZONTAL_PADDLE:
                    paddle_x = x
                case Tile.BALL:
                    ball_x = x
                case Tile.WALL | Tile.BLOCK | Tile.EMPTY:
                    pass
            if ball_x > paddle_x:
                input_[0] = 1
            elif ball_x < paddle_x:
                input_[0] = -1
            else:
                input_[0] = 0

    return score


def init_program_executor(program: list[int], input_: list[int]) -> Iterator[int]:
    program_dict = defaultdict[Index, int](lambda: 0)
    for i, val in enumerate(program):
        program_dict[i] = val

    i = 0
    relative_base = 0

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
            program_dict[save_location] = input_[-1]
            i += 2
        elif opcode == OUTPUT:
            yield resolve_operand(program_dict, relative_base, i, 1)
            i += 2
        elif opcode == HALT:
            return
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
