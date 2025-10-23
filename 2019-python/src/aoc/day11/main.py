from __future__ import annotations

import os
from collections import defaultdict
from collections.abc import Generator, Iterator
from dataclasses import dataclass
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
class Color(Enum):
    BLACK = 0
    WHITE = 1


@unique
class RotationDirection(Enum):
    COUNTERCLOCKWISE = 0
    CLOCKWISE = 1


@unique
class Direction(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT = 3

    def rotate(self, rotation_dir: RotationDirection) -> Direction:
        match rotation_dir:
            case RotationDirection.CLOCKWISE:
                next_val = self.value + 1
            case RotationDirection.COUNTERCLOCKWISE:
                next_val = self.value - 1
        return Direction(next_val % len(Direction))


COORD_DELTAS: dict[Direction, Coord] = {
    Direction.UP: (-1, 0),
    Direction.RIGHT: (0, 1),
    Direction.DOWN: (1, 0),
    Direction.LEFT: (0, -1),
}


@dataclass(kw_only=True)
class Position:
    coord: Coord
    direction: Direction

    def rotate(self, rotation_dir: RotationDirection) -> None:
        self.direction = self.direction.rotate(rotation_dir)

    def move_forward(self) -> None:
        i, j = self.coord
        di, dj = COORD_DELTAS[self.direction]
        self.coord = (i + di, j + dj)


class Board:
    _white_coords: set[Coord]

    def __init__(self) -> None:
        self._white_coords = set()

    def set_color(self, coord: Coord, color: Color) -> None:
        if color is Color.WHITE:
            self._white_coords.add(coord)
        else:
            self._white_coords.discard(coord)

    def get_color(self, coord: Coord) -> Color:
        if coord in self._white_coords:
            return Color.WHITE
        else:
            return Color.BLACK

    def serialize(self) -> str:
        i_min = min(i for i, _ in self._white_coords)
        i_max = max(i for i, _ in self._white_coords)
        j_min = min(j for _, j in self._white_coords)
        j_max = max(j for _, j in self._white_coords)

        row_strs: list[str] = []
        for i in range(i_min, i_max + 1):
            row: list[str] = []
            for j in range(j_min, j_max + 1):
                curr_color = self.get_color((i, j))
                if curr_color is Color.WHITE:
                    row.append("█")
                else:
                    row.append(" ")
            row_str = "".join(row)
            row_strs.append(row_str)
        return "\n".join(row_strs)


def main() -> None:
    data_path = os.path.join(os.path.dirname(__file__), "data.txt")
    with open(data_path) as file:
        num_strings = file.read().strip("\n").split(",")

    program = [int(num_str) for num_str in num_strings]

    print(f"PART1: {part1(program)}")
    print(f"PART2:\n{part2(program)}")


def part1(program: list[int]) -> int:
    board = Board()
    start_position = Position(
        coord=(0, 0),
        direction=Direction.UP,
    )
    painted_coords = do_moves(program, board, start_position)
    return len(set(painted_coords))


def part2(program: list[int]) -> str:
    board = Board()
    start_position = Position(
        coord=(0, 0),
        direction=Direction.UP,
    )
    board.set_color(start_position.coord, Color.WHITE)
    for _ in do_moves(program, board, start_position):
        pass
    return board.serialize()


def do_moves(
    program: list[int],
    board: Board,
    start_position: Position,
) -> Iterator[Coord]:
    position = start_position
    program_executor = init_program_executor(program)

    while True:
        # Check for halting condition
        try:
            _ = next(program_executor)
        except StopIteration:
            break

        # Paint the next color
        curr_color = board.get_color(position.coord)
        new_color = program_executor.send(curr_color.value)
        new_color = Color(new_color)
        board.set_color(position.coord, new_color)

        # We painted!
        yield position.coord

        # Rotate position, then move forward
        rotation_dir = RotationDirection(next(program_executor))
        position.rotate(rotation_dir)
        position.move_forward()


type ProgramExecutor = Generator[int | None, int]


def init_program_executor(program: list[int]) -> ProgramExecutor:
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
            program_dict[save_location] = yield
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
