from collections import defaultdict
from collections.abc import Generator, Iterator
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


class Board:
    _grid: dict[Coord, Tile]

    def __init__(self) -> None:
        """initializes an empty board"""
        self._grid = dict()

    def __iter__(self) -> Iterator[tuple[Coord, Tile]]:
        return iter(self._grid.items())

    def set(self, coord: Coord, tile: Tile) -> None:
        self._grid[coord] = tile


def part1(program: list[int]) -> int:
    board = Board()
    run_program1(program, board)
    block_tiles = (tile for _, tile in board if tile is Tile.BLOCK)
    return sum(1 for _ in block_tiles)


def run_program1(program: list[int], board: Board) -> None:
    """Run the program, drawing things onto the board"""
    program_executor = init_program_executor(program)
    # _ = next(program_executor)
    while True:
        try:
            x = next(program_executor)
            y = next(program_executor)
            tile = Tile(next(program_executor))
            assert isinstance(x, int)
            assert isinstance(y, int)
        except StopIteration:
            break
        board.set((x, y), tile)


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
