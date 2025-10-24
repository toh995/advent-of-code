import math
from copy import deepcopy
from dataclasses import dataclass, field
from typing import override


@dataclass(kw_only=True)
class State:
    position: list[int] = field(default_factory=list)
    velocity: list[int] = field(default_factory=list)


def part2() -> int:
    states = [
        State(position=[-16, -1, -12]),
        State(position=[0, -4, -17]),
        State(position=[-11, 11, 0]),
        State(position=[2, 2, -6]),
    ]
    for state in states:
        state.velocity = [0, 0, 0]

    return math.lcm(
        *(find_cycle_length(states, axis) for axis in range(3)),
    )


def find_cycle_length(states: list[State], axis: int) -> int:
    @dataclass(kw_only=True)
    class SingleState:
        position: int
        velocity: int

        @override
        def __eq__(self, other: object) -> bool:
            return (
                isinstance(other, SingleState)
                and self.position == other.position
                and self.velocity == other.velocity
            )

        @override
        def __hash__(self) -> int:
            return hash((self.position, self.velocity))

    curr_single_states: tuple[SingleState, ...] = tuple(
        SingleState(
            position=state.position[axis],
            velocity=state.velocity[axis],
        )
        for state in states
    )

    cycle_length = 0

    seen = set[tuple[SingleState, ...]]()
    while curr_single_states not in seen:
        seen.add(curr_single_states)
        cycle_length += 1

        # move to the next state
        curr_single_states = deepcopy(curr_single_states)

        # update velocities
        for state1 in curr_single_states:
            for state2 in curr_single_states:
                state1.velocity += gravity_delta(state1.position, state2.position)

        # update positions
        for state in curr_single_states:
            state.position += state.velocity

    return cycle_length


def gravity_delta(coord_val1: int, coord_val2: int) -> int:
    """Computes the gravity, to apply to coord_val1, wrt coord_val2"""
    if coord_val1 < coord_val2:
        return 1
    elif coord_val1 > coord_val2:
        return -1
    else:
        return 0
