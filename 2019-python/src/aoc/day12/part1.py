from dataclasses import dataclass, field

type Position = list[int]
type Velocity = list[int]


@dataclass(kw_only=True)
class State:
    position: Position = field(default_factory=list)
    velocity: Velocity = field(default_factory=list)

    def energy(self) -> int:
        potential_energy = sum(abs(x) for x in self.position)
        kinetic_energy = sum(abs(x) for x in self.velocity)
        return potential_energy * kinetic_energy


def part1():
    states = [
        State(position=[-16, -1, -12]),
        State(position=[0, -4, -17]),
        State(position=[-11, 11, 0]),
        State(position=[2, 2, -6]),
    ]
    for state in states:
        state.velocity = [0, 0, 0]

    for _ in range(1_000):
        # update velocities
        # for each pair, compute gravity and mutate next_states
        for state1 in states:
            for state2 in states:
                # update state1 wrt state2
                for i in range(len(state1.velocity)):
                    state1.velocity[i] += gravity_delta(
                        state1.position[i], state2.position[i]
                    )

        # update positions
        for state in states:
            for i in range(len(state.velocity)):
                state.position[i] += state.velocity[i]

    total_energy = sum(state.energy() for state in states)
    return total_energy


def gravity_delta(coord_val1: int, coord_val2: int) -> int:
    """Computes the gravity, to apply to coord_val1, wrt coord_val2"""
    if coord_val1 < coord_val2:
        return 1
    elif coord_val1 > coord_val2:
        return -1
    else:
        return 0
