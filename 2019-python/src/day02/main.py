def main() -> None:
    with open("data.txt") as file:
        input_str = file.readline()
    num_strings = input_str.split(",")
    nums = [int(num_str) for num_str in num_strings]

    part1_answer = part1(nums.copy())
    print(f"PART1: {part1_answer}")

    part2_answer = part2(nums.copy())
    print(f"PART2: {part2_answer}")


def part1(nums: list[int]) -> int:
    nums[1] = 12
    nums[2] = 2
    run_program(0, nums)
    return nums[0]


def part2(nums: list[int]) -> int:
    target = 19690720
    for noun in range(100):
        for verb in range(100):
            input_ = nums.copy()
            input_[1] = noun
            input_[2] = verb
            run_program(0, input_)
            if input_[0] == target:
                return (100 * noun) + verb
    raise ValueError("could not find solution")


def run_program(i: int, nums: list[int]) -> None:
    match nums[i]:
        case 99:
            return
        case 1:
            new_val = nums[nums[i + 1]] + nums[nums[i + 2]]
            nums[nums[i + 3]] = new_val
            run_program(i + 4, nums)
        case 2:
            new_val = nums[nums[i + 1]] * nums[nums[i + 2]]
            nums[nums[i + 3]] = new_val
            run_program(i + 4, nums)
        case _:
            raise ValueError(f"unexpected value {nums[i]} at index {i}")


if __name__ == "__main__":
    main()
