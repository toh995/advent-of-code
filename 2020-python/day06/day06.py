from typing import List

def main():
    answer_groups: List[str] = get_data()

    part1_answer = part1(answer_groups)
    part2_answer = part2(answer_groups)

    print(f"Part 1 answer: {part1_answer}")
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n\n")


def part1(answer_groups: List[str]) -> int:
    count = 0
    for group in answer_groups:
        answers = group.split("\n")

        s = set()
        for answer in answers:
            for char in answer:
                s.add(char)

        count += len(s)

    return count


def part2(answer_groups: List[str]) -> int:
    count = 0
    for group in answer_groups:
        answers = group.split("\n")
        answers = [set(x) for x in answers]
        s = set.intersection(*answers)
        count += len(s)

    return count


if __name__ == "__main__":
    main()
