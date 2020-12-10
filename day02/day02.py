import re
from typing import List, Dict

def main():
    objects = get_data()

    part1_answer = part1(objects)
    part2_answer = part2(objects)

    print(f"Part 1 answer: {part1_answer}")
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[Dict]:
    with open("data.txt") as file:
        lines = file.read()

    lines = lines.split("\n")
    ret = []

    for line in lines:
        word1, word2, word3 = line.split(" ")

        num1, num2 = word1.split("-")
        num1 = int(num1)
        num2 = int(num2)

        ret.append({
            "num1": num1,
            "num2": num2,
            "letter": word2[0],
            "password": word3,
        })

    return ret


def part1(objects: List[Dict]) -> int:
    count = 0

    for obj in objects:
        num1 = obj["num1"]
        num2 = obj["num2"]
        letter = obj["letter"]
        password = obj["password"]

        matches = re.findall(letter, password)
        num_matches = len(matches)

        if (num1 <= num_matches) and (num_matches <= num2):
            count += 1

    return count


def part2(objects: List[Dict]) -> int:
    count = 0

    for obj in objects:
        num1 = obj["num1"]
        num2 = obj["num2"]
        letter = obj["letter"]
        password = obj["password"]

        test_letter1 = password[num1 - 1]
        test_letter2 = password[num2 - 1]

        if (test_letter1 == letter) and (test_letter2 == letter):
            continue
        elif (test_letter1 != letter) and (test_letter2 != letter):
            continue
        else:
            count += 1

    return count


if __name__ == "__main__":
    main()
