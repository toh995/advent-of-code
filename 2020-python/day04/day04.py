import re
from typing import List, Dict


def main():
    passports: List[str] = get_data()

    part1_answer = part1(passports)
    part2_answer = part2(passports)

    print(f"Part 1 answer: {part1_answer}")
    print(f"Part 2 answer: {part2_answer}")



def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()
    return lines.split("\n\n")


def part1(passports: List[str]) -> int:
    required_keys = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
    count = 0

    for passport in passports:
        attrs = re.split(" |\n", passport)
        keys = {x.split(":")[0] for x in attrs}

        if keys.issuperset(required_keys):
            count += 1

    return count


def part2(passports: List[str]) -> int:
    count = 0

    for passport in passports:
        passport = re.split(" |\n", passport)
        passport = [x.split(":") for x in passport]
        passport_dict = {k:v for k,v in passport}

        if is_valid(passport_dict):
            count += 1

    return count


def is_valid(passport_dict: Dict) -> bool:
    # check if all required keys exist in the passport_dict
    required_keys = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
    keys = set(passport_dict.keys())

    if not keys.issuperset(required_keys):
        return False

    # byr
    byr = passport_dict["byr"]
    byr = int(byr)
    if (byr < 1920) or (byr > 2002):
        return False

    # iyr
    iyr = passport_dict["iyr"]
    iyr = int(iyr)
    if (iyr < 2010) or (iyr > 2020):
        return False

    # eyr
    eyr = passport_dict["eyr"]
    eyr = int(eyr)
    if (eyr < 2020) or (eyr > 2030):
        return False

    # hgt
    hgt = passport_dict["hgt"]
    match = re.match("^(\d+)(cm|in)$", hgt)

    if match is None:
        return False

    number, unit = match.groups()
    number = int(number)
    if unit == "cm":
        if (number < 150) or (number > 193):
            return False

    if unit == "in":
        if (number < 59) or (number > 76):
            return False

    # hcl
    hcl = passport_dict["hcl"]
    match = re.match("^#[0-9a-f]{6}$", hcl)
    if match is None:
        return False

    # ecl
    ecl = passport_dict["ecl"]
    valid_colors = {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}
    if ecl not in valid_colors:
        return False

    # pid
    pid = passport_dict["pid"]
    match = re.match("^\d{9}$", pid)
    if match is None:
        return False

    # finished!
    return True


if __name__ == "__main__":
    main()
