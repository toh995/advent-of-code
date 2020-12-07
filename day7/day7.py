import copy
from typing import List, Set, Dict, Tuple

def main():
    rules: List[str] = get_data()

    # part 1
    rules_dict: dict = parse_rules1(rules)
    part1_answer: set = part1(rules_dict, "shiny gold")
    part1_answer: int = len(part1_answer)
    print(f"Part 1 answer: {part1_answer}")

    # part 2
    rules_dict: dict = parse_rules2(rules)
    part2_answer: int = part2(rules_dict, "shiny gold")
    print(f"Part 2 answer: {part2_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        rules: str = file.read()
    return rules.split("\n")


def parse_rules1(rules: List[str]) -> Dict[str, Set[str]]:
    """
    rules_dict will be return value. we will build out the rest of "rules_dict"
    in this function.

    in rules_dict, each key represents a parent, and each value is a
    set containing all of the descendants of the parent.

    example input:
    shiny indigo bags contain 4 vibrant lime bags.
    clear lime bags contain 1 dotted lime bag, 2 clear gold bags.

    example output:
    {
        "shiny indigo": {"vibrant lime"},
        "clear lime": {"dotted lime", "clear gold"},
    }
    """
    rules_dict: dict = {}

    for rule in rules:
        # remove trailing period from the rule
        rule = rule[:-1]

        parent, children = rule.split(" contain ")

        # parse out parent
        # i.e. "shiny indigo bags" => "shiny indigo"
        parent = " ".join(parent.split(" ")[:-1])

        # parse out the children
        children = children.split(", ")

        # children_set that will be added to rules_dict
        children_set = set()

        for child in children:
            # for each child, parse out the child
            # i.e. "4 vibrant lime bags" => "vibrant lime"
            child = child.split(" ")[1:-1]
            child = " ".join(child)
            children_set.add(child)

        # add stuff to rules_dict
        if parent in rules_dict:
            rules_dict[parent] = rules_dict[parent].union(children_set)
        else:
            rules_dict[parent] = children_set

    return rules_dict


def part1(rules_dict: Dict[str, Set[str]], bag: str) -> Set[str]:
    """
    recursive function that returns the set of all ancestors for the given bag.
    """

    # first, perform a deep copy of rules_dict, to protect against mutating
    # the dict inadvertently
    rules_dict = copy.deepcopy(rules_dict)

    # the "ancestors" set will be the return value
    ancestors = set()

    # iterate through rules_dict. if the bag appears as a child in the dict,
    # then add that parent to the "ancestors" set.
    for parent, children_set in rules_dict.items():
        if bag in children_set:
            ancestors.add(parent)

            # find the ancestors for the parent, then add them to the
            # "ancestors" set.
            great_ancestors = part1(rules_dict, parent)
            ancestors = ancestors.union(great_ancestors)

    return ancestors


def parse_rules2(rules: List[str]) -> Dict[str, List[Tuple[str, int]]]:
    """
    rules_dict will be return value. we will build out the rest of "rules_dict"
    in this function.

    in rules_dict, the key represents a parent, and the value is
    a list of tuples, where each tuple represents a direct child of the parent.

    in each "child" tuple, the entries represent the following:
    1) the name of the child (i.e. "vibrant lime"
    2) the quantity of how many of the child belongs to the parent (i.e. 4)

    example input:
    shiny indigo bags contain 4 vibrant lime bags.
    clear lime bags contain 1 dotted lime bag, 2 clear gold bags.

    example output:
    {
        "shiny indigo": [("vibrant lime", 4)],
        "clear lime": [("dotted lime", 1), ("clear gold", 2)],
    }
    """
    rules_dict: dict = {}

    for rule in rules:
        # remove trailing period
        rule = rule[:-1]

        parent, children = rule.split(" contain ")

        # parse out parent
        # i.e. "shiny indigo bags" => "shiny indigo"
        parent = " ".join(parent.split(" ")[:-1])

        # parse out the children
        children = children.split(", ")

        # children_list that will be added to rules_dict
        children_list = []

        # iterate through each of the children, and add the "child tuple" to
        # children_list
        for child in children:
            # if the first child is "no other bags", then that means that
            # the current parent has no children, so we will just stop
            # iterating through the loop.
            if child == "no other bags":
                break

            # get the bag name.
            # i.e. "4 vibrant lime bags" => "vibrant lime"
            bag_name = child.split(" ")[1:-1]
            bag_name = " ".join(bag_name)

            # get the quantity of the child.
            # i.e. "4 vibrant lime bags" => 4
            quantity = child.split(" ")[0]
            quantity = int(quantity)

            # append the child tuple to children_list
            child_tuple = (bag_name, quantity)
            children_list.append(child_tuple)

        # add stuff to rules_dict
        if parent in rules_dict:
            rules_dict[parent] = rules_dict[parent].extend(children_list)
        else:
            rules_dict[parent] = children_list

    return rules_dict


def part2(rules_dict: Dict[str, List[Tuple[str, int]]], bag: str) -> int:
    """
    recursive function that returns the TOTAL number of descendants for the
    given bag.
    """

    # first, perform a deep copy of rules_dict, to protect against mutating
    # the dict inadvertently
    rules_dict = copy.deepcopy(rules_dict)

    children_list = rules_dict[bag]

    # define the return value
    total = 0

    for child in children_list:
        child_bag, quantity = child

        total += quantity
        total += (quantity * part2(rules_dict, child_bag))

    return total


if __name__ == "__main__":
    main()
