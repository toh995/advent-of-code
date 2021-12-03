import copy
from typing import List, Optional, Tuple

ADD_SYMBOL = "+"
MULT_SYMBOL = "*"


def main():
    lines = get_data()

    # part 2
    line_lists = [x.split(" ") for x in lines]

    to_sum = []
    for line in lines:
        components = line.split(" ")
        try:
            value = evaluate(components)
        except:
            print(components)
            raise
        to_sum.append(value)


    part1_answer = sum([evaluate(x) for x in line_lists])
    print(f"Part 1 answer: {part1_answer}")


def get_data() -> List[str]:
    with open("data.txt") as file:
        lines: str = file.read()[:-1]
    return lines.split("\n")


def evaluate(components: List[str], ret: Optional[int] = None) -> int:
    ret = None

    expr_string = "".join(components)
    if ("(" not in expr_string) and (")" not in expr_string):
        if len(components) == 1:
            ret = int(components[0])

        elif (ADD_SYMBOL in expr_string) and (MULT_SYMBOL in expr_string):
            components = copy.deepcopy(components)

            first_add_index = -1
            for i in range(len(components)):
                if components[i] == ADD_SYMBOL:
                    if first_add_index == -1:
                        components[i-1] = "(" + components[i-1]
                        first_add_index = i
                    if ((i+2) >= len(components)) \
                        or (components[i+2] == MULT_SYMBOL):
                        components[i+1] = components[i+1] + ")"
                        first_add_index = -1

            ret = evaluate(components)

        else:
            ret = int(components[0])

            i = 1
            while i < len(components):
                operator = components[i]

                next_val = components[i+1]
                next_val = int(next_val)

                if operator == ADD_SYMBOL:
                    ret += next_val
                elif operator == MULT_SYMBOL:
                    ret *= next_val

                i += 2

    else:
        reduced_components = []

        i = 0
        while i < len(components):
            sub_expression, i_delta = find_sub_expression(components[i:])
            i += i_delta

            if sub_expression[0].startswith("("):
                sub_expression[0] = sub_expression[0][1:]
                sub_expression[-1] = sub_expression[-1][:-1]
            val = evaluate(sub_expression)
            val = str(val)

            reduced_components.append(val)

            if (i+1) < len(components):
                operator = components[i + 1]
                reduced_components.append(operator)

            i += 2

        ret = evaluate(reduced_components)

    return ret


def find_sub_expression(components: List[str]) -> Tuple[List[str], int]:
    first_component = components[0]

    if not first_component.startswith("("):
        return [first_component], 0

    else:
        num_start_parentheses: int = first_component.count("(")
        num_end_parentheses: int = 0

        ret_list: List[str] = [components[0]]
        i: int = 0

        while num_end_parentheses < num_start_parentheses:
            i += 1
            curr_component = components[i]

            if curr_component.isdigit():
                ret_list.append(curr_component)
                continue

            if curr_component in [ADD_SYMBOL, MULT_SYMBOL]:
                ret_list.append(curr_component)
                continue

            if curr_component.endswith(")"):
                num_end_parentheses += curr_component.count(")")
                ret_list.append(curr_component)
                continue

            if curr_component.startswith("("):
                num_start_parentheses += curr_component.count("(")
                ret_list.append(curr_component)

        return ret_list, i


if __name__ == "__main__":
    main()

