import copy

from sympy import Symbol, solve

def main():
    lines = get_lines()
    line_map = build_line_map(lines)
    part2_answer = part2(line_map)
    print(f'PART 2: {part2_answer}')

def get_lines():
    with open("../../data/Day21.txt") as file:
        inputStr = file.read()

    lines = inputStr.split("\n")
    lines = [l for l in lines if l]

    return lines

def build_line_map(lines):
    ret = {}
    for line in lines:
        first, second = line.split(': ')
        ret[first] = second
    return ret

def evaluate(line_map, key):
    if key not in line_map:
        return Symbol(key)

    line = line_map[key]

    if ' ' not in line:
        return Symbol(line)

    first, operation, second = line.split(' ')

    first_evaled = evaluate(line_map, first)
    second_evaled = evaluate(line_map, second)
    
    if operation == '+':
        return my_simplify(first_evaled + second_evaled)
    elif operation == '-':
        return my_simplify(first_evaled - second_evaled)
    elif operation == '*':
        return my_simplify(first_evaled * second_evaled)
    elif operation == '/':
        return my_simplify(first_evaled / second_evaled)

def my_simplify(sympy_expr):
    try:
        return eval(str(sympy_expr))
    except:
        return sympy_expr

def part2(line_map):
    line_map = copy.deepcopy(line_map)
    root_line = line_map['root']
    first, _, second = root_line.split(' ')

    del line_map['humn']

    first_evaled = evaluate(line_map, first)
    second_evaled = evaluate(line_map, second)

    [solved] = solve(first_evaled - second_evaled, Symbol('humn'))
    return my_simplify(solved)


if __name__ == "__main__":
    main()
