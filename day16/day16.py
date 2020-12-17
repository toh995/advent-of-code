import copy
from typing import List, Dict


def main():
    rules_dict, your_ticket, nearby_tickets = get_data()

    # part 1
    container: list = []
    for ticket in nearby_tickets:
        bad_numbers = get_bad_numbers(rules_dict, ticket)
        container.extend(bad_numbers)

    print(sum(container))

    # part 2
    valid_tickets = filter(
        lambda ticket: all([not is_bad_number(rules_dict, x) for x in ticket]),
        nearby_tickets,
    )
    valid_tickets = list(valid_tickets)

    index_to_field_map = compute_index_to_fields_map(rules_dict, valid_tickets)

    departure_indexes = []
    for index, field in index_to_field_map.items():
        if field.startswith("departure"):
            departure_indexes.append(index)

    factors = map(lambda index: your_ticket[index], departure_indexes)

    product = 1
    for factor in factors:
        product *= factor

    print(product)


def compute_index_to_fields_map(rules_dict: Dict, valid_tickets: List[List[int]]):
    # key is a field name
    # value is a list of indexes which could potentially apply to the key
    mapping = {}

    for key, endpoints in rules_dict.items():
        indexes = compute_possible_indexes(endpoints, valid_tickets)
        mapping[key] = indexes

    return eliminate(mapping, {})


def compute_possible_indexes(endpoints: List[int], valid_tickets:List[List[int]]) -> List[int]:
    ticket_length = len(valid_tickets[0])
    indexes = range(ticket_length)
    ret = filter(lambda i: is_good_index(valid_tickets, endpoints, i), indexes)
    ret = list(ret)
    return ret


def is_good_index(valid_tickets, endpoints, index):
    start1, end1, start2, end2 = endpoints

    for valid_ticket in valid_tickets:
        number = valid_ticket[index]

        if ((start1 <= number) and (number <= end1)) \
                or ((start2 <= number) and (number <= end2)):
            continue
        else:
            return False

    return True


def eliminate(mapping: Dict[str, List[int]], ret: dict):
    for field_name, indexes in mapping.items():
        if len(indexes) == 1:
            index = indexes[0]

            ret[index] = field_name
            new_mapping = copy.deepcopy(mapping)

            new_mapping.pop(field_name)

            for new_indexes in new_mapping.values():
                if index in new_indexes:
                    new_indexes.remove(index)

            eliminate(new_mapping, ret)

    return ret


def get_data():
    with open("data.txt") as file:
        lines: str = file.read()

    rules_dict, your_ticket, nearby_tickets = lines.split("\n\n")

    rules_dict = parse_rules_dict(rules_dict)
    your_ticket = parse_your_ticket(your_ticket)
    nearby_tickets = parse_nearby_tickets(nearby_tickets)

    return rules_dict, your_ticket, nearby_tickets


def parse_rules_dict(rules_dict: str) -> Dict[str, List[int]]:
    ret = {}
    for line in rules_dict.split("\n"):
        key = line.split(": ")[0]
        values = line.split(": ")[1]

        indexes = []

        ranges = values.split(" or ")
        for range in ranges:
            start, end = range.split("-")
            start = int(start)
            end = int(end)

            indexes.extend([start, end])

        ret[key] = indexes

    return ret


def parse_your_ticket(your_ticket: str) -> List[int]:
    your_ticket = your_ticket.split("\n")[1]
    your_ticket = your_ticket.split(",")
    your_ticket = [int(x) for x in your_ticket]
    return your_ticket


def parse_nearby_tickets(nearby_tickets: str) -> List[List[int]]:
    lines = nearby_tickets.split("\n")
    lines = lines[1:]

    ret = []
    for line in lines:
        line = line.split(",")
        line = [int(x) for x in line]
        ret.append(line)

    return ret


def get_bad_numbers(rules_dict: Dict, ticket: List[int]) -> List[int]:
    ret = filter(lambda x: is_bad_number(rules_dict, x), ticket)
    ret = list(ret)
    return ret


def is_bad_number(rules_dict: Dict, number: int) -> bool:
    for _, intervals in rules_dict.items():
        start1, end1, start2, end2 = intervals

        if ((start1<=number) and (number<=end1)) \
                or ((start2<=number) and (number<=end2)):
            return False

    return True


if __name__ == "__main__":
    main()
