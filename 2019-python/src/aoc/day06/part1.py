from collections import defaultdict
from functools import cache

type Node = str
type AdjList = defaultdict[Node, list[Node]]


def part1(raw_str: str) -> int:
    adj_list = parse_adj_list(raw_str)

    @cache
    def count_reachable_nodes_from(node: Node) -> int:
        neighbors = adj_list[node]
        return len(neighbors) + sum(
            (count_reachable_nodes_from(neighbor) for neighbor in neighbors),
        )

    all_nodes = tuple(adj_list.keys())
    return sum(
        (count_reachable_nodes_from(node) for node in all_nodes),
    )


def parse_adj_list(raw_str: str) -> AdjList:
    adj_list = defaultdict[Node, list[Node]](list)
    lines = raw_str.split("\n")
    pairs = (line.split(")") for line in lines)
    for to, from_ in pairs:
        adj_list[from_].append(to)
    return adj_list
