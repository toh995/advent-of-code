from collections import defaultdict, deque

type Node = str
type AdjList = defaultdict[Node, list[Node]]


def part2(raw_str: str) -> int:
    adj_list = parse_adj_list(raw_str)

    assert len(adj_list["YOU"]) == 1
    assert len(adj_list["SAN"]) == 1
    START_NODE = adj_list["YOU"][0]
    DEST_NODE = adj_list["SAN"][0]

    # Stores pairs
    # (curr_node, num_moves)
    queue = deque([(START_NODE, 0)])
    seen = set[Node]()
    while queue:
        curr_node, num_moves = queue.popleft()
        if curr_node == DEST_NODE:
            return num_moves
        if curr_node in seen:
            continue
        seen.add(curr_node)
        neighbors = adj_list[curr_node]
        for neighbor in neighbors:
            queue_item = (neighbor, num_moves + 1)
            queue.append(queue_item)
    raise ValueError("could not find path")


def parse_adj_list(raw_str: str) -> AdjList:
    adj_list = defaultdict[Node, list[Node]](list)
    lines = raw_str.split("\n")
    pairs = (line.split(")") for line in lines)
    for u, v in pairs:
        adj_list[v].append(u)
        adj_list[u].append(v)
    return adj_list
