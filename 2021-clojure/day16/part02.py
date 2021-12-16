import math

def main():
    binary_string = get_binary_string()
    value, length = compute_value_and_length(binary_string)
    print(value)

def get_binary_string():
    with open("data.txt") as file:
        hex_string = file.read()

    ret = ""
    for char in hex_string:
        ret += hex_char_to_binary(char)
    return ret

def hex_char_to_binary(hex_char):
    map = {
        "0": "0000",
        "1": "0001",
        "2": "0010",
        "3": "0011",
        "4": "0100",
        "5": "0101",
        "6": "0110",
        "7": "0111",
        "8": "1000",
        "9": "1001",
        "A": "1010",
        "B": "1011",
        "C": "1100",
        "D": "1101",
        "E": "1110",
        "F": "1111",
    }
    return map[hex_char]

def compute_value_and_length(packet):
    length = 0
    type_id = int(packet[3:6], 2)
    length += 6

    # literal case
    if type_id == 4:
        value_string = ""

        while True:
            value_string += packet[length+1:length+5]

            if packet[length] == "0":
                length += 5
                break
            else:
                length += 5
        
        value = int(value_string, 2)
        return value, length
    
    # otherwise, build the length, and get the subpacket values
    subpacket_values = []

    if packet[length] == "0":
        length += 1
        subpacket_total_length = int(packet[length:length+15], 2)
        
        length += 15
        stop = length + subpacket_total_length

        while length < stop:
            subpacket = packet[length:]
            subpacket_value, subpacket_length = compute_value_and_length(subpacket)

            subpacket_values.append(subpacket_value)
            length += subpacket_length

    elif packet[length] == "1":
        length += 1
        num_subpackets = int(packet[length:length+11], 2)
        
        length += 11

        for _ in range(num_subpackets):
            subpacket = packet[length:]
            subpacket_value, subpacket_length = compute_value_and_length(subpacket)

            subpacket_values.append(subpacket_value)
            length += subpacket_length

    # now, iterate through different type ids...
    if type_id == 0:
        value = sum(subpacket_values)
    elif type_id == 1:
        value = math.prod(subpacket_values)
    elif type_id == 2:
        value = min(subpacket_values)
    elif type_id == 3:
        value = max(subpacket_values)
    elif type_id == 5:
        value = int(subpacket_values[0] > subpacket_values[1])
    elif type_id == 6:
        value = int(subpacket_values[0] < subpacket_values[1])
    elif type_id == 7:
        value = int(subpacket_values[0] == subpacket_values[1])

    return value, length

if __name__ == "__main__":
    main()
