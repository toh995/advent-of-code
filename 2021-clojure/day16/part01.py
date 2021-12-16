def main():
    binary_string = get_binary_string()
    version_sum, length = compute_version_sum_and_length(binary_string)
    print(version_sum)

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

def compute_version_sum_and_length(packet):
    version_sum = 0
    length = 0

    version = int(packet[:3], 2)
    type_id = int(packet[3:6], 2)

    version_sum += version
    length += 6

    if type_id == 4:
        while True:
            if packet[length] == "0":
                length += 5
                break
            else:
                length += 5
    
    else:
        if packet[length] == "0":
            length += 1
            subpacket_total_length = int(packet[length:length+15], 2)
            
            length += 15
            stop = length + subpacket_total_length

            while length < stop:
                subpacket = packet[length:]
                subpacket_sum, subpacket_length = compute_version_sum_and_length(subpacket)

                version_sum += subpacket_sum
                length += subpacket_length

        elif packet[length] == "1":
            length += 1
            num_subpackets = int(packet[length:length+11], 2)
            
            length += 11

            for _ in range(num_subpackets):
                subpacket = packet[length:]
                subpacket_sum, subpacket_length = compute_version_sum_and_length(subpacket)

                version_sum += subpacket_sum
                length += subpacket_length

    return version_sum, length

if __name__ == "__main__":
    main()
