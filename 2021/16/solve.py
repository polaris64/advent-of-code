from math import prod

EXAMPLES = [
    [ # Part 1
        ("8A004A801A8002F478", 16),
        ("620080001611562C8802118E34", 12),
        ("C0015000016115A2E0802F182340", 23),
        ("A0016C880162017C3686B18A3D4780", 31),
    ],
    [ # Part 2
        ("C200B40A82", 3),
        ("04005AC33890", 54),
        ("880086C3E88112", 7),
        ("CE00C43D881120", 9),
        ("D8005AC2A8F0", 1),
        ("F600BC2D8F", 0),
        ("9C005AC2F8F0", 0),
        ("9C0141080250320F1802104A08", 1),
    ]
]

def read_input(filename):
    with open(filename, "rt") as fh:
        return fh.readline().strip()

def decode_literal(p_bin):
    res = ""
    while True:
        group = p_bin[:5]
        res += group[1:]
        p_bin = p_bin[5:]
        if group[0] == "0":
            break
    return (int(res, 2), p_bin)

def decode_operator(p_bin):
    ltid = p_bin[0]
    p_bin = p_bin[1:]
    packets = []
    if ltid == "0": # Next 15 bits are the total length in bits of payload
        payload_size = int(p_bin[:15], 2)
        p_bin = p_bin[15:]
        len_start = len(p_bin)
        rem = p_bin
        while len_start - len(rem) < payload_size:
            packet, rem = parse_packet(rem)
            packets.append(packet)
        p_bin = rem
    else: # Next 11 bits is number of sub-packets in payload
        num_packets = int(p_bin[:11], 2)
        p_bin = p_bin[11:]
        rem = p_bin
        for _ in range(num_packets):
            packet, rem = parse_packet(rem)
            packets.append(packet)
        p_bin = rem
    return (packets, p_bin)

def parse_header(p_bin):
    return (
        {
            "v": int(p_bin[0:3], 2),
            "t": int(p_bin[3:6], 2),
        },
        p_bin[6:],
    )

def parse_packet(p_bin):
    header, rem = parse_header(p_bin)
    payload = None
    if header["t"] == 4: # Literal value
        payload, rem = decode_literal(rem)
    else: # Operator packet
        payload, rem = decode_operator(rem)
    return (
        {
            "h": header,
            "p": payload,
        },
        rem
    )

def convert_packet(packet_hex):
    return "".join([bin(int(x, 16))[2:].rjust(4, "0") for x in list(packet_hex)])

def sum_versions(packet):
    res = packet["h"]["v"]
    if isinstance(packet["p"], list):
        res += sum([sum_versions(x) for x in packet["p"]])
    return res

def eval_packet(packet):
    t = packet["h"]["t"]
    if t == 4:
        return packet["p"]
    else:
        if t == 0: # SUM
            return sum([eval_packet(p) for p in packet["p"]])
        if t == 1: # PRODUCT
            return prod([eval_packet(p) for p in packet["p"]])
        if t == 2: # MIN
            return min([eval_packet(p) for p in packet["p"]])
        if t == 3: # MAX
            return max([eval_packet(p) for p in packet["p"]])
        if t == 5: # GREATER-THAN
            return 1 if eval_packet(packet["p"][0]) > eval_packet(packet["p"][1]) else 0
        if t == 6: # LESS-THAN
            return 1 if eval_packet(packet["p"][0]) < eval_packet(packet["p"][1]) else 0
        if t == 7: # EQUAL-TO
            return 1 if eval_packet(packet["p"][0]) == eval_packet(packet["p"][1]) else 0
        return None

def solve_p1(inp):
    return sum_versions(parse_packet(convert_packet(inp))[0])

def solve_p2(inp):
    return eval_packet(parse_packet(convert_packet(inp))[0])

def main():
    inp = read_input("input.txt")
    print("The solution to part 1 is: {}".format(solve_p1(inp)))
    print("The solution to part 2 is: {}".format(solve_p2(inp)))

def test_main():
    inp = read_input("input.txt")
    assert solve_p1(inp) == 860
    assert solve_p2(inp) == 470949537659

def test_ex():
    for p_hex, s in EXAMPLES[0]:
        assert solve_p1(p_hex) == s
    for p_hex, s in EXAMPLES[1]:
        assert solve_p2(p_hex) == s

if __name__ == '__main__':
    main()
