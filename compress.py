import sys
from collections import Counter
from operator import itemgetter

PETSCII_MIN = 0x0d
PETSCII_MAX = 0x5f
LEN_BITS = 3
MIN_LENGTH = 3

def compress(data):
    len_mask = (1 << LEN_BITS) - 1
    sym_min = PETSCII_MAX + 1 - PETSCII_MIN
    assert min(data) >= PETSCII_MIN
    assert max(data) <= PETSCII_MAX
    def shift_byte(x):
        assert PETSCII_MIN <= x <= PETSCII_MAX
        return x - PETSCII_MIN
    def shift_bytes(xs):
        return bytes(map(shift_byte, xs))
    max_offset = ((0x100 - sym_min) << (7 - LEN_BITS)) - 1
    # max_offset = 0x800-1
    i = 0
    max_length = (1 << LEN_BITS) + MIN_LENGTH - 1
    # print(f"len_mask = {len_mask:02x}")
    # print(f"max length = {max_length}")
    # print(f"max offset = {max_offset}")
    length_counts = Counter()
    compressed = []
    most_recent = [{} for _ in range(max_length+1)]

    def add_substrings(pos):
        for length in range(MIN_LENGTH, min(max_length, pos) + 1):
            substring = data[pos-length:pos]
            most_recent[length][substring] = pos-length

    while i < len(data):
        add_substrings(i)

        best_length = None
        best_offset = None
        for length in range(MIN_LENGTH, min(max_length, len(data)-i)+1):
            upcoming = data[i:i+length]
            if upcoming in most_recent[length]:
                offset = (i - most_recent[length][upcoming])
                if offset <= max_offset:
                    best_length = length
                    best_offset = offset
                # else:
                #     print('TOO FAR:', length, offset, upcoming)
        if best_offset is None:
            compressed.append(shift_byte(data[i]))
            length_counts[1] += 1
            # print(i, data[i:i+1])
            i += 1
        else:
            assert MIN_LENGTH <= best_offset <= max_offset
            assert MIN_LENGTH <= best_length <= max_length
            offset_high = (best_offset >> 8) & 0xff
            compressed.append(
                    ((best_length - MIN_LENGTH) & len_mask) +
                    (offset_high << LEN_BITS) +
                    sym_min)
            compressed.append(best_offset & 0xff)
            # print(i, best_offset, best_length,
            #       data[i-offset:i-offset+best_length])
            length_counts[best_length] += 1
            for j in range(i+1, i+best_length):
                add_substrings(j)
            i += best_length
    print(sorted(length_counts.items()))
    return bytes(compressed)


def uncompress(compressed):
    sym_min = PETSCII_MAX + 1 - PETSCII_MIN
    offset_mask = 0xff ^ (1 << (8 - LEN_BITS))
    output = []
    i = 0
    while i < len(compressed):
        if compressed[i] < sym_min:
            output.append(compressed[i] + PETSCII_MIN)
            i += 1
        else:
            # print('CODE:', compressed[i], compressed[i+1])
            high = compressed[i] - sym_min
            length = (high & ((1 << LEN_BITS) - 1)) + MIN_LENGTH;
            offset_low = compressed[i+1]
            offset_high = high >> LEN_BITS
            offset = (offset_high << 8) | offset_low
            for j in range(length):
                output.append(output[-(offset)])
            i += 2
    return bytes(output)


def main():
    input_path, output_path = sys.argv[1:]
    with open(input_path, 'rb') as f:
        data = f.read()
    # print(min(data), max(data))
    compressed = compress(data)
    print(f"{len(data)} -> {len(compressed)} "
          f"({100.0*len(compressed)/len(data):.1f}%)")
    uncompressed = uncompress(compressed)
    print(f"-> {len(uncompressed)}")
    print(f"VERIFIED: {uncompressed == data}")
    # print(uncompressed[-40:])

    if uncompressed == data:
        with open(output_path, 'wb') as f:
            f.write(compressed)


if __name__ == '__main__':
    main()


