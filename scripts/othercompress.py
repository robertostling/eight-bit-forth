import sys
from collections import Counter
from operator import itemgetter

PETSCII_MIN = 0x0d
PETSCII_MAX = 0x5f
# LEN_BITS = 3
# MIN_LENGTH = 3
sym_min = PETSCII_MAX + 1 - PETSCII_MIN
max_offset = 0x100 - sym_min

def decompress_at(data, i):
    if data[i] < sym_min:
        return [data[i] + PETSCII_MIN]
    offset = data[i] - sym_min
    return decompress_at(data, i-(offset+2)) + decompress_at(data, i-(offset+1))

def decompress(data):
    output = []
    for i in range(len(data)):
        output.extend(decompress_at(data, i))
    return bytes(output)


def compress(data):
    pos_str = [(1, 0), (1, 1)]
    i = 2
    output = [data[0]-PETSCII_MIN, data[1]-PETSCII_MIN]
    while i < len(data):
        best_length = 1
        best_offset = None
        for offset in range(min(len(pos_str)-1, max_offset)):
            enc_length1, enc_i = pos_str[-(offset+2)]
            enc_length2, _ = pos_str[-(offset+1)]
            enc_length = enc_length1 + enc_length2
            enc_str = data[enc_i:enc_i+enc_length]
            if enc_length > 1 and enc_str == data[i:i+enc_length]:
                if enc_length > best_length:
                    best_length = enc_length
                    best_offset = offset
        pos_str.append((best_length, i))
        if best_length == 1:
            output.append(data[i] - PETSCII_MIN)
        else:
            output.append(best_offset + sym_min)
        i += best_length
    return bytes(output)

def main():
    input_path = sys.argv[1]
    with open(input_path, 'rb') as f:
        data = f.read()
    # print(min(data), max(data))
    compressed = compress(data)
    print(f"{len(data)} -> {len(compressed)} "
          f"({100.0*len(compressed)/len(data):.1f}%)")
    uncompressed = decompress(compressed)
    print(f"-> {len(uncompressed)}")
    print(f"VERIFIED: {uncompressed == data}")
    print(str(uncompressed, 'utf-8').replace('\r', '\n'))

    #if uncompressed == data:
    #    with open(output_path, 'wb') as f:
    #        f.write(compressed)


if __name__ == '__main__':
    main()


