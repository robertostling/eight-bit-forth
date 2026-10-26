import sys
from collections import Counter
from operator import itemgetter

PETSCII_MIN = 0x1f
PETSCII_MAX = 0x7e
sym_min = PETSCII_MAX + 1 - PETSCII_MIN
max_offset = 0xff - sym_min
# max_offset = 0x40

# with max: 16876 -> 9245
# with 0x40: ... -> 10576

def is_atom(x):
    return x < sym_min

def to_atom(x):
    assert 0 <= x < sym_min
    if x == 0:
        return 0x0d
    return PETSCII_MIN + x

def from_atom(x):
    assert (x == 0xd) or (0x20 <= x <= 0x7e)
    if x == 0xd:
        return 0
    else:
        return x - PETSCII_MIN

def to_offset(x):
    return x - sym_min

def from_offset(x):
    assert 0 <= x <= max_offset
    return x + sym_min


def decompress_at(data, i):
    if is_atom(data[i]):
        return [to_atom(data[i])]
    offset = to_offset(data[i])
    return decompress_at(data, i-(offset+2)) + decompress_at(data, i-(offset+1))

def decompress(data):
    output = []
    for i in range(len(data)):
        output.extend(decompress_at(data, i))
    return bytes(output)


def compress(data):
    pos_str = [(1, 0), (1, 1)]
    i = 2
    output = [from_atom(data[0]), from_atom(data[1])]
    while i < len(data):
        best_length = 1
        best_offset = None
        for offset in reversed(range(min(len(pos_str)-1, max_offset))):
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
            output.append(from_atom(data[i]))
        else:
            output.append(from_offset(best_offset))
        i += best_length
    return bytes(output)


def compress_beam(data, beam_size, max_offset=159):
    if beam_size == 1:
        return compress(data)

    """Compress using beam search to find better encodings than greedy search"""
    def encode_literal(c):
        if c == 0x0d:
            return 0
        elif 0x20 <= c <= 0x7e:
            return c - 0x1f
        else:
            raise ValueError(f'Unencodable symbol with value ${c:x}')

    def encode_offset(offset):
        return 0x60 + offset

    beam = [(2,
             [encode_literal(data[0]), encode_literal(data[1])],
             [data[0:1], data[1:2]])]

    while True:
        candidates = []
        for total_length, encoded, sequences in beam:
            partial_candidates = {
                total_length+1: (encode_literal(data[total_length]),
                                 sequences + [data[total_length:total_length+1]])
            }
            for offset in range(min(len(sequences)-1, max_offset)):
                sequence = sequences[-(offset+2)] + sequences[-(offset+1)]
                length = total_length + len(sequence)
                if length not in partial_candidates and sequence == data[total_length:length]:
                    partial_candidates[length] = (encode_offset(offset), sequences + [sequence])
            for length, (symbol, merged_sequences) in partial_candidates.items():
                if length == len(data):
                    return bytes(encoded + [symbol])
                candidates.append((length, encoded + [symbol], sequences + [data[total_length:length]]))
        candidates.sort(reverse=True)
        beam = candidates[:beam_size]



def main():
    input_path, output_path = sys.argv[1:]
    with open(input_path, 'rb') as f:
        data = f.read().replace(b'\n', b'\r')
    # print(min(data), max(data))
    compressed = compress_beam(data, 1)
    print(f"{len(data)} -> {len(compressed)} "
          f"({100.0*len(compressed)/len(data):.1f}%)")
    uncompressed = decompress(compressed)
    print(f"-> {len(uncompressed)}")
    print(f"VERIFIED: {uncompressed == data}")
    # print(str(uncompressed, 'utf-8').replace('\r', '\n'))

    padded = compressed + bytes([0xff]*(0x100 - len(compressed)))
    if len(padded) != 0x100:
        print(f'WARNING: padded length = ${len(padded):x}')

    with open(output_path, 'wb') as f:
        f.write(padded)

    #if uncompressed == data:
    #    with open(output_path, 'wb') as f:
    #        f.write(compressed)


if __name__ == '__main__':
    main()


