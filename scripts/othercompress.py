import sys
from collections import Counter
from operator import itemgetter

PETSCII_MIN = 0x0d
PETSCII_MAX = 0x5f
# LEN_BITS = 3
# MIN_LENGTH = 3

def compress(data):
    sym_min = PETSCII_MAX + 1 - PETSCII_MIN
    assert min(data) >= PETSCII_MIN
    assert max(data) <= PETSCII_MAX
    def shift_byte(x):
        assert PETSCII_MIN <= x <= PETSCII_MAX
        return x - PETSCII_MIN
    def shift_bytes(xs):
        return bytes(map(shift_byte, xs))

    symbols = shift_bytes(data)
    max_offset = 0x100 - sym_min

    generated = []

    def decode(pos, top=True):
        # print(f'decoding {pos} given {len(generated)} generated, {len(symbols)} symbols')
        code = generated[pos]
        if code < sym_min:
            return [code]
        else:
            offset = code - sym_min
            return decode(pos - (offset + 2)) + decode(pos - (offset + 1))

    for i in range(len(symbols)):
        options = []
        for offset in range(1, min(i, max_offset)):
            decoded = decode(i - offset)
            print(f'{i}: DECODE "{decoded}" at offset {offset}')
            if len(decoded) > 1 and bytes(decoded) == symbols[i:i+len(decoded)]:
                options.append((len(decoded), offset, decoded))
        if options:
            length, offset, decoded = max(options)
            code = sym_min + offset
            print(f'best: "{decoded}" at offset {offset}')
        else:
            code = symbols[i]
            print(f'symbol: {code}')
        generated.append(code)


def main():
    input_path = sys.argv[1]
    with open(input_path, 'rb') as f:
        data = f.read()
    # print(min(data), max(data))
    compressed = compress(data)
    #print(f"{len(data)} -> {len(compressed)} "
    #      f"({100.0*len(compressed)/len(data):.1f}%)")
    #uncompressed = uncompress(compressed)
    #print(f"-> {len(uncompressed)}")
    #print(f"VERIFIED: {uncompressed == data}")

    #if uncompressed == data:
    #    with open(output_path, 'wb') as f:
    #        f.write(compressed)


if __name__ == '__main__':
    main()


