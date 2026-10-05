import sys
import pprint
from collections import Counter

import othercompress

PAD_BYTE = 0xff
SUFFIX = []  # ['-->']

def make_block(lines):
    return othercompress.compress((('\r'.join(lines) + '\r').encode('ascii')))


def split_forth(filename, suffix=[], max_raw_size=0x2ff):
    block = []

    def block_size(lines):
        return len(make_block(lines))
        # return sum(map(len, lines)) + len(lines)

    def raw_size(lines):
        return sum(map(len, lines)) + len(lines)

    last_safe = 0
    with open(filename) as f:
        for line in f:
            line = line.rstrip()
            may_split = not line.startswith(' ')
            if may_split:
                last_safe = len(block)
            block.append(line)
            compressed_bytes = block_size(block + suffix)
            raw_bytes = raw_size(block + suffix)
            compressed_ok = compressed_bytes <= 0xff
            raw_ok = raw_bytes <= max_raw_size
            if not (compressed_ok and raw_ok):
                if last_safe == 0:
                    raise ValueError(
                            'Block too large: '
                            f'{raw_bytes}/{compressed_bytes} bytes')
                else:
                    yield block[:last_safe] + suffix
                    block[:last_safe] = []
                    last_safe = 0
    if block:
        yield block + suffix


def symbol_stats(data):
    n_literal = 0
    n_offset = Counter()
    for x in data:
        if x < othercompress.sym_min:
            n_literal += 1
        else:
            offset = x - othercompress.sym_min
            n_offset[offset] += 1
    print(f'{n_literal} literal ({100*n_literal/len(data):.1f}%)')
    # pprint.pp(sorted(n_offset.items()))


def main():
    padded_format = False
    block_idx = 0
    compressed_sizes = []
    raw_sizes = []
    with open('blocks.compressed', 'wb') as outf:
        # TODO: write 4 zero bytes, seek(0) later to write size + #blocks
        # TODO: ...only if padded_format is True
        for filename in sys.argv[1:]:
            try:
                blocks = list(split_forth(filename, suffix=SUFFIX))
                for i, block in enumerate(blocks):
                    is_last_block = (i == len(blocks)-1)
                    if is_last_block and SUFFIX:
                        # remove SUFFIX_WORD from last block
                        block = block[:-1]
                    print('-'*32 + f' {block_idx:04d} ' + '-'*32)
                    raw_size = len('\n'.join(block)) + len(block) + 1
                    compressed_block = make_block(block)
                    print(f'( {raw_size} --> {len(compressed_block)} bytes )')
                    symbol_stats(compressed_block)
                    print('\n'.join(block))
                    compressed_sizes.append(len(compressed_block))
                    raw_sizes.append(raw_size)
                    if padded_format:
                        compressed_block = compressed_block + bytes(
                                [PAD_BYTE]*(0x100-len(compressed_block)))
                    else:
                        block_idx_bytes = block_idx.to_bytes(2, 'little')
                        block_size_bytes = len(compressed_block).to_bytes(
                                1, 'little')
                        compressed_block = (block_idx_bytes
                                            + block_size_bytes
                                            + compressed_block)
                    outf.write(compressed_block)
                    block_idx += 1
            except ValueError as e:
                print(f'Failure at block {block_idx}: {e}')
                sys.exit(1)

        if not padded_format:
            outf.write((0xffff).to_bytes(2, 'little'))

    print('Overall compressed size: '
          f'{100*sum(compressed_sizes)/sum(raw_sizes):.1f}')
    print(f'Compressed: {sum(compressed_sizes)} bytes')
    print(f'Original: {sum(raw_sizes)} bytes')

if __name__ == '__main__':
    main()

