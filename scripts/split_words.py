import sys
import othercompress


def make_block(lines):
    return othercompress.compress('\r'.join(lines).encode('ascii'))


def split_forth(filename):
    block = []

    def block_size(lines):
        return len(make_block(lines))
        # return sum(map(len, lines)) + len(lines)

    last_safe = 0
    with open(filename) as f:
        for line in f:
            line = line.rstrip()
            may_split = not line.startswith(' ')
            if may_split:
                last_safe = len(block)
            block.append(line)
            if block_size(block) >= 256:
                if last_safe == 0:
                    print(f'BLOCK TOO LARGE: {block_size(block)} bytes')
                else:
                    yield block[:last_safe]
                    block[:last_safe] = []
                    last_safe = 0
    if block:
        yield block


import pprint
from collections import Counter

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
    block_idx = 0
    compressed_sizes = []
    raw_sizes = []
    for filename in sys.argv[1:]:
        for block in split_forth(filename):
            print('-'*32 + f' {block_idx:04d} ' + '-'*32)
            raw_size = len('\n'.join(block)) + len(block) + 1
            compressed_block = make_block(block)
            print(f'( {raw_size} --> {len(compressed_block)} bytes )')
            symbol_stats(compressed_block)
            # print('\n'.join(block))
            compressed_sizes.append(len(compressed_block))
            raw_sizes.append(raw_size)
            block_idx += 1

    print('Overall compressed size: '
          f'{100*sum(compressed_sizes)/sum(raw_sizes):.1f}')
    print(f'Compressed: {sum(compressed_sizes)} bytes')
    print(f'Original: {sum(raw_sizes)} bytes')

if __name__ == '__main__':
    main()

