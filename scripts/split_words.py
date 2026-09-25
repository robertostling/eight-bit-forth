import sys
from othercompress import compress


def make_block(lines):
    return compress('\r'.join(lines).encode('ascii'))


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


def main():
    filename = sys.argv[1]
    for block_idx, block in enumerate(split_forth(filename)):
        print('-'*32 + f' {block_idx:04d} ' + '-'*32)
        raw_size = len('\n'.join(block)) + len(block)
        print(f'( {raw_size} --> {len(make_block(block))} bytes )')
        print('\n'.join(block))


if __name__ == '__main__':
    main()

