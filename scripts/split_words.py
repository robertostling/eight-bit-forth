import sys
import argparse
import os.path

import othercompress

PAD_BYTE = 0xff

# Test with all blocks compressed individually
# beam size 32: Overall 28972 -> 16785 bytes (57.9%)
# greedy search: Overall 28972 -> 17187 bytes (59.3%)
# one of each length in beam, up to 8: Overall 28972 -> 16875 bytes (58.2%)

class Block:
    def __init__(self, category, source, lineno=None):
        self.category = category
        self.source = source
        self.lineno = lineno
        self.lines = []
        self.compressed = None

    def reference(self):
        return f'{self.source}:{self.lineno}'

    def __str__(self):
        return '\n'.join(self.lines)

    def ascii(self):
        return ('\r'.join(self.lines) + '\r').encode('ascii')

    def compress(self, beam_size=1):
        if self.compressed is None:
            self.compressed = othercompress.compress_beam(self.ascii(), beam_size)
        return self.compressed

    def append(self, line):
        self.lines.append(line.rstrip())
        self.compressed = None


def main():
    parser = argparse.ArgumentParser(
        description='Encode source files as compressed blocks')
    parser.add_argument('-o', '--output',
                        help='File to write block data to')
    parser.add_argument('-f', '--format',
                        choices=['merged'], default='merged',
                        help='Format of the output data file')
    parser.add_argument('--beam-size', default=1, type=int,
                        help='Beam size in compression')
    parser.add_argument('-d', '--include-docs',
                        action='store_true',
                        help='Include blocks marked with "# doc" in output')
    parser.add_argument('inputs', nargs='+')
    args = parser.parse_args()

    blocks = []
    block = None
    for filename in args.inputs:
        name = os.path.basename(filename)
        with open(filename) as f:
            for lineno, line in enumerate(f):
                if line.strip() == '# code':
                    block = Block('code', name, lineno=lineno+1)
                    blocks.append(block)
                elif line.strip() == '# doc':
                    block = Block('doc', name, lineno=lineno+1)
                    blocks.append(block)
                else:
                    if block is None:
                        print(f'{name}:{lineno} lacks #code or #doc')
                    length = len(line.rstrip())
                    if length > 39:
                        print(f'{name}:{lineno+1} is {length} bytes')
                    block.append(line)
            block = None

    output_blocks = [block for block in blocks
                     if args.include_docs or block.category == 'code']

    total_ascii = 0
    total_compressed = 0
    for idx, block in enumerate(output_blocks):
        ascii = block.ascii()
        compressed = block.compress(args.beam_size)
        percent = 100*len(compressed)/len(ascii)
        print(f'{block.reference()} ${idx:x}: '
              f'{len(ascii)} -> {len(compressed)} bytes '
              f'({percent:.1f}%)')
        print(f'    {block.lines[0]}')
        if len(ascii) > 0x2ff or len(compressed) > 0xfe:
            print(block)
            print(f'Invalid size, aborting.')
            sys.exit(1)
        total_ascii += len(ascii)
        total_compressed += len(compressed)

    percent = 100*total_compressed/total_ascii
    print(f'Overall {total_ascii} -> {total_compressed} bytes '
          f'({percent:.1f}%)')

    if args.format == 'merged':
        with open(args.output, 'wb') as outf:
            for idx, block in enumerate(output_blocks):
                idx_bytes = idx.to_bytes(2, 'little')
                compressed = block.compress()
                size_bytes = len(compressed).to_bytes(1, 'little')
                data = (idx_bytes + size_bytes + compressed)
                outf.write(data)
            outf.write((0xffff).to_bytes(2, 'little'))
    else:
        raise NotImplementedError()

if __name__ == '__main__':
    main()

