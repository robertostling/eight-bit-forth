# Utility to make .prg files from text files, for uploading sources to .d64
# images.

import sys

adr = int(sys.argv[1], 16)
infile, outfile = sys.argv[2:]

with open(infile) as f, open(outfile, 'wb') as outf:
    outf.write(bytes([0x00, 0x80]))
    outf.write(f.read().replace('\n', '\r').encode('ascii'))
