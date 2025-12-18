import sys

with open(sys.argv[1]) as f, open(sys.argv[2], 'wb') as outf:
    outf.write(bytes([0x00, 0x80]))
    outf.write(f.read().replace('\n', '\r').encode('ascii'))
