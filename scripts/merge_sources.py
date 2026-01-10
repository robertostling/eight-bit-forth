import sys

def read_file(filename):
    with open(filename) as f:
        return f.read().replace('\n', '\r')

sys.stdout.write('\r'.join(map(read_file, sys.argv[1:])))

