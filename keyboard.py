import re
import argparse

def decode_hex(s):
    return bytes([int(part.strip()[1:], 16) for part in s.split(',')])

# https://sta.c64.org/cbm64kbdlay.html

standard_table = decode_hex('''
$14,$0D,$1D,$88,$85,$86,$87,$11,
$33,$57,$41,$34,$5A,$53,$45,$01,
$35,$52,$44,$36,$43,$46,$54,$58,
$37,$59,$47,$38,$42,$48,$55,$56,
$39,$49,$4A,$30,$4D,$4B,$4F,$4E,
$2B,$50,$4C,$2D,$2E,$3A,$40,$2C,
$5C,$2A,$3B,$13,$01,$3D,$5E,$2F,
$31,$5F,$04,$32,$20,$02,$51,$03''')

shift_table = decode_hex('''
$94,$8D,$9D,$8C,$89,$8A,$8B,$91,
$23,$D7,$C1,$24,$DA,$D3,$C5,$01,
$25,$D2,$C4,$26,$C3,$C6,$D4,$D8,
$27,$D9,$C7,$28,$C2,$C8,$D5,$D6,
$29,$C9,$CA,$30,$CD,$CB,$CF,$CE,
$DB,$D0,$CC,$DD,$3E,$5B,$BA,$3C,
$A9,$C0,$5D,$93,$01,$3D,$DE,$3F,
$21,$5F,$04,$22,$A0,$02,$D1,$83''')

def petscii_to_ascii(x):
    if isinstance(x, bytes):
        return bytes([petscii_to_ascii(n) for n in x])
    if 0x41 <= x <= 0x5a:
        return x + 0x20
    elif 0xc0 <= x <= 0xda:
        return x - 0x80
    elif 0xdb <= x <= 0xde:
        return x - 0x60
    return x

def ascii_to_petscii(x):
    if isinstance(x, bytes):
        return bytes([ascii_to_petscii(n) for n in x])
    if 0x41 <= x <= 0x5a:
        return x + 0x80
    elif 0x61 <= x <= 0x7a:
        return x - 0x20
    elif 0x7b <= x <= 0x7e:
        return x + 0x60
    return x

def make_dvorak(table):
    qwerty = ( b'qwertyuiopasdfghjkl:;zxcvbnm,./'
               b'QWERTYUIOPASDFGHJKL[]ZXCVBNM<>?')
    dvorak = ( b"',.pyfgcrlaoeuidhtns-;qjkxbmwvz"
               b'"<>PYFGCRLAOEUIDHTNS_:QJKXBMWVZ')
    tr = bytes.maketrans(ascii_to_petscii(qwerty), ascii_to_petscii(dvorak))
    # assert ascii_to_petscii(qwerty).translate(tr) == ascii_to_petscii(dvorak)
    return table.translate(tr)

def petscii_to_char(x):
    if 0x20 <= x < 0x7f or 0xc0 <= x < 0xe0:
        return ' ' + chr(petscii_to_ascii(x)) + ' '
    return f'${x:02x}'

def print_table(f, table, name):
    print(name, file=f)
    for row in range(8):
        print(' '.join(f'[{petscii_to_char(x)}]'
                       for x in table[8*row:8*(row+1)]),
              file=f)
    print(file=f)

def read_table(f):
    re_entry = re.compile(r'\[(?: (.) |\$([0-9a-z][0-9a-z]))\]')
    def parse_key(m):
        if m.group(1).startswith('$'):
            return int(m.group(1)[1:], 16)
        return ascii_to_petscii(ord(m.group(1)[1]))

    for row in range(8):
        line = next(f)
        values = list(map(parse_key, re_entry.finditer()))
        print(values)

def read_tables(filename):
    tables = {}
    with open(filename) as f:
        for line in f:
            if not (name := line.strip()):
                tables[name] = read_table(f)
    return tables

def main():
    parser = argparse.ArgumentParser(
        description='Convert Commodore keymaps between text and binary formats')

    parser.add_argument('-i', '--input',
                        help='Input file, format determined by suffix (.bin/.txt)')
    parser.add_argument('-o', '--output',
                        help='Output file, format determined by suffix (.bin/.txt)')
    parser.add_argument('-p', '--pad', action='store_true',
                        help='Pad binary output with $ff bytes to page')
    parser.add_argument('-d', '--dvorak', action='store_true',
                        help='Automatically attempt translation to dvorak')
    args = parser.parse_args()

    tables = dict(standard=standard_table, shift=shift_table)
    if args.dvorak:
        tables = {k: make_dvorak(table) for k, table in tables.items()}

    if (not args.input) and args.output and args.output.endswith('.txt'):
        with open(args.output, 'w') as f:
            for name in ['standard', 'shift']:
                print_table(f, tables[name], name)
    elif args.output and args.output.endswith('.bin'):
        if args.input and args.input.endswith('.txt'):
            tables = read_tables(args.input)
        elif args.input:
            print(f'Unknown input suffix: {args.input}')
            sys.exit(1)
        data = b''
        data += tables['standard'] + bytes([0xff])
        if 'shift' in tables:
            data += tables['shift'] + bytes([0xff])
        if args.pad:
            data += bytes([0xff]*(0x100 - len(data)))
        with open(args.output, 'wb') as f:
            f.write(data)
    else:
        print('Invalid configuration of input/output files')

if __name__ == '__main__':
    main()
