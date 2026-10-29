import argparse


def decode_hex(s):
    return [int(part.strip()[1:], 16) for part in s.split(',')]

# https://sta.c64.org/cbm64kbdlay.html

default_normal = decode_hex('''
$14,$0D,$1D,$88,$85,$86,$87,$11,
$33,$57,$41,$34,$5A,$53,$45,$01,
$35,$52,$44,$36,$43,$46,$54,$58,
$37,$59,$47,$38,$42,$48,$55,$56,
$39,$49,$4A,$30,$4D,$4B,$4F,$4E,
$2B,$50,$4C,$2D,$2E,$3A,$40,$2C,
$5C,$2A,$3B,$13,$01,$3D,$5E,$2F,
$31,$5F,$04,$32,$20,$02,$51,$03''')

default_shift = decode_hex('''
$94,$8D,$9D,$8C,$89,$8A,$8B,$91,
$23,$D7,$C1,$24,$DA,$D3,$C5,$01,
$25,$D2,$C4,$26,$C3,$C6,$D4,$D8,
$27,$D9,$C7,$28,$C2,$C8,$D5,$D6,
$29,$C9,$CA,$30,$CD,$CB,$CF,$CE,
$DB,$D0,$CC,$DD,$3E,$5B,$BA,$3C,
$A9,$C0,$5D,$93,$01,$3D,$DE,$3F,
$21,$5F,$04,$22,$A0,$02,$D1,$83''')

key_order = '''
Insert/Delete Return Left/Right F7 F1 F3 F5 Up/Down
3 W A 4 Z S E LeftShift
5 R D 6 C F T X
7 Y G 8 B H U V
9 I J 0 M K O N
+ P L - . : @ ,
Pound * ; Clear/Home RightShift = ^ /
1 LeftArrow Control 2 Space Commodore Q Run/Stop'''.split()

key_index = {key: i for i, key in enumerate(key_order)}

tables = {
    'dvorak': {
        'Q': ("'", '"'),
        'W': (',', '<'),
        'E': ('.', '>'),
        'R': ('p', 'P'),
        'T': ('y', 'Y'),
        'Y': ('f', 'F'),
        'U': ('g', 'G'),
        'I': ('c', 'C'),
        'O': ('r', 'R'),
        'P': ('l', 'L'),
        '@': ('/', '?'),
        '*': ('=', '+'),
        'A': ('a', 'A'),
        'S': ('o', 'O'),
        'D': ('e', 'E'),
        'F': ('u', 'U'),
        'G': ('i', 'I'),
        'H': ('d', 'D'),
        'J': ('h', 'H'),
        'K': ('t', 'T'),
        'L': ('n', 'N'),
        ':': ('s', 'S'),
        ';': ('-', '_'),
        '=': ('=', '|'),
        'Z': (';', ':'),
        'X': ('q', 'Q'),
        'C': ('j', 'J'),
        'V': ('k', 'K'),
        'B': ('x', 'X'),
        'N': ('b', 'B'),
        'M': ('m', 'M'),
        ',': ('w', 'W'),
        '.': ('v', 'V'),
        '/': ('z', 'Z'),
        '^': ('^', '*'),
        '2': ('2', '@'),
        }
    }

def petscii_to_ascii(x):
    if 0x41 <= x <= 0x5a:
        return x + 0x20
    elif 0xc0 <= x <= 0xda:
        return x - 0x80
    elif 0xdb <= x <= 0xde:
        return x - 0x60
    return x

def ascii_to_petscii(x):
    if 0x41 <= x <= 0x5a:
        return x + 0x80
    elif 0x61 <= x <= 0x7a:
        return x - 0x20
    elif 0x7b <= x <= 0x7e:
        return x + 0x60
    return x

def generate_keymap(table):
    shift = default_shift.copy()
    normal = default_normal.copy()
    for key_name, (v_normal, v_shift) in table.items():
        c_normal = ascii_to_petscii(ord(v_normal))
        c_shift = ascii_to_petscii(ord(v_shift))
        index = key_index[key_name]
        shift[index] = c_shift
        normal[index] = c_normal
    return normal, shift


def main():
    parser = argparse.ArgumentParser(
        description='Create Commodore keymaps')

    parser.add_argument('-o', '--output',
                        help='Output file, format determined by suffix (.bin/.txt)')
    parser.add_argument('-k', '--keymap', choices=list(tables.keys()),
                        help='Keymap to generate')
    parser.add_argument('-p', '--pad', action='store_true')
    args = parser.parse_args()

    keymap = generate_keymap(tables[args.keymap])

    data = b''.join(bytes(section + [0xff]) for section in keymap)
    if args.pad:
        data += bytes([0xff]*(0x100 - len(data)))
    with open(args.output, 'wb') as f:
        f.write(data)

if __name__ == '__main__':
    main()
