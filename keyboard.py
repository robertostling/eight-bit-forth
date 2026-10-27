def decode_hex(s):
    return bytes([int(part.strip()[1:], 16) for part in s.split(',')])

standard_table = decode_hex('''
$14,$0D,$1D,$88,$85,$86,$87,$11,
$33,$57,$41,$34,$5A,$53,$45,$01,
$35,$52,$44,$36,$43,$46,$54,$58,
$37,$59,$47,$38,$42,$48,$55,$56,
$39,$49,$4A,$30,$4D,$4B,$4F,$4E,
$2B,$50,$4C,$2D,$2E,$3A,$40,$2C,
$5C,$2A,$3B,$13,$01,$3D,$5E,$2F,
$31,$5F,$04,$32,$20,$02,$51,$03,
$FF''')

shift_table = decode_hex('''
$94,$8D,$9D,$8C,$89,$8A,$8B,$91,
$23,$D7,$C1,$24,$DA,$D3,$C5,$01,
$25,$D2,$C4,$26,$C3,$C6,$D4,$D8,
$27,$D9,$C7,$28,$C2,$C8,$D5,$D6,
$29,$C9,$CA,$30,$CD,$CB,$CF,$CE,
$DB,$D0,$CC,$DD,$3E,$5B,$BA,$3C,
$A9,$C0,$5D,$93,$01,$3D,$DE,$3F,
$21,$5F,$04,$22,$A0,$02,$D1,$83,
$FF''')

def make_dvorak(table):
    qwerty = ( b'qwertyuiop@*asdfghjkl:;=zxcvbnm,./'
               b'QWERTYUIOP@*ASDFGHJKL()=ZXCVBNM<>?')
    dvorak = ( b"',.pyfgcrl/=aoeuidhtns-\\;qjkxbmwvz"
               b'"<>PYFGCRL?+AOEUIDHTNS_|:QJKXBMWVZ')
    assert len(qwerty) == len(dvorak)
    tr = bytes.maketrans(qwerty, dvorak)
    return table.translate(tr)

def main():
    with open('dvorak.bin', 'wb') as f:
        f.write(make_dvorak(standard_table))
        f.write(make_dvorak(shift_table))
        f.write(bytes([0xff]*(0x100 - f.tell())))

if __name__ == '__main__':
    main()
