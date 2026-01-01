forth.prg: src/*.fth
	cat src/core.fth src/kernal.fth src/hny.fth | tr '\n' '\r' >kernel-cr.fth
	python3 compress.py kernel-cr.fth kernel-cr.compressed
	acme forth.asm

upload: forth.prg
	sudo ef3usb /dev/ttyACM0 s
	sudo ef3usb /dev/ttyACM0 e forth.prg

# forth.d64: forth.prg kernel.prg tests.prg tasks.prg vicsid.prg morse.prg pi.prg
# 	c1541 -format "runtime,0" d64 forth.d64 -attach forth.d64 \
# 		-write turbotape.prg turbotape \
# 		-write forth.prg forth \
# 		-write vicsid.prg vicsid \
# 		-write morse.prg morse \
# 		-write tasks.prg tasks \
# 		-write pi.prg pi \
# 		-write tests.prg tests \
# 		-write kernel.prg kernel

# kernel.prg: kernel.fth make_prg.py
# 	python3 make_prg.py kernel.fth kernel.prg

# tests.prg: tests.fth make_prg.py
# 	python3 make_prg.py tests.fth tests.prg

# tasks.prg: tasks.fth make_prg.py
# 	python3 make_prg.py tasks.fth tasks.prg

# vicsid.prg: vicsid.fth make_prg.py
# 	python3 make_prg.py vicsid.fth vicsid.prg

# morse.prg: morse.fth make_prg.py
# 	python3 make_prg.py morse.fth morse.prg

# pi.prg: pi.fth make_prg.py
# 	python3 make_prg.py pi.fth pi.prg

# forth.prg: forth.asm kernel.fth
# 	tr '\n' '\r' <kernel.fth >kernel-cr.fth
# 	python3 compress.py kernel-cr.fth kernel-cr.compressed
# 	acme forth.asm

# run: forth.prg forth.d64
# 	x64 -8 forth.d64 -autostartprgmode 1 forth.prg 

# noauto: forth.d64
# 	x64 -8 forth.d64

clean:
	rm -f forth.d64 forth.prg kernel.prg tests.prg tasks.prg vicsid.prg \
		morse.prg pi.prg *-cr.fth *-cr.compressed
