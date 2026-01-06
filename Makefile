# The sources specified here are compressed and included in main binary.
forth.prg: src/*.fth
	cat src/core.fth | tr '\n' '\r' >kernel-cr.fth
	echo | tr '\n' '\r' >>kernel-cr.fth
	cat src/kernal.fth | tr '\n' '\r' >>kernel-cr.fth
	echo | tr '\n' '\r' >>kernel-cr.fth
	cat src/source.fth | tr '\n' '\r' >>kernel-cr.fth
	echo | tr '\n' '\r' >>kernel-cr.fth
	cat src/editor.fth | tr '\n' '\r' >>kernel-cr.fth
	echo | tr '\n' '\r' >>kernel-cr.fth
	cat src/init.fth | tr '\n' '\r' >>kernel-cr.fth
	echo | tr '\n' '\r' >>kernel-cr.fth
	python3 compress.py kernel-cr.fth kernel-cr.compressed
	acme forth.asm

# Tested with Kung Fu Flash
upload: forth.prg
	sudo ef3usb /dev/ttyACM0 s
	sudo ef3usb /dev/ttyACM0 e forth.prg

PRGS = prg/vicsid prg/morse prg/tasks prg/pi prg/tests prg/kernal prg/editor prg/core prg/source

forth.d64: forth.prg $(PRGS)
	c1541 -format "runtime,0" d64 forth.d64 -attach forth.d64 \
		-write forth.prg forth
	sh copy_prgs.sh forth.d64 $(PRGS)

# Address here must be synced with forth.asm
$(PRGS): prg/%: src/%.fth
	python3 make_prg.py 7000 $< $@

run: forth.prg forth.d64
	x64 -8 forth.d64 -autostartprgmode 1 forth.prg 

noauto: forth.d64
	x64 -8 forth.d64

clean:
	rm -f forth.d64 forth.prg prg/* *-cr.fth *-cr.compressed
