
all: slo rla

slo:
	mads -l -o:slo_asoa.bin slo_asoa.asm
	mads -d:ATARI=1 -o:slo_asoa.xex slo_asoa.asm
	mads -l -o:slo_asoax.bin slo_asoax.asm
	mads -d:ATARI=1 -o:slo_asoax.xex slo_asoax.asm
	mads -l -o:slo_asoay.bin slo_asoay.asm
	mads -d:ATARI=1 -o:slo_asoay.xex slo_asoay.asm
	mads -l -o:slo_asoix.bin slo_asoix.asm
	mads -d:ATARI=1 -o:slo_asoix.xex slo_asoix.asm
	mads -l -o:slo_asoiy.bin slo_asoiy.asm
	mads -d:ATARI=1 -o:slo_asoiy.xex slo_asoiy.asm
	mads -l -o:slo_asoz.bin slo_asoz.asm
	mads -d:ATARI=1 -o:slo_asoz.xex slo_asoz.asm
	mads -l -o:slo_asozx.bin slo_asozx.asm
	mads -d:ATARI=1 -o:slo_asozx.xex slo_asozx.asm

rla:
	mads -l -o:rlaa.bin rlaa.asm
	mads -d:ATARI=1 -o:rlaa.xex rlaa.asm
	mads -l -o:rlaax.bin rlaax.asm
	mads -d:ATARI=1 -o:rlaax.xex rlaax.asm
	mads -l -o:rlaay.bin rlaay.asm
	mads -d:ATARI=1 -o:rlaay.xex rlaay.asm
	mads -l -o:rlaix.bin rlaix.asm
	mads -d:ATARI=1 -o:rlaix.xex rlaix.asm
	mads -l -o:rlaiy.bin rlaiy.asm
	mads -d:ATARI=1 -o:rlaiy.xex rlaiy.asm
	mads -l -o:rlaz.bin rlaz.asm
	mads -d:ATARI=1 -o:rlaz.xex rlaz.asm
	mads -l -o:rlazx.bin rlazx.asm
	mads -d:ATARI=1 -o:rlazx.xex rlazx.asm

clean:
	rm -f *~ *.bin *.xex *.lst
