#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include "fake6502.h"

uint8_t memory[65536];

uint8_t read6502(uint16_t address) {
    return memory[address];
}

void write6502(uint16_t address, uint8_t value) {
    memory[address] = value;
}

void test(const char *filename, uint16_t success, bool trace) {
    printf("%s -- ", filename);
    FILE *f = fopen(filename,"rb");
    if (!f) {
        fprintf(stderr, "cannot open test\n");
        return;
    }
    if (fread(memory, 1, 65536, f) != 65536) {
        fprintf(stderr, "premature EOF\n");
        return;
    }
    fflush(stdout);
    instructions = 0;
    reset6502();
    while (1) {
        uint16_t save = PC;
        step6502();
        if (trace) {
            printf("PC=%04X ", PC);
            printf("SP=01%02X ", SP);
            printf("A=%02X X=%02X Y=%02X ", A, X,  Y);
            printf("P=%02X\n", getP());
        }
        if (save == PC) {
            printf("%s -- ", PC != success ? "\x1b[1;31mFAIL!\x1b[0m" :
                                             "\x1b[1;32mpass\x1b[0m");
            printf("PC=%04x ", PC);
            printf("A=%02x X=%02x Y=%02x SP=%02x P=%02x\n", A, X, Y, SP, getP());
            return;
        }
    }
}

int main(void) {
#if 1
    printf("Klaus Dormann test suite.\n");
    test("tests/6502_functional_test.bin", 0x3469, false);
    test("tests/6502_decimal_test.bin", 0x044b, false);
    printf("\nBird Computer test suite.\n");
    test("tests/bird6502.bin", 0x861c, false);
    printf("\nRuud Baltissen test suite.\n");
    test("tests/ttl6502.bin", 0xf5ea, false);
    printf("\nLorenz test suite for undocumented opcodes.\n");
    test("tests/lorenz/slo_asoa.bin", 0x08b3, false);
    test("tests/lorenz/slo_asoax.bin", 0x08ca, false);
    test("tests/lorenz/slo_asoay.bin", 0x08ca, false);
    test("tests/lorenz/slo_asoix.bin", 0x08c4, false);
    test("tests/lorenz/slo_asoiy.bin", 0x08ce, false);
    test("tests/lorenz/slo_asoz.bin", 0x08b6, false);
    test("tests/lorenz/slo_asozx.bin", 0x08c0, false);
    test("tests/lorenz/rlaa.bin", 0x08aa, false);
    test("tests/lorenz/rlaax.bin", 0x08c0, false);
    test("tests/lorenz/rlaay.bin", 0x08c0, false);
    test("tests/lorenz/rlaix.bin", 0x08ba, false);
    test("tests/lorenz/rlaiy.bin", 0x08c4, false);
    test("tests/lorenz/rlaz.bin", 0x08ad, false);
    test("tests/lorenz/rlazx.bin", 0x08b6, false);
    test("tests/lorenz/sre_lsea.bin", 0x08a8, false);
    test("tests/lorenz/sre_lseax.bin", 0x08be, false);
    test("tests/lorenz/sre_lseay.bin", 0x08be, false);
    test("tests/lorenz/sre_lseix.bin", 0x08b8, false);
    test("tests/lorenz/sre_lseiy.bin", 0x08c2, false);
    test("tests/lorenz/sre_lsez.bin", 0x08ab, false);
    test("tests/lorenz/sre_lsezx.bin", 0x08b4, false);
    test("tests/lorenz/rraa.bin", 0x0887, false);
    test("tests/lorenz/rraax.bin", 0x089d, false);
    test("tests/lorenz/rraay.bin", 0x089d, false);
    test("tests/lorenz/rraix.bin", 0x0897, false);
    test("tests/lorenz/rraiy.bin", 0x08a1, false);
    test("tests/lorenz/rraz.bin", 0x088a, false);
    test("tests/lorenz/rrazx.bin", 0x0893, false);
    test("tests/lorenz/sax_axsa.bin", 0x088d, false);
    test("tests/lorenz/sax_axsix.bin", 0x0897, false);
    test("tests/lorenz/sax_axsz.bin", 0x0890, false);
    test("tests/lorenz/sax_axszy.bin", 0x0899, false);
    test("tests/lorenz/laxa.bin", 0x088e, false);
    test("tests/lorenz/laxay.bin", 0x08a4, false);
    test("tests/lorenz/laxix.bin", 0x089e, false);
    test("tests/lorenz/laxiy.bin", 0x08a8, false);
    test("tests/lorenz/laxz.bin", 0x0891, false);
    test("tests/lorenz/laxzy.bin", 0x089a, false);
    test("tests/lorenz/dcp_dcma.bin", 0x088c, false);
    test("tests/lorenz/dcp_dcmax.bin", 0x08a2, false);
    test("tests/lorenz/dcp_dcmay.bin", 0x08a2, false);
    test("tests/lorenz/dcp_dcmix.bin", 0x089c, false);
    test("tests/lorenz/dcp_dcmiy.bin", 0x08a6, false);
    test("tests/lorenz/dcp_dcmz.bin", 0x088f, false);
    test("tests/lorenz/dcp_dcmzx.bin", 0x0898, false);
    test("tests/lorenz/isc_insa.bin", 0x088c, false);
    test("tests/lorenz/isc_insax.bin", 0x08a2, false);
    test("tests/lorenz/isc_insay.bin", 0x08a2, false);
    test("tests/lorenz/isc_insix.bin", 0x089c, false);
    test("tests/lorenz/isc_insiy.bin", 0x08a6, false);
    test("tests/lorenz/isc_insz.bin", 0x088f, false);
    test("tests/lorenz/isc_inszx.bin", 0x0898, false);
    test("tests/lorenz/ancb.bin", 0x08d8, false);
    test("tests/lorenz/alrb.bin", 0x08aa, false);
    test("tests/lorenz/arrb.bin", 0x0947, false);
    test("tests/lorenz/sbxb.bin", 0x08c3, false);
    test("tests/lorenz/lasay.bin", 0x08f1, false);
    test("tests/lorenz/shaay.bin", 0x08d6, false);
    test("tests/lorenz/shaiy.bin", 0x08d9, false);
    test("tests/lorenz/shxay.bin", 0x08b5, false);
    test("tests/lorenz/shyax.bin", 0x08b5, false);
    test("tests/lorenz/tas_shsay.bin", 0x08f5, false);
    test("tests/lorenz/aneb.bin", 0x08cb, false);
    test("tests/lorenz/lxab.bin", 0x08c2, false);
    printf("\nVisual6502 test for adc/sbc in decimal mode.\n");
    test("tests/6502DecimalMode.bin", 0x8133, false);
    printf("\nPiotr Fusik tests.\n");
    test("tests/cpu_decimal.bin", 0x302f, false);
    test("tests/cpu_las.bin", 0x304f, false);
    printf("\nAvery Lee tests.\n");
    test("tests/avery.bin", 0x20db, false);
    test("tests/avery2.bin", 0x20fa, false);
#endif
    return 0;
}
