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
            printf("PC=%04x ", PC);
            printf("A=%02x X=%02x Y=%02x SP=%02x P=%02x\n", A, X, Y, SP, getP());
        }
        if (save == PC) {
            printf("%s --- ", PC != success ? "\x1b[1;31mFAIL!\x1b[0m" :
                                              "\x1b[1;32mpass\x1b[0m");
            printf("PC=%04x ", PC);
            printf("A=%02x X=%02x Y=%02x SP=%02x P=%02x\n", A, X, Y, SP, getP());
            return;
        }
    }
}

int main(void) {
#if 1
    test("test/6502_functional_test.bin", 0x3469, false);
    test("test/6502_decimal_test.bin", 0x044b, false);
    test("test/bird6502.bin", 0x861c, false);
    test("test/ttl6502.bin", 0xf5ea, false);
    test("test/6502DecimalMode.bin", 0x8133, false);
    test("test/cpu_decimal.bin", 0x302f, false);
    test("test/lorenz/slo_asoa.bin", 0x08b3, false);
    test("test/lorenz/slo_asoax.bin", 0x08ca, false);
    test("test/lorenz/slo_asoay.bin", 0x08ca, false);
    test("test/lorenz/slo_asoix.bin", 0x08c4, false);
    test("test/lorenz/slo_asoiy.bin", 0x08ce, false);
    test("test/lorenz/slo_asoz.bin", 0x08b6, false);
    test("test/lorenz/slo_asozx.bin", 0x08c0, false);
    test("test/lorenz/rlaa.bin", 0x08aa, false);
    test("test/lorenz/rlaax.bin", 0x08c0, false);
    test("test/lorenz/rlaay.bin", 0x08c0, false);
    test("test/lorenz/rlaix.bin", 0x08ba, false);
    test("test/lorenz/rlaiy.bin", 0x08c4, false);
    test("test/lorenz/rlaz.bin", 0x08ad, false);
    test("test/lorenz/rlazx.bin", 0x08b6, false);
    test("test/lorenz/sre_lsea.bin", 0x08a8, false);
    test("test/lorenz/sre_lseax.bin", 0x08be, false);
    test("test/lorenz/sre_lseay.bin", 0x08be, false);
    test("test/lorenz/sre_lseix.bin", 0x08b8, false);
    test("test/lorenz/sre_lseiy.bin", 0x08c2, false);
    test("test/lorenz/sre_lsez.bin", 0x08ab, false);
    test("test/lorenz/sre_lsezx.bin", 0x08b4, false);
    test("test/lorenz/rraa.bin", 0x0887, false);
    test("test/lorenz/rraax.bin", 0x089d, false);
    test("test/lorenz/rraay.bin", 0x089d, false);
    test("test/lorenz/rraix.bin", 0x0897, false);
    test("test/lorenz/rraiy.bin", 0x08a1, false);
    test("test/lorenz/rraz.bin", 0x088a, false);
    test("test/lorenz/rrazx.bin", 0x0893, false);
    test("test/lorenz/sax_axsa.bin", 0x088d, false);
    test("test/lorenz/sax_axsix.bin", 0x0897, false);
    test("test/lorenz/sax_axsz.bin", 0x0890, false);
    test("test/lorenz/sax_axszy.bin", 0x0899, false);
    test("test/lorenz/laxa.bin", 0x088e, false);
    test("test/lorenz/laxay.bin", 0x08a4, false);
    test("test/lorenz/laxix.bin", 0x089e, false);
    test("test/lorenz/laxiy.bin", 0x08a8, false);
    test("test/lorenz/laxz.bin", 0x0891, false);
    test("test/lorenz/laxzy.bin", 0x089a, false);
    test("test/lorenz/dcp_dcma.bin", 0x088c, false);
    test("test/lorenz/dcp_dcmax.bin", 0x08a2, false);
    test("test/lorenz/dcp_dcmay.bin", 0x08a2, false);
    test("test/lorenz/dcp_dcmix.bin", 0x089c, false);
    test("test/lorenz/dcp_dcmiy.bin", 0x08a6, false);
    test("test/lorenz/dcp_dcmz.bin", 0x088f, false);
    test("test/lorenz/dcp_dcmzx.bin", 0x0898, false);
    test("test/lorenz/isc_insa.bin", 0x088c, false);
    test("test/lorenz/isc_insax.bin", 0x08a2, false);
    test("test/lorenz/isc_insay.bin", 0x08a2, false);
    test("test/lorenz/isc_insix.bin", 0x089c, false);
    test("test/lorenz/isc_insiy.bin", 0x08a6, false);
    test("test/lorenz/isc_insz.bin", 0x088f, false);
    test("test/lorenz/isc_inszx.bin", 0x0898, false);
    test("test/lorenz/ancb.bin", 0x08d8, false);
    test("test/lorenz/alrb.bin", 0x08aa, false);
    test("test/lorenz/arrb.bin", 0x0947, false);
    test("test/lorenz/sbxb.bin", 0x08c3, false);
    test("test/lorenz/lasay.bin", 0x08f1, false);
    test("test/lorenz/shaay.bin", 0x08d6, false);
    test("test/lorenz/shaiy.bin", 0x08d9, false);
    test("test/lorenz/shxay.bin", 0x08b5, false);
    test("test/lorenz/shyax.bin", 0x08b5, false);
    test("test/lorenz/tas_shsay.bin", 0x08f5, false);
#endif
    return 0;
}
