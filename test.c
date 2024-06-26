#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include "fake6502.h"

#define FAIL "\x1b[1;31mFAIL!\x1b[0m"
#define PASS "\x1b[1;32mpass\x1b[0m"

uint8_t memory[65536];

uint8_t read6502(uint16_t address) {
    return memory[address];
}

void write6502(uint16_t address, uint8_t value) {
    memory[address] = value;
}

void load_file(const char *filename) {
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
}

void test(const char *filename, uint16_t success, bool trace) {
    load_file(filename);
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
            printf("%s -- ", PC != success ? FAIL : PASS);
            printf("PC=%04x ", PC);
            printf("A=%02x X=%02x Y=%02x SP=%02x ", A, X, Y, SP);
            printf("P=%02x\n", getP());
            return;
        }
    }
}

// expected cycles in test program order, starting at timing:
uint8_t exp_cycles[] = {
    7,6,6,3,5,3,2,2,4,6,                    // documented: 00-0f
    2,2,2,3,4,2,5,2,6,4,6,2,4,5,2,4,5,7,    // documented: 10-1f
    6,6,6,3,3,5,4,2,2,4,4,6,                // documented: 20-2f
    2,2,2,3,4,2,5,2,6,4,6,2,4,5,4,5,7,      // documented: 30-3f
    6,3,5,3,2,2,3,4,6,                      // documented: 40-4f
    2,3,4,2,2,3,4,2,5,2,6,4,6,2,4,5,4,5,7,  // documented: 50-5f
    2,6,3,2,4,2,7,4,3,5,5,4,2,5,6,          // documented: 60-6f
    2,2,2,3,4,3,4,2,2,5,2,6,4,4,5,4,5,      // documented: 70-7f
    2,2,6,2,7,5,5,6,5,6,6,2,7,
    2,6,3,3,3,2,2,4,4,4,                    // documented: 80-8f
    2,2,2,3,4,2,6,2,3,6,2,4,4,4,2,5,5,      // documented: 90-9f
    2,5,5,
    2,6,2,3,3,3,2,2,2,4,4,4,                // documented: a0-af
    2,2,2,3,4,2,5,2,6,4,4,4,2,2,4,5,2,2,    // documented: b0-bf
    4,5,4,5,4,5,
    2,6,3,3,5,2,2,2,4,4,6,                  // documented: c0-cf
    2,2,2,3,4,2,5,2,6,4,6,2,4,5,4,5,7,      // documented: d0-df
    2,2,6,3,2,4,2,7,4,3,5,3,5,2,2,4,6,      // documented: e0-ef
    2,2,2,3,4,2,2,5,2,6,4,4,5,2,4,2,5,      // documented: f0-ff
    2,2,6,2,7,5,5,6,2,5,2,6,6,2,7,

    2,2,2,2,2,2, 2,2,2,2,2, 3,3,3,          // undocumented nops
    4,4,4,4,4,4, 4,
    2,4,4,4,4,4,4,5,5,5,5,5,5,

    5,6,8,8,6,7,7,          // SLO
    5,6,8,8,6,7,7,          // RLA
    5,6,8,8,6,7,7,          // SRE
    5,6,8,8,6,7,7,          // RRA
    3,4,6,4,                // SAX
    3,4,6,4,2,5,4,2,6,5,    // LAX
    5,6,8,8,6,7,7,          // DCP
    5,6,8,8,6,7,7,          // ISC
    2,2, 2, 2, 2,           // ANC, ALR, ARR, SBX
    2,4,5,                  // LAS
    6,5,5, 5,5, 2,5,5, 5,5, // SHA, SHX, SHY, TAS
    2, 2,                   // ANE, LXA
    3
};

void test_cycles(void) {
    printf("\nTest cycles per instruction.\n");
    bool do_compare = false;
    int idx = 0;
    load_file("tests/cycles.bin");
    reset6502();
    while (1) {
        uint16_t save = PC;
        uint8_t instr = read6502(PC);
        if (PC == 0x3000) do_compare = true;
        int spent = step6502();
        if (do_compare) {
            if (exp_cycles[idx] != spent) {
                printf("PC: %04X instr: $%02x spent: %d expected: %d\n", save, instr, spent, exp_cycles[idx]);
                goto errout;
            }
            idx++;
        }
        if (PC == 0x200a) break;
    }
    printf("%s\n", PASS);
    return;
errout:
    printf("%s\n", FAIL);
}

int main(void) {
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
    test("tests/avery3.bin", 0x209d, false);
    printf("\nHCM6502 tests.\n");
    test("tests/AllSuiteA.bin", 0x45c0, false);
    test_cycles();
    return 0;
}
