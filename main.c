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
    reset6502();
    while (1) {
        uint16_t save = PC;
        step6502();
        if (trace) {
            printf("PC=%04x ", PC);
            printf("A=%02x X=%02x Y=%02x SP=%02x P=%02x\n", A, X, Y, SP, getP());
        }
        if (save == PC) {
            printf("%s --- ", PC != success ? "FAIL!" : "pass");
            printf("PC=%04x ", PC);
            printf("A=%02x X=%02x Y=%02x SP=%02x P=%02x\n", A, X, Y, SP, getP());
            return;
        }
    }
}

void main(void) {
    test("test/6502_functional_test.bin", 0x3469, false);
    instructions = 0;
    test("test/6502_decimal_test.bin", 0x044b, false);
    instructions = 0;
    test("test/bird6502.bin", 0x861c, false);
    instructions = 0;
    test("test/ttl6502.bin", 0xf5ea, false);
    instructions = 0;
    test("test/6502DecimalMode.bin", 0x8133, false);
    instructions = 0;
    test("test/cpu_decimal.bin", 0x302f, false);
    instructions = 0;
    test("test/lorenz/slo_asoa.bin", 0x08b3, false);
    instructions = 0;
    test("test/lorenz/slo_asoax.bin", 0x08ca, false);
    instructions = 0;
    test("test/lorenz/slo_asoay.bin", 0x08ca, false);
    instructions = 0;
    test("test/lorenz/slo_asoix.bin", 0x08c4, false);
    instructions = 0;
    test("test/lorenz/slo_asoiy.bin", 0x08ce, false);
    instructions = 0;
    test("test/lorenz/slo_asoz.bin", 0x08b6, false);
    instructions = 0;
    test("test/lorenz/slo_asozx.bin", 0x08c0, false);
}
