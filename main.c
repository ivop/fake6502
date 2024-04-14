#include <stdio.h>
#include <stdint.h>
#include "fake6502.h"

uint8_t memory[65536];

uint8_t read6502(uint16_t address) {
    return memory[address];
}

void write6502(uint16_t address, uint8_t value) {
    memory[address] = value;
}

void test(const char *filename, uint16_t success) {
    printf("Test: %s -- ", filename);
    FILE *f = fopen(filename,"rb");
    if (!f) {
        fprintf(stderr, "cannot open test\n");
        return;
    }
    if (fread(memory, 1, 65536, f) != 65536) {
        fprintf(stderr, "premature EOF\n");
        return;
    }
    reset6502();
    while (1) {
        uint16_t save = pc;
        step6502();
        if (save == pc) {
            printf("%s ", pc != success ? "FAIL!" : "PASS");
            printf("pc = $%04x, instructions = %ld\n", pc, instructions);
            printf("\tA=%02x X=%02x Y=%02x SP=%02x\n", a, x, y, sp);
            return;
        }
    }
}

void main(void) {
    test("test/6502_functional_test.bin", 0x3469);
    instructions = 0;
    test("test/6502_decimal_test.bin", 0x044b);
}
