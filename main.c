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

int main(int argc, char **argv) {
    FILE *f = fopen("test/6502_functional_test.bin","rb");
    if (!f) {
        fprintf(stderr, "cannot open test\n");
        return 1;
    }
    if (fread(memory, 1, 65536, f) != 65536) {
        fprintf(stderr, "premature EOF\n");
        return 1;
    }
    pc = 0x0400;
    while (1) {
        uint16_t save = pc;
        step6502();
        if (save == pc) {
            printf("done. pc = $%04x, instructions = %ld\n", pc, instructions);
            if (pc != 0x3469) printf("FAIL!\n"); else printf("PASS\n");
            return 0;
        }
    }
}
