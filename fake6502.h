#pragma once
#include <stdio.h>
#include <stdint.h>

extern uint16_t PC;
extern uint8_t SP, A, X, Y, status;

extern uint64_t instructions;
extern uint32_t clockticks6502;

extern uint8_t read6502(uint16_t address);
extern void write6502(uint16_t address, uint8_t value);

void nmi6502(void);
void reset6502(void);
void irq6502(void);
void exec6502(uint32_t tickcount);
void step6502(void);
void hookexternal(void *funcptr);
