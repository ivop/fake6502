#pragma once
#include <stdio.h>
#include <stdint.h>

extern uint16_t PC;
extern uint8_t SP, A, X, Y, status;
void setP(uint8_t x);
uint8_t getP(void);

extern uint8_t read6502(uint16_t address);
extern void write6502(uint16_t address, uint8_t value);

int nmi6502(void);
int reset6502(void);
int irq6502(void);
int step6502(void);
