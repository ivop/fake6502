/*
 * Fake6502 -- MOS6502 CPU Emulator
 *
 * Copyright © 2011-2013 Mike Chambers
 * Copyright © 2024 Ivo van poorten
 *
 * This file is licensed under the terms of the 2-clause BSD license. Please
 * see the LICENSE file in the root project directory for the full text.
 */

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "fake6502.h"

static void (*addrtable[256])();
static void (*optable[256])();
static uint8_t penaltyop, penaltyaddr, callexternal;
static uint64_t clockticks6502;

#define FLAG_BREAK     0x10
#define FLAG_CONSTANT  0x20
#define BASE_STACK     0x100

uint16_t PC;
uint8_t SP, A, X, Y;
bool C, Z, I, D, V, N;
static uint16_t ea;
static uint8_t opcode;

// ------------------ Flags ---------------------------------------------------

static inline void calcZ  (uint8_t  x) { Z = !x; }
static inline void calcN  (uint8_t  x) { N = x & 0x80; }
static inline void calcZN (uint8_t x)  { calcZ(x), calcN(x); }
static inline void calcC  (uint16_t x) { C = x & 0xff00; }
static inline void calcCZN(uint16_t x) { calcC(x), calcZN(x); }

static inline void calcV(uint16_t result, uint8_t accu, uint16_t value) {
    V = (result ^ accu) & (result ^ value) & 0x80;
}

static void splitP(uint8_t x) {
    N=(x)&0x80, V=(x)&0x40, D=(x)&8, I=(x)&4, Z=(x)&2, C=(x)&1;
}

uint8_t getP(void){ return (N<<7)|(V<<6)|(1<<5)|(0<<4)|(D<<3)|(I<<2)|(Z<<1)|C;}

// ----------------------------------------------------------------------------

static void push16(uint16_t pushval) {
    write6502(BASE_STACK + SP, (pushval >> 8) & 0xFF);
    write6502(BASE_STACK + ((SP - 1) & 0xFF), pushval & 0xFF);
    SP -= 2;
}

static void push8(uint8_t pushval) {
    write6502(BASE_STACK + SP--, pushval);
}

static uint16_t pull16() {
    SP += 2;
    return read6502(BASE_STACK + ((SP - 1) & 0xFF)) | \
          (read6502(BASE_STACK + ((SP    ) & 0xFF)) << 8);
}

static uint8_t pull8() {
    return read6502(BASE_STACK + ++SP);
}

static uint16_t read6502word(uint16_t addr) {
    return read6502(addr) | (read6502(addr+1) << 8);
}

// ------------------ Addressing modes ----------------------------------------

static void imp()  { }
static void acc()  { }
static void imm()  { ea = PC++; }
static void zp()   { ea = read6502(PC++); }
static void zpx()  { ea = (read6502(PC++) + X) & 0xff; }
static void zpy()  { ea = (read6502(PC++) + Y) & 0xff; }
static void abso() { ea = read6502word(PC); PC += 2; }
static void rel()  { ea = PC + (int8_t) read6502(PC++); }

static void absx() {
    ea = read6502word(PC);
    uint16_t startpage = ea & 0xff00;
    ea += X;
    penaltyaddr = startpage != (ea & 0xff00);     // page crossing
    PC += 2;
}

static void absy() {
    ea = read6502word(PC);
    uint16_t startpage = ea & 0xff00;
    ea += Y;
    penaltyaddr = startpage != (ea & 0xff00);     // page crossing
    PC += 2;
}

static void ind() {
    ea = read6502word(PC);
    uint16_t ea2 = (ea & 0xff00) | ((ea + 1) & 0xff); // page wrap bug!
    ea = read6502(ea) | (read6502(ea2) << 8);
    PC += 2;
}

static void indx() {
    ea = ((read6502(PC++) + X) & 0xff);             // page wraparound
    ea = read6502(ea) | (read6502((ea+1) & 0xff) << 8);
}

static void indy() { // (indirect),Y
    ea = read6502(PC++);
    ea = read6502(ea) | (read6502((ea+1) & 0xff) << 8);  // page wrap
    uint16_t startpage = ea & 0xff00;
    ea += Y;
    penaltyaddr = startpage != (ea & 0xff00);     // page cross penalty
}

// ----------------------------------------------------------------------------

static inline uint16_t getvalue() {
    return addrtable[opcode] == acc ? A : read6502(ea);
}

static inline void putvalue(uint16_t saveval) {
    if (addrtable[opcode] == acc) A = saveval; else write6502(ea, saveval);
}

// ------------------ Opcodes -------------------------------------------------

static void and() { penaltyop = 1; calcZN(A = A & getvalue()); }
static void eor() { penaltyop = 1; A = A ^ getvalue(); calcZN(A); }
static void ora() { penaltyop = 1; A |= getvalue(); calcZN(A); }

static void branch(bool condition) {
    if (condition) {
        uint16_t oldpc = PC;    // for page cross check
        PC = ea;
        if ((oldpc & 0xFF00) != (PC & 0xFF00)) clockticks6502 += 2;
            else clockticks6502++;
    }
}

static void bcc() { branch(!C); }
static void bcs() { branch( C); }
static void bne() { branch(!Z); }
static void beq() { branch( Z); }
static void bpl() { branch(!N); }
static void bmi() { branch( N); }
static void bvc() { branch(!V); }
static void bvs() { branch( V); }

static void clc() { C = 0; }
static void sec() { C = 1; }
static void cld() { D = 0; }
static void sed() { D = 1; }
static void cli() { I = 0; }
static void sei() { I = 1; }
static void clv() { V = 0; }

static void inx() { calcZN(++X); }
static void iny() { calcZN(++Y); }
static void dex() { calcZN(--X); }
static void dey() { calcZN(--Y); }

static void jmp() { PC = ea; }
static void jsr() { push16(PC - 1); PC = ea; }

static void lda() { penaltyop = 1; A = getvalue(); calcZN(A); }
static void ldx() { penaltyop = 1; X = getvalue(); calcZN(X); }
static void ldy() { penaltyop = 1; Y = getvalue(); calcZN(Y); }
static void sta() { putvalue(A); }
static void stx() { putvalue(X); }
static void sty() { putvalue(Y); }

static inline void compare(uint8_t reg, uint8_t value) {
    calcN(reg - value);
    C = reg >= value & 0xff;
    Z = reg == value & 0xff;
}
static void cmp() { compare(A, getvalue()); penaltyop = 1; }
static void cpx() { compare(X, getvalue()); }
static void cpy() { compare(Y, getvalue()); }

static void pha() { push8(A); }
static void php() { push8(getP() | FLAG_BREAK); }
static void pla() { A = pull8(); calcZN(A); }
static void plp() { uint8_t P = pull8(); splitP(P); }

static void rti() { uint8_t P = pull8(); splitP(P); PC = pull16(); }
static void rts() { PC = pull16() + 1; }

static void tax() { X = A; calcZN(X); }
static void tay() { Y = A; calcZN(Y); }
static void tsx() { X = SP; calcZN(X); }
static void txa() { A = X; calcZN(A); }
static void txs() { SP = X; }
static void tya() { A = Y; calcZN(A); }

static void bit() {
    uint16_t value = getvalue();
    calcZ(A & value);
    N = value & 0x80;
    V = value & 0x40;
}

static void brk() {
    push16(++PC);                 // address before next instruction
    push8(getP() | FLAG_BREAK);
    I = 1;
    PC = read6502word(0xfffe);
}

static void dec() {
    uint16_t result = getvalue() - 1;
    calcZN(result);
    putvalue(result);
}

static void inc() {
    uint16_t result = getvalue() + 1;
    calcZN(result);
    putvalue(result);
}

static void asl() {
    uint16_t result = getvalue() << 1;
    calcCZN(result);
    putvalue(result);
}

static void lsr() {
    uint16_t value = getvalue();
    uint16_t result = value >> 1;
    C = value & 1;
    calcZN(result);
    putvalue(result);
}

static void rol() {
    uint16_t result = (getvalue() << 1) | C;
    calcCZN(result);
    putvalue(result);
}

static void ror() {
    uint16_t value = getvalue();
    uint16_t result = (value >> 1) | (C << 7);
    C = value & 1;
    calcZN(result);
    putvalue(result);
}

static void nop() {
    switch (opcode) {
        case 0x1C:
        case 0x3C:
        case 0x5C:
        case 0x7C:
        case 0xDC:
        case 0xFC:
            penaltyop = 1;
            break;
    }
}

static void adc() {
    penaltyop = 1;
    uint16_t value = getvalue();
    uint16_t result = A + value + C;
    calcZ(result);

    if (!D) {
        calcC(result);
        calcV(result, A, value);
        calcN(result);
    } else {
        result = (A & 0x0f) + (value & 0x0f) + C;
        if (result >= 0x0a) result = ((result + 0x06) & 0x0f) + 0x10;
        result += (A & 0xf0) + (value & 0xf0);
        calcN(result);
        calcV(result, A, value);
        if (result >= 0xa0) result += 0x60;
        calcC(result);
        clockticks6502++;
    }

    A = result;
}

static void sbc() {
    bool cC = C;
    penaltyop = 1;
    uint16_t value = getvalue() ^ 0xff;
    uint16_t result = A + value + C;
    calcCZN(result);
    calcV(result, A, value);

    if (D) {
        uint16_t AL, B;
        B = value ^ 0xff;
        AL = (A & 0x0f) - (B & 0x0f) + cC - 1;
        if(AL & 0x8000)  AL =  ((AL - 0x06) & 0x0f) - 0x10;
        result = (A & 0xf0) - (B & 0xf0) + AL;
        if(result & 0x8000) result -= 0x60;
        clockticks6502++;
    }

    A = result;
}

// ------------------ Stable undocumented opcodes -----------------------------

static void SLO() { asl(); ora(); }
static void RLA() { rol(); and(); }
static void SRE() { lsr(); eor(); }
static void RRA() { ror(); adc(); }
static void SAX() { putvalue(A & X); }
static void LAX() { penaltyop = 1; lda(); ldx(); }
static void DCP() { dec(); cmp(); }
static void ISC() { inc(); sbc(); }
static void ANC() { and(); C = A & 0x80; }
static void ALR() { and(); C = A & 1; A >>= 1; calcZN(A); }
static void LAS() { penaltyop = 1; calcZN(SP = A = X = getvalue() & SP); }
static void JAM() { nop(); }


static void ARR() {
    and();

    uint8_t inA = A;

    A >>= 1;
    A |= C << 7;
    calcZN(A);

    if (!D) {
        C = A & 0x40;
        V = C ^ ((A >> 5) & 1);
    } else {
        V = (A ^ inA) & 0x40;
        if (((inA & 0x0f) + (inA & 0x01)) > 0x05)
            A = (A & 0xf0) | ((A + 0x06) & 0x0f);
        if ((uint16_t)inA + (inA & 0x10) >= 0x60) {
            A += 0x60;
            C = 1;
        } else {
            C = 0;
        }
    }
}

static void SBX() {
    uint8_t value = getvalue();
    X &= A;
    compare(X, value);
    X -= value;
}

// ------------------ Unstable undocumented opcodes ---------------------------

static void SHA() { putvalue(A & X & ((ea >> 8) + 1)); }
static void SHX() {
    uint8_t value = X & (((ea - Y) >> 8) + 1);
    if (((ea - Y) & 0xff) + Y > 0xff)
        ea = ea & 0xff | value << 8;
    putvalue(value);
}
static void SHY() {
    uint8_t value = Y & (((ea-X) >> 8) + 1);
    if (((ea - X) & 0xff) + X > 0xff)
        ea = ea & 0xff | value << 8;
    putvalue(value);
}
static void TAS() { SP = A & X; putvalue(SP & ((ea >> 8) + 1));
}

// ------------------ Magic constants undocumented opcodes --------------------

static void ANE() { A = (A | 0xef) & X & getvalue(); calcZN(A); }
static void LXA() { A = X = ( A | 0xee) & getvalue(); calcZN(A); }

// ----------------------------------------------------------------------------

static void (*addrtable[256])() = {
// 0    1   2    3   4   5   6   7   8    9   A    B    C    D    E    F
  imp,indx,imp,indx, zp, zp, zp, zp,imp, imm,acc, imm,abso,abso,abso,abso, // 0
  rel,indy,imp,indy,zpx,zpx,zpx,zpx,imp,absy,imp,absy,absx,absx,absx,absx, // 1
 abso,indx,imp,indx, zp, zp, zp, zp,imp, imm,acc, imm,abso,abso,abso,abso, // 2
  rel,indy,imp,indy,zpx,zpx,zpx,zpx,imp,absy,imp,absy,absx,absx,absx,absx, // 3
  imp,indx,imp,indx, zp, zp, zp, zp,imp, imm,acc, imm,abso,abso,abso,abso, // 4
  rel,indy,imp,indy,zpx,zpx,zpx,zpx,imp,absy,imp,absy,absx,absx,absx,absx, // 5
  imp,indx,imp,indx, zp, zp, zp, zp,imp, imm,acc, imm, ind,abso,abso,abso, // 6
  rel,indy,imp,indy,zpx,zpx,zpx,zpx,imp,absy,imp,absy,absx,absx,absx,absx, // 7
  imm,indx,imm,indx, zp, zp, zp, zp,imp, imm,imp, imm,abso,abso,abso,abso, // 8
  rel,indy,imp,indy,zpx,zpx,zpy,zpy,imp,absy,imp,absy,absx,absx,absy,absy, // 9
  imm,indx,imm,indx, zp, zp, zp, zp,imp, imm,imp, imm,abso,abso,abso,abso, // A
  rel,indy,imp,indy,zpx,zpx,zpy,zpy,imp,absy,imp,absy,absx,absx,absy,absy, // B
  imm,indx,imm,indx, zp, zp, zp, zp,imp, imm,imp, imm,abso,abso,abso,abso, // C
  rel,indy,imp,indy,zpx,zpx,zpx,zpx,imp,absy,imp,absy,absx,absx,absx,absx, // D
  imm,indx,imm,indx, zp, zp, zp, zp,imp, imm,imp, imm,abso,abso,abso,abso, // E
  rel,indy,imp,indy,zpx,zpx,zpx,zpx,imp,absy,imp,absy,absx,absx,absx,absx  // F
};

static void (*optable[256])() = {
//   0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    brk,ora,JAM,SLO,nop,ora,asl,SLO,php,ora,asl,ANC,nop,ora,asl,SLO, // 0
    bpl,ora,JAM,SLO,nop,ora,asl,SLO,clc,ora,nop,SLO,nop,ora,asl,SLO, // 1
    jsr,and,JAM,RLA,bit,and,rol,RLA,plp,and,rol,ANC,bit,and,rol,RLA, // 2
    bmi,and,JAM,RLA,nop,and,rol,RLA,sec,and,nop,RLA,nop,and,rol,RLA, // 3
    rti,eor,JAM,SRE,nop,eor,lsr,SRE,pha,eor,lsr,ALR,jmp,eor,lsr,SRE, // 4
    bvc,eor,JAM,SRE,nop,eor,lsr,SRE,cli,eor,nop,SRE,nop,eor,lsr,SRE, // 5
    rts,adc,JAM,RRA,nop,adc,ror,RRA,pla,adc,ror,ARR,jmp,adc,ror,RRA, // 6
    bvs,adc,JAM,RRA,nop,adc,ror,RRA,sei,adc,nop,RRA,nop,adc,ror,RRA, // 7
    nop,sta,nop,SAX,sty,sta,stx,SAX,dey,nop,txa,ANE,sty,sta,stx,SAX, // 8
    bcc,sta,JAM,SHA,sty,sta,stx,SAX,tya,sta,txs,TAS,SHY,sta,SHX,SHA, // 9
    ldy,lda,ldx,LAX,ldy,lda,ldx,LAX,tay,lda,tax,LXA,ldy,lda,ldx,LAX, // A
    bcs,lda,JAM,LAX,ldy,lda,ldx,LAX,clv,lda,tsx,LAS,ldy,lda,ldx,LAX, // B
    cpy,cmp,nop,DCP,cpy,cmp,dec,DCP,iny,cmp,dex,SBX,cpy,cmp,dec,DCP, // C
    bne,cmp,JAM,DCP,nop,cmp,dec,DCP,cld,cmp,nop,DCP,nop,cmp,dec,DCP, // D
    cpx,sbc,nop,ISC,cpx,sbc,inc,ISC,inx,sbc,nop,sbc,cpx,sbc,inc,ISC, // E
    beq,sbc,JAM,ISC,nop,sbc,inc,ISC,sed,sbc,nop,ISC,nop,sbc,inc,ISC  // F
};

static const uint32_t ticktable[256] = {
//  0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6, // 0
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 1
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6, // 2
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 3
    6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6, // 4
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 5
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6, // 6
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // 7
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, // 8
    2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5, // 9
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, // A
    2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4, // B
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, // C
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7, // D
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, // E
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7  // F
};

int nmi6502() {
    push16(PC);
    push8(getP());
    I = 1;
    PC = read6502word(0xfffa);
    return 7;
}

int reset6502() {
    PC = read6502word(0xfffc);
    A = X = Y = 0;
    C = Z = I = D = V = N = 0;
    SP = 0xFD;
    return 7;
}

int irq6502() {
    push16(PC);
    push8(getP());
    I = 1;
    PC = read6502word(0xfffe);
    return 7;
}

int step6502() {
    opcode = read6502(PC++);

    penaltyop = 0;
    penaltyaddr = 0;
    clockticks6502 = ticktable[opcode];

    (*addrtable[opcode])();
    (*optable[opcode])();

    if (penaltyop && penaltyaddr) clockticks6502++;
    return clockticks6502;
}
