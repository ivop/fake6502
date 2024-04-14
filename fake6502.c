/* Fake6502 CPU emulator core v1.1 *******************
 * (c)2011-2013 Mike Chambers                        *
 *****************************************************/

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

extern uint8_t read6502(uint16_t address);
extern void write6502(uint16_t address, uint8_t value);

static void (*addrtable[256])();
static void (*optable[256])();
static uint8_t penaltyop, penaltyaddr;

#define FLAG_BREAK     0x10
#define FLAG_CONSTANT  0x20
#define BASE_STACK     0x100

#define saveaccum(n)        A = (n)
#define zerocalc(n)         Z = !((n) & 0xff)
#define signcalc(n)         N = (n) & 0x80
#define carrycalc(n)        C = (n) & 0xff00
#define overflowcalc(n,m,o) V = ((n) ^ (uint16_t)(m)) & ((n) ^ (o)) & 0x80

#define makeP ((N<<7)|(V<<6)|(1<<5)|(B<<4)|(D<<3)|(I<<2)|(Z<<1)|C)
#define splitP(x) \
    N=(x)&0x80, V=(x)&0x40, B=(x)&0x10, D=(x)&8, I=(x)&4, Z=(x)&2, C=(x)&1

uint16_t PC;
uint8_t SP, A, X, Y;
bool C, Z, I, D, B, V, N;
uint64_t instructions = 0; //keep track of total instructions executed
uint32_t clockticks6502 = 0, clockgoal6502 = 0;
static uint16_t ea, reladdr, value, result;
static uint8_t opcode, oldstatus;
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
    return (read6502(BASE_STACK + ++SP));
}

void reset6502() {
    PC = (uint16_t)read6502(0xFFFC) | ((uint16_t)read6502(0xFFFD) << 8);
    A = 0;
    X = 0;
    Y = 0;
    SP = 0xFD;
}

static void imp() { //implied
}

static void acc() { //accumulator
}

static void imm() { //immediate
    ea = PC++;
}

static void zp() { //zero-page
    ea = (uint16_t)read6502((uint16_t)PC++);
}

static void zpx() { //zero-page,X
    ea = ((uint16_t)read6502((uint16_t)PC++) + (uint16_t)X) & 0xFF; //zero-page wraparound
}

static void zpy() { //zero-page,Y
    ea = ((uint16_t)read6502((uint16_t)PC++) + (uint16_t)Y) & 0xFF; //zero-page wraparound
}

static void rel() { //relative for branch ops (8-bit immediate value, sign-extended)
    reladdr = (uint16_t)read6502(PC++);
    if (reladdr & 0x80) reladdr |= 0xFF00;
}

static void abso() { //absolute
    ea = (uint16_t)read6502(PC) | ((uint16_t)read6502(PC+1) << 8);
    PC += 2;
}

static void absx() { //absolute,X
    uint16_t startpage;
    ea = ((uint16_t)read6502(PC) | ((uint16_t)read6502(PC+1) << 8));
    startpage = ea & 0xFF00;
    ea += (uint16_t)X;

    if (startpage != (ea & 0xFF00)) { //one cycle penlty for page-crossing on some opcodes
        penaltyaddr = 1;
    }

    PC += 2;
}

static void absy() { //absolute,Y
    uint16_t startpage;
    ea = ((uint16_t)read6502(PC) | ((uint16_t)read6502(PC+1) << 8));
    startpage = ea & 0xFF00;
    ea += (uint16_t)Y;

    if (startpage != (ea & 0xFF00)) { //one cycle penlty for page-crossing on some opcodes
        penaltyaddr = 1;
    }

    PC += 2;
}

static void ind() { //indirect
    uint16_t eahelp, eahelp2;
    eahelp = (uint16_t)read6502(PC) | (uint16_t)((uint16_t)read6502(PC+1) << 8);
    eahelp2 = (eahelp & 0xFF00) | ((eahelp + 1) & 0x00FF); //replicate 6502 page-boundary wraparound bug
    ea = (uint16_t)read6502(eahelp) | ((uint16_t)read6502(eahelp2) << 8);
    PC += 2;
}

static void indx() { // (indirect,X)
    uint16_t eahelp;
    eahelp = (uint16_t)(((uint16_t)read6502(PC++) + (uint16_t)X) & 0xFF); //zero-page wraparound for table pointer
    ea = (uint16_t)read6502(eahelp & 0x00FF) | ((uint16_t)read6502((eahelp+1) & 0x00FF) << 8);
}

static void indy() { // (indirect),Y
    uint16_t eahelp, eahelp2, startpage;
    eahelp = (uint16_t)read6502(PC++);
    eahelp2 = (eahelp & 0xFF00) | ((eahelp + 1) & 0x00FF); //zero-page wraparound
    ea = (uint16_t)read6502(eahelp) | ((uint16_t)read6502(eahelp2) << 8);
    startpage = ea & 0xFF00;
    ea += (uint16_t)Y;

    if (startpage != (ea & 0xFF00)) { //one cycle penlty for page-crossing on some opcodes
        penaltyaddr = 1;
    }
}

static inline uint16_t getvalue() {
    return addrtable[opcode] == acc ? A : read6502(ea);
}

static inline void putvalue(uint16_t saveval) {
    if (addrtable[opcode] == acc) A = saveval; else write6502(ea, saveval);
}

static void adc() {
    penaltyop = 1;
    value = getvalue();
    result = A + value + C;
    zerocalc(result);

    if (D) {
        uint8_t B = (A & 0x0f) + (value & 0x0f) + C;
        if (B >= 0x0a) B = ((B + 0x06) & 0x0f) + 0x10;
        result = (A & 0xf0) + (value & 0xf0) + B;
        if (result >= 0xa0) result += 0x60;
        clockticks6502++;
    }

    carrycalc(result);
    overflowcalc(result, A, value);
    signcalc(result);

    saveaccum(result);
}

static void and() {
    penaltyop = 1;
    value = getvalue();
    result = A & value;

    zerocalc(result);
    signcalc(result);

    saveaccum(result);
}

static void asl() {
    value = getvalue();
    result = value << 1;

    carrycalc(result);
    zerocalc(result);
    signcalc(result);

    putvalue(result);
}

static void branch(bool condition) {
    if (condition) {
        uint16_t oldpc = PC;    // for page cross check
        PC += reladdr;
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

static void bit() {
    value = getvalue();
    result = A & value;

    zerocalc(result);
    N = value & 0x80;
    V = value & 0x40;
}

static void brk() {
    PC++;
    push16(PC);                 // address before next instruction
    push8(makeP | FLAG_BREAK);
    I = 1;
    PC = read6502(0xFFFE) | (read6502(0xFFFF) << 8);
}

static void clc() { C = 0; }
static void cld() { D = 0; }
static void cli() { I = 0; }
static void clv() { V = 0; }

static void cmp() {
    penaltyop = 1;
    value = getvalue();
    result = A - value;

    C = A >= (value & 0xff);
    Z = A == (value & 0xff);
    signcalc(result);
}

static void cpx() {
    value = getvalue();
    result = X - value;

    C = X >= (value & 0xff);
    Z = X == (value & 0xff);
    signcalc(result);
}

static void cpy() {
    value = getvalue();
    result = Y - value;

    C = Y >= (value & 0xff);
    Z = Y == (value & 0xff);
    signcalc(result);
}

static void dec() {
    value = getvalue();
    result = value - 1;

    zerocalc(result);
    signcalc(result);

    putvalue(result);
}

static void dex() {
    X--;

    zerocalc(X);
    signcalc(X);
}

static void dey() {
    Y--;

    zerocalc(Y);
    signcalc(Y);
}

static void eor() {
    penaltyop = 1;
    value = getvalue();
    result = A ^ value;

    zerocalc(result);
    signcalc(result);

    saveaccum(result);
}

static void inc() {
    value = getvalue();
    result = value + 1;

    zerocalc(result);
    signcalc(result);

    putvalue(result);
}

static void inx() {
    X++;

    zerocalc(X);
    signcalc(X);
}

static void iny() {
    Y++;

    zerocalc(Y);
    signcalc(Y);
}

static void jmp() {
    PC = ea;
}

static void jsr() {
    push16(PC - 1);
    PC = ea;
}

static void lda() {
    penaltyop = 1;
    value = getvalue();
    A = value;

    zerocalc(A);
    signcalc(A);
}

static void ldx() {
    penaltyop = 1;
    value = getvalue();
    X = value;

    zerocalc(X);
    signcalc(X);
}

static void ldy() {
    penaltyop = 1;
    value = getvalue();
    Y = value;

    zerocalc(Y);
    signcalc(Y);
}

static void lsr() {
    value = getvalue();
    result = value >> 1;

    C = value & 1;
    zerocalc(result);
    signcalc(result);

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

static void ora() {
    penaltyop = 1;
    value = getvalue();
    result = A | value;

    zerocalc(result);
    signcalc(result);

    saveaccum(result);
}

static void pha() {
    push8(A);
}

static void php() {
    push8(makeP | FLAG_BREAK);
}

static void pla() {
    A = pull8();

    zerocalc(A);
    signcalc(A);
}

static void plp() {
    uint8_t P = pull8();
    splitP(P);
}

static void rol() {
    value = getvalue();
    result = (value << 1) | C;

    carrycalc(result);
    zerocalc(result);
    signcalc(result);

    putvalue(result);
}

static void ror() {
    value = getvalue();
    result = (value >> 1) | (C << 7);

    C = value & 1;
    zerocalc(result);
    signcalc(result);

    putvalue(result);
}

static void rti() {
    uint8_t P = pull8();
    splitP(P);
    value = pull16();
    PC = value;
}

static void rts() {
    value = pull16();
    PC = value + 1;
}

static void sbc() {
    bool cC = C;
    penaltyop = 1;
    value = getvalue() ^ 0xff;
    result = A + value + C;
    carrycalc(result);
    zerocalc(result);
    overflowcalc(result, A, value);
    signcalc(result);

    if (D) {
        uint16_t AL, B;
        B = value ^ 0xff;
        AL = (A & 0x0f) - (B & 0x0f) + cC - 1;
        if(AL & 0x8000)  AL =  ((AL - 0x06) & 0x0f) - 0x10;
        result = (A & 0xf0) - (B & 0xf0) + AL;
        if(result & 0x8000) result -= 0x60;
        clockticks6502++;
    }

    saveaccum(result);
}

static void sec() { C = 1; }
static void sed() { D = 1; }
static void sei() { I = 1; }

static void sta() {
    putvalue(A);
}

static void stx() {
    putvalue(X);
}

static void sty() {
    putvalue(Y);
}

static void tax() {
    X = A;

    zerocalc(X);
    signcalc(X);
}

static void tay() {
    Y = A;

    zerocalc(Y);
    signcalc(Y);
}

static void tsx() {
    X = SP;

    zerocalc(X);
    signcalc(X);
}

static void txa() {
    A = X;

    zerocalc(A);
    signcalc(A);
}

static void txs() {
    SP = X;
}

static void tya() {
    A = Y;

    zerocalc(A);
    signcalc(A);
}

static void lax() {
    lda();
    ldx();
}

static void sax() {
    sta();
    stx();
    putvalue(A & X);
    if (penaltyop && penaltyaddr) clockticks6502--;
}

static void dcp() {
    dec();
    cmp();
    if (penaltyop && penaltyaddr) clockticks6502--;
}

static void isb() {
    inc();
    sbc();
    if (penaltyop && penaltyaddr) clockticks6502--;
}

static void slo() {
    asl();
    ora();
    if (penaltyop && penaltyaddr) clockticks6502--;
}

static void rla() {
    rol();
    and();
    if (penaltyop && penaltyaddr) clockticks6502--;
}

static void sre() {
    lsr();
    eor();
    if (penaltyop && penaltyaddr) clockticks6502--;
}

static void rra() {
    ror();
    adc();
    if (penaltyop && penaltyaddr) clockticks6502--;
}

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
    brk,ora,nop,slo,nop,ora,asl,slo,php,ora,asl,nop,nop,ora,asl,slo, // 0
    bpl,ora,nop,slo,nop,ora,asl,slo,clc,ora,nop,slo,nop,ora,asl,slo, // 1
    jsr,and,nop,rla,bit,and,rol,rla,plp,and,rol,nop,bit,and,rol,rla, // 2
    bmi,and,nop,rla,nop,and,rol,rla,sec,and,nop,rla,nop,and,rol,rla, // 3
    rti,eor,nop,sre,nop,eor,lsr,sre,pha,eor,lsr,nop,jmp,eor,lsr,sre, // 4
    bvc,eor,nop,sre,nop,eor,lsr,sre,cli,eor,nop,sre,nop,eor,lsr,sre, // 5
    rts,adc,nop,rra,nop,adc,ror,rra,pla,adc,ror,nop,jmp,adc,ror,rra, // 6
    bvs,adc,nop,rra,nop,adc,ror,rra,sei,adc,nop,rra,nop,adc,ror,rra, // 7
    nop,sta,nop,sax,sty,sta,stx,sax,dey,nop,txa,nop,sty,sta,stx,sax, // 8
    bcc,sta,nop,nop,sty,sta,stx,sax,tya,sta,txs,nop,nop,sta,nop,nop, // 9
    ldy,lda,ldx,lax,ldy,lda,ldx,lax,tay,lda,tax,nop,ldy,lda,ldx,lax, // A
    bcs,lda,nop,lax,ldy,lda,ldx,lax,clv,lda,tsx,lax,ldy,lda,ldx,lax, // B
    cpy,cmp,nop,dcp,cpy,cmp,dec,dcp,iny,cmp,dex,nop,cpy,cmp,dec,dcp, // C
    bne,cmp,nop,dcp,nop,cmp,dec,dcp,cld,cmp,nop,dcp,nop,cmp,dec,dcp, // D
    cpx,sbc,nop,isb,cpx,sbc,inc,isb,inx,sbc,nop,sbc,cpx,sbc,inc,isb, // E
    beq,sbc,nop,isb,nop,sbc,inc,isb,sed,sbc,nop,isb,nop,sbc,inc,isb  // F
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

void nmi6502() {
    push16(PC);
    push8(makeP);
    I = 1;
    PC = read6502(0xFFFA) | (read6502(0xFFFB) << 8);
}

void irq6502() {
    push16(PC);
    push8(makeP);
    I = 1;
    PC = read6502(0xFFFE) | (read6502(0xFFFF) << 8);
}

uint8_t callexternal = 0;
void (*loopexternal)();

void exec6502(uint32_t tickcount) {
    clockgoal6502 += tickcount;

    while (clockticks6502 < clockgoal6502) {
        opcode = read6502(PC++);

        penaltyop = 0;
        penaltyaddr = 0;

        (*addrtable[opcode])();
        (*optable[opcode])();
        clockticks6502 += ticktable[opcode];
        if (penaltyop && penaltyaddr) clockticks6502++;

        instructions++;

        if (callexternal) (*loopexternal)();
    }

}

void step6502() {
    opcode = read6502(PC++);

    penaltyop = 0;
    penaltyaddr = 0;

    (*addrtable[opcode])();
    (*optable[opcode])();
    clockticks6502 += ticktable[opcode];
    if (penaltyop && penaltyaddr) clockticks6502++;
    clockgoal6502 = clockticks6502;

    instructions++;

    if (callexternal) (*loopexternal)();
}

void hookexternal(void *funcptr) {
    if (funcptr != (void *)NULL) {
        loopexternal = funcptr;
        callexternal = 1;
    } else callexternal = 0;
}
