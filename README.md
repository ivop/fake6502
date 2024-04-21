# Fake6502

MOS 6502 Emulator in C.

## Features
* Full MOS 6502 instruction set, including _all_ undocumented opcodes.
* Small (<500 lines of code, including comments).
* Passes all relevant test suites.

## Test suites
* Klaus Dormann functional test, and decimal test.
* Bird Computer test suite.
* Ruud Baltissen ttl6502 test suite.
* Lorenz test suite for undocumented opcodes.
* Visual6502 test for adc/sbc in decimal mode with invalid arguments.
* Piots Fusik tests.
* Avery Lee tests.
* hcm6502 tests.

## Credits
This is a heavily bug-fixed and extended continuation of Fake6502 by Mike Chambers, Copyright © 2011-2013.
All new code is Copyright © 2024 by Ivo van poorten.
License is BSD 2-Clause.
