
all: test

test: test.c fake6502.c fake6502.h
	$(CC) -flto -march=native -O3 -o test test.c fake6502.c

clean:
	rm -f *~ */*~ test

