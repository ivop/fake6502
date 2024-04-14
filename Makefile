
all: main

main: main.c fake6502.c fake6502.h
	$(CC) -flto -march=native -O3 -o main main.c fake6502.c

clean:
	rm -f *~ */*~ main

