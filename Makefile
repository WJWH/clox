CC = gcc

# Add parallel execution by default
MAKEFLAGS += --jobs

# This works (gathers up all the .c files in current folder and compiles them into clox executable) but
# is not incremental and will recompile everything each time
# clox: $(wildcard *.c)
# 	$(CC) $^ -o clox

CLOX_OBJECTS = main.o chunk.o memory.o debug.o value.o vm.o scanner.o compiler.o

build: clox

clox: $(CLOX_OBJECTS)
	$(CC) $^ -o $@

%.o : %.c
	$(CC) -c -o $@ $^ 

.PHONY: clean
clean:
	rm -f *.o clox
