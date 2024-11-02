#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char* argv[]) {
  initVM();
  Chunk chunk;
  initChunk(&chunk);
  int constant = addConstant(&chunk, 1.2); // make a new constant in the constant pool of the chunk
  writeChunk(&chunk, OP_CONSTANT,123); // add OP_constant
  writeChunk(&chunk, constant,123);    // next byte is index of the constant in the constant pool of the chunk
  writeChunk(&chunk, OP_RETURN,123);

  disassembleChunk(&chunk, "test chunk");
  freeVM();
  freeChunk(&chunk);
  return 0;
}
