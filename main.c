#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char* argv[]) {
  Chunk chunk;
  initChunk(&chunk);
  int constant = addConstant(&chunk, 1.2); // make a new constant in the constant pool of the chunk
  writeChunk(&chunk, OP_CONSTANT); // add OP_constant
  writeChunk(&chunk, constant);    // next byte is index of the constant in the constant pool of the chunk
  writeChunk(&chunk, OP_RETURN);

  disassembleChunk(&chunk, "test chunk");
  freeChunk(&chunk);
  return 0;
}
