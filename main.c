#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char* argv[]) {
  initVM();
  Chunk chunk;
  initChunk(&chunk);
  int constant = addConstant(&chunk, 1.2); // push 1.2
  writeChunk(&chunk, OP_CONSTANT,123);
  writeChunk(&chunk, constant,123);
  constant = addConstant(&chunk, 3.4); // push 3.4
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constant, 123);

  writeChunk(&chunk, OP_ADD, 123); // stack is now 1.2+3.4=4.6

  constant = addConstant(&chunk, 5.6); // push 5.6
  writeChunk(&chunk, OP_CONSTANT, 123);
  writeChunk(&chunk, constant, 123);

  writeChunk(&chunk, OP_DIVIDE, 123); // 4.6/5.6 = ~0.82
  writeChunk(&chunk, OP_NEGATE, 123); // -0.82
  writeChunk(&chunk, OP_RETURN,123);  // return

  disassembleChunk(&chunk, "test chunk");
  interpret(&chunk);
  freeVM();
  freeChunk(&chunk);
  return 0;
}
