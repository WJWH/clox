#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"


VM vm;

static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
  vm.stackTop = vm.stack; // not even zeroing out the array, just "forget" about the contents
  vm.frameCount = 0;
  vm.openUpvalues = NULL;
}

static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  CallFrame* frame = &vm.frames[vm.frameCount - 1];
  size_t instruction = frame->ip - frame->closure->function->chunk.code - 1; // deze frame->closure->function was niet aangepast in het boek, maar moet toch wel???
  int line = frame->closure->function->chunk.lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);

  // stack trace
  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->closure->function;
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", 
            function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  resetStack();
}

static void defineNative(const char* name, NativeFn function) {
  push(OBJ_VAL(copyString(name, (int)strlen(name)))); // name under which it can be called from lox
  push(OBJ_VAL(newNative(function)));                 // the C function wrapped in a lox object
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]); // stick the new native function in the globals table
  pop(); // clean up. The values got put on the stack before being put in the globals table so that the GC knows they're alive
  pop(); // and won't free them. For example, newNative allocates memory and could trigger a GC, which might then free the name string, leading to tears
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;
  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;

  initTable(&vm.globals);
  initTable(&vm.strings);

  defineNative("clock", clockNative);
}

void freeVM() {
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  freeObjects();
}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}

// set up new call frame
static bool call(ObjClosure* closure, int argCount) {
  // various runtime error checks
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }

  // everything seems to be OK, set up new callframe for this function and jump to it by setting the ip to its bytecode chunk
  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;
  frame->slots = vm.stackTop - argCount - 1; // slots for this frame start from stacktop, minus the args, minus 1 extra for the function object
  return true;
}

// mostly just asserting that this Value is indeed callable.
static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_CLOSURE:
        return call(AS_CLOSURE(callee), argCount);
      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }
      default:
        break; // Non-callable object type.
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}

// only nil and false are falsey in lox
static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static ObjUpvalue* captureUpvalue(Value* local) {
  // walk the open upvalue linked list to see if there is already an upvalue referring to this local
  ObjUpvalue* prevUpvalue = NULL;
  ObjUpvalue* upvalue = vm.openUpvalues;
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }
  // if so, just use that upvalue so multiple closures with the same closed-over variable
  // can see each others' writes to that upvalue
  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue* createdUpvalue = newUpvalue(local);
  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    // has been set in the while loop above. This keeps the list sorted
    prevUpvalue->next = createdUpvalue;
  }
  return createdUpvalue;
}

static void closeUpvalues(Value* last) {
  while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
    ObjUpvalue* upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}


// pretty straightforward: get the strings from the stack, allocate new object, then copy
// the characters into the new object and push the result back onto the stack
static void concatenate() {
  ObjString* b = AS_STRING(pop());
  ObjString* a = AS_STRING(pop());

  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1); // once again one extra for the null terminator
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);
  push(OBJ_VAL(result));
}

static InterpretResult run() {
  CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)

#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])

#define READ_STRING() AS_STRING(READ_CONSTANT())

#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtimeError("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(valueType(a op b)); \
    } while (false)

  for (;;) {
    // Section for debug tracing
    #ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code));
    #endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_NEGATE:
        if (!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
      case OP_ADD: { // more complex than the other binary ops because it needs to work on strings too
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else {
          runtimeError(
              "Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
      case OP_NIL:      push(NIL_VAL); break;
      case OP_TRUE:     push(BOOL_VAL(true)); break;
      case OP_FALSE:    push(BOOL_VAL(false)); break;
      case OP_NOT:
        push(BOOL_VAL(isFalsey(pop())));
        break;
      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
      case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
      case OP_PRINT: {
        printValue(pop());
        printf("\n");
        break;
      }
      case OP_POP: {
        pop();
        break;
      }
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING(); // get name object from the stack, top of stack is now the value
        tableSet(&vm.globals, name, peek(0)); // store name and value in the globals table
        pop(); // then pop the expression result off the stack
        break;
      }
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING(); // get name object from the stack
        Value value; // allocate memory to put the value into
        if (!tableGet(&vm.globals, name, &value)) { // read the value from the globals table
          runtimeError("Undefined variable '%s'.", name->chars); // raise error if not there
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value); // stick value onto the stack
        break;
      }
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING(); // read the name
        if (tableSet(&vm.globals, name, peek(0))) { // insert the new value and check if it was new
          tableDelete(&vm.globals, name); // it was new! not allowed, you need to define the var first. Delete the value again
          runtimeError("Undefined variable '%s'.", name->chars); // and report an error
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE(); // next byte is the operand, indicating which slot in the stack the local is at
        push(frame->slots[slot]); // read it and put it on top of the stack so other opcodes can access it
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE(); // next byte is the operand, indicating which slot in the stack the local is at
        vm.stack[slot] = peek(0); // write top of stack to the local, keeping it at top of stack too (assignments are expessions!!)
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(0))) frame->ip += offset;
        break;
      }
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        break;
      }
      case OP_CALL: {
        // the function object is below all the args on the stack, so we read how many args there are and then peek
        // that many values from the top. If there's not a callable object there, return with a runtime error, else call it
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1]; // reset the frame after the function call is done
        break;
      }
      case OP_CLOSURE: {
        // just reads the next byte, makes a closure out of it and sticks it on the stack
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure* closure = newClosure(function);
        push(OBJ_VAL(closure));
        // fill in the upvalue array for the closure
        for (int i = 0; i < closure->upvalueCount; i++) {
          uint8_t isLocal = READ_BYTE(); // seem quite wasteful to use and entire byte for a bool?
          uint8_t index = READ_BYTE();
          if (isLocal) {
            // make a new upvalue from the corresponding local
            closure->upvalues[i] = captureUpvalue(frame->slots + index);
          } else {
            // it comes from an enclosing scope, so just copy the pointer to an upvalue from the enclosing scope
            closure->upvalues[i] = frame->closure->upvalues[index];
          }
        }
        break;
      }
      case OP_GET_UPVALUE: {
        uint8_t slot = READ_BYTE(); // slot in the upvalue array where the pointer to the upvalue is kept
        push(*frame->closure->upvalues[slot]->location); // get the object pointed at and stick it on the stack
        break;
      }
      case OP_SET_UPVALUE: {
        uint8_t slot = READ_BYTE(); // slot in the upvalue array where the pointer to the upvalue is kept
        *frame->closure->upvalues[slot]->location = peek(0); // set the value at the pointed address to the value at the top of the stack
        break;
      }
      case OP_CLOSE_UPVALUE:
        closeUpvalues(vm.stackTop - 1);
        pop();
        break;
      case OP_RETURN: {
        Value result = pop(); // get result from the stack
        closeUpvalues(frame->slots); // close any upvalues remaining on the stack
        vm.frameCount--;
        if (vm.frameCount == 0) { // we returned from the top level script, so the program ends
          pop(); // be a good citizen and pop the top level function off the stack, which is now empty
          return INTERPRET_OK;
        }

        vm.stackTop = frame->slots; // "pop" the function object, its args and its local variables off the stack
        push(result); // put result back on the stack for further processing
        frame = &vm.frames[vm.frameCount - 1]; // go back to previous callframe
        break;
      }
    }
  }

#undef BINARY_OP
#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
}

InterpretResult interpret(const char* source) {
  // compile the source code into a new function, returning an error if compiling fails
  ObjFunction* function = compile(source);
  if (function == NULL) return INTERPRET_COMPILE_ERROR;

  // push the function object representing the script onto the bottom of the stack
  push(OBJ_VAL(function));
  ObjClosure* closure = newClosure(function); // make new closure to get it into the constant table
  pop(); // remove the newly created function object from the stack (and throw it away since we already have it)
  push(OBJ_VAL(closure)); // put it back onto the stack as a closure
  // then call that function (with argCount = 0, since the main script never has args)
  call(closure, 0);


  // run it
  return run();
}
