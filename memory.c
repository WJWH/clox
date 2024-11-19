#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
    // always collect at every memory allocation in stress mode
    collectGarbage();
#endif
  }

  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, newSize);
  if (result == NULL) exit(1);
  return result;
}

void markObject(Obj* object) {
  if (object == NULL) return;    // nothing to mark
  if (object->isMarked) return;  // object is already marked, re-adding to the graystack might cause an infinite loop so we exit early
#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  object->isMarked = true;

  // grow the grayStack capacity if needed
  if (vm.grayCapacity < vm.grayCount + 1) {
    vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
    // notice we don't use the `reallocate` from the memory.c module here, since that one could itself
    // trigger a GC and we might get stuck in an infinite loop of GCs causing more GCs. Instead use normal realloc from libc
    vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
    if (vm.grayStack == NULL) exit(1); // if we couldn't allocate memory, exit the program
  }

  // increase the grayCount and stick the new object on top of the grayStack
  vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
  // non-object values like booleans and numbers have no dynamic allocation at all, so they don't need management by the GC
  if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
  for (int i = 0; i < array->count; i++) {
    markValue(array->values[i]);
  }
}

static void blackenObject(Obj* object) {
  switch (object->type) {
    // objects without references to other objects
    case OBJ_NATIVE:
    case OBJ_STRING:
      break;
    // only the closed value. If the upvalue is still open then closed is NULL and markValue won't do anything
    case OBJ_UPVALUE:
      markValue(((ObjUpvalue*)object)->closed);
      break;
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      markObject((Obj*)function->name);
      markArray(&function->chunk.constants);
      break;
    }
    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      markObject((Obj*)closure->function);
      for (int i = 0; i < closure->upvalueCount; i++) {
        markObject((Obj*)closure->upvalues[i]);
      }
      break;
    }
  }
}

// many types of object have special extra fields that need custom freeing logic,
// so we switch on object type to see what is needed
static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void*)object, object->type);
#endif
  switch (object->type) {
    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      FREE_ARRAY(char, string->chars, string->length + 1);
      FREE(ObjString, object);
      break;
    }
    case OBJ_FUNCTION: {
      // note: function name doesn't need explicit freeing since it's a Lox object and so will
      // be freed by the garbage collector
      ObjFunction* function = (ObjFunction*)object;
      freeChunk(&function->chunk);
      FREE(ObjFunction, object);
      break;
    }
    case OBJ_NATIVE:
      FREE(ObjNative, object);
      break;
    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      // the closure doesn't own the upvalue objects themselves, but it does own the array containing pointers to them
      FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
      FREE(ObjClosure, object);
      break;
    }
    case OBJ_UPVALUE:
      FREE(ObjUpvalue, object);
      break;
  }
}

static void markRoots() {
  // all values on the stack are roots
  for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
    markValue(*slot);
  }
  // the closures in the callframes used by the VM are also roots
  for (int i = 0; i < vm.frameCount; i++) {
    markObject((Obj*)vm.frames[i].closure);
  }
  // as well as the current list of open upvalues
  for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
    markObject((Obj*)upvalue);
  }

  markCompilerRoots();
  // globals are also roots
  markTable(&vm.globals);
}

static void traceReferences() {
  // while we have gray objects left, take the topmost one from the graystack and mark its children
  while (vm.grayCount > 0) {
    Obj* object = vm.grayStack[--vm.grayCount];
    blackenObject(object);
  }
}

static void sweep() {
  // walk the entire list of objects known to the VM, removing and freeing any unmarked objects
  Obj* previous = NULL;
  Obj* object = vm.objects;
  while (object != NULL) {
    if (object->isMarked) {
      object->isMarked = false; // reset isMarked for the next run
      previous = object;
      object = object->next;
    } else {
      Obj* unreached = object;
      object = object->next;
      if (previous != NULL) { // special case to check if we're at the head of the list
        previous->next = object;
      } else {
        vm.objects = object;
      }

      freeObject(unreached);
    }
  }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
#endif
  markRoots();
  traceReferences();
  tableRemoveWhite(&vm.strings);
  sweep();
}

// walk the linked list of allocated objects, calling freeObject each one
void freeObjects() {
  Obj* object = vm.objects;
  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
  // also free the entire grayStack
  free(vm.grayStack);
}
