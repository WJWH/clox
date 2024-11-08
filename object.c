#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size); // get some memory
  object->type = type; // pre-set the type, rest of the fields will be done in allocateString/etc
  return object;
}

ObjString* copyString(const char* chars, int length) {
  char* heapChars = ALLOCATE(char, length + 1); // allocate some new memory
  memcpy(heapChars, chars, length); // copy the chars to the new memory
  heapChars[length] = '\0'; // add null terminator
  return allocateString(heapChars, length);
}

static ObjString* allocateString(char* chars, int length) {
  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  return string;
}

ObjString* takeString(char* chars, int length) {
  return allocateString(chars, length);
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
  }
}