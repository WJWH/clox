#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

// linear probing until we find the key or an empty bucket
// if we find an empty bucket before finding the key, the key is not yet present in the table
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
  uint32_t index = key->hash % capacity;
  Entry* tombstone = NULL; // used to store whether we've passed a tombstone during this search

  for (;;) {
    Entry* entry = &entries[index];
    if (entry->key == NULL) { // might be a tombstone
      if (IS_NIL(entry->value)) { // real empty values have value == nil, tombstones have BOOL_VAL(true)
        // Empty entry.
        return tombstone != NULL ? tombstone : entry; // return address of tombstone so it can be reused
      } else {
        // We found a tombstone.
        if (tombstone == NULL) tombstone = entry; // set tombstone (unless it was already set) and continue searching
      }
    } else if (entry->key == key) {
      // We found the key.
      return entry;
    }

    index = (index + 1) % capacity;
  }
}

static void adjustCapacity(Table* table, int capacity) {
  // allocate a new array
  Entry* entries = ALLOCATE(Entry, capacity);
  // set all entries in the new array to the empty entry
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  // for all non-empty entries, insert them into the new array
  table->count = 0;
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL) continue;

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  // now we can free the memory for the old array
  FREE_ARRAY(Entry, table->entries, table->capacity);
  // and set the new values for entries and capacity
  table->entries = entries;
  table->capacity = capacity;
}

bool tableGet(Table* table, ObjString* key, Value* value) {
  if (table->count == 0) return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;

  *value = entry->value;
  return true;
}

bool tableSet(Table* table, ObjString* key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  Entry* entry = findEntry(table->entries, table->capacity, key); // find a bucket for this key
  bool isNewKey = entry->key == NULL; // key not yet present
  // updates don't increase the count but new insertions do. Overwriting a tombstone also doesn't increase the count
  if (isNewKey && IS_NIL(entry->value)) table->count++;

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
  if (table->count == 0) return false;

  // Find the entry.
  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;

  // Place a tombstone in the entry.
  entry->key = NULL;
  entry->value = BOOL_VAL(true);
  return true;
}

// add all the entries of the "from" table to the "to" table
void tableAddAll(Table* from, Table* to) {
  for (int i = 0; i < from->capacity; i++) {
    Entry* entry = &from->entries[i];
    if (entry->key != NULL) {
      tableSet(to, entry->key, entry->value);
    }
  }
}
