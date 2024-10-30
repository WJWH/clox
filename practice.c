#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct node {
  char* str;
  struct node* previous;
  struct node* next;
} node;

typedef struct doubly_linked_list {
  int size;
  struct node* first;
  struct node* last;
} doubly_linked_list;

// don't run this on an already initialized list or you'll leak all the memory
void initialize_doubly_linked_list(doubly_linked_list* dll){
  dll->size = 0;
  dll->first = NULL;
  dll->last = NULL;
}

void insert(doubly_linked_list* dll, char* new_str){
  node* first_node = dll->first;

  node *new_node = malloc(sizeof(node));
  new_node->str = new_str;
  new_node->previous = NULL; // insert at start of list so no node before this
  new_node->next = first_node;

  if(dll->size == 0){
    // first node inserted, so it's immediately also the last node
    dll->last = new_node;
  }
  else {
    // there were other nodes already, so we should add the backlink as well
    first_node->previous = new_node;
  }
  dll->first = new_node;
  dll->size += 1;
}

void print_first(doubly_linked_list* dll){
  printf("%s\n", dll->first->str);
}

node* find_first(doubly_linked_list* dll, char* str_to_find){
  node* current_node = dll->first;
  while(current_node != NULL){
    if (strcmp(current_node->str,str_to_find) == 0){
      return current_node;
    }
    current_node = current_node->next;
  }
}

int xmain(int argc, char const *argv[]) {
  doubly_linked_list dll;
  initialize_doubly_linked_list(&dll);
  printf("Linked list size: %d\n", dll.size);
  insert(&dll, "borp");
  print_first(&dll);
  printf("Linked list size: %d\n", dll.size);
  insert(&dll, "birp");
  print_first(&dll);
  printf("Linked list size: %d\n", dll.size);

  // tests for finding
  node* borpnode = find_first(&dll,"borp");
  printf("string at this node: %s\n", borpnode->str);
  // since it's the last node, the next node should be null
  printf("next node is null: %s\n", borpnode->next == NULL ? "true" : "false");
  // previous node should be birp
  printf("string at previous node: %s\n", borpnode->previous->str);
  return 0;
}

