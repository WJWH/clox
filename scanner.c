#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
  const char* start;
  const char* current;
  int line;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}

static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}

static bool isAtEnd() {
  return *scanner.current == '\0'; // encountered the NULL byte terminator, source string is at its end
}

static char advance() {
  char currentChar = *scanner.current;
  scanner.current++;
  // return scanner.current[-1]; // wtf is this weird syntax (I know it works, but it's definitely not very clear IMO)
  return currentChar
}

static char peek() {
  return *scanner.current;
}

static char peekNext() {
  if (isAtEnd()) return '\0';
  return scanner.current[1];
}

static bool match(char expected) {
  if (isAtEnd()) return false;
  if (*scanner.current != expected) return false;
  scanner.current++;
  return true;
}

static Token makeToken(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (int)(scanner.current - scanner.start);
  token.line = scanner.line;
  return token;
}

static Token errorToken(const char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = (int)strlen(message);
  token.line = scanner.line;
  return token;
}

static void skipWhitespace() {
  for (;;) {
    char c = peek();
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        advance(); // all previous cases fall through to here, notice the lack of `break`s
        break;
      case '\n': // newlines
        scanner.line++; // also increase line nr after newline
        advance();
        break;
      case '/': // comments
        if (peekNext() == '/') {
          // A comment goes until the end of the line.
          while (peek() != '\n' && !isAtEnd()) advance();
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}

static Token string() {
  while(peek() != '"' && !isAtEnd()) { // while the string is not ended yet, keep advancing
    if peek() == '\n' current.line++;  // and update line nr as needed
    advance();
  }

  // now that the previous loop is done, either we're at the end of the string or at the end of the source code
  if isAtEnd() return errorToken("Unterminated string.")

  advance(); // consume the closing quote
  makeToken(TOKEN_STRING);
}

static Token number() {
  while(isDigit(peek())) { // while the number is not ended yet, keep advancing
    advance();
  }

  // maybe a fractional part?
  if(peek() == '.' && isDigit(peekNext())) { // remember that you can have numbers like 5. and they will be parsed as 5.0
    advance(); // consume the '.'
    while(isDigit(peek())) { // while the fractional part is not ended yet, keep advancing
      advance();
    }
  }

  makeToken(TOKEN_NUMBER);
}

Token scanToken() {
  skipWhitespace();
  scanner.start = scanner.current;

  if (isAtEnd()) return makeToken(TOKEN_EOF);

  char c = advance();
  if(isDigit(c)) return number();
  switch (c) {
    // single character tokens
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);
    // literals
    case '"': return string();
  }

  return errorToken("Unexpected character.");
}
