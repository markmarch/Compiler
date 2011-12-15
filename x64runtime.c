#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct{ long _size; void* _data; } _ArrayHandle;
typedef void _RecordHandle;

/* -------- intrinsics -------- */
char* append(char* lhs, char* rhs) {
  size_t lhsLen = strlen(lhs);
  size_t rhsLen = strlen(rhs);
  char* result = (char*)malloc(lhsLen + rhsLen + 1);
  strcpy(result, lhs);
  strcpy(result + lhsLen, rhs);
  result[lhsLen + rhsLen] = '\0';
  return result;
}

long bool2int(long b) { return b; }
char* bool2string(long b) { return b ? "true" : "false"; }

long int2bool(long i) { return i == 0 ? 0 : 1; }
char* int2string(long i) {
  char buf[20];
  long len = sprintf(buf, "%ld", i);
  char* result = malloc(len + 1);
  strncpy(result, buf, len);
  result[len] = '\0';
  return result;
}

long length(char* s) { return strlen(s); }

_ArrayHandle* newArray(long eSize, long aSize) {
  _ArrayHandle* result = (_ArrayHandle*)malloc(sizeof(_ArrayHandle));
  result->_size = aSize;
  result->_data = malloc(eSize * aSize);
  return result;
}

_RecordHandle* newRecord(long rSize) { return malloc(rSize); }

void print(char* s) { printf("%s", s); }

_ArrayHandle* range(long start, long end) {
  long eSize = sizeof(long);
  long aSize = (start < end) ? (end - start) : 0;
  long* data = (long*)malloc(eSize * aSize);
  _ArrayHandle* result = (_ArrayHandle*)malloc(sizeof(_ArrayHandle));
  long i;
  result->_size = aSize;
  for (i=start; i<end; i++)
    data[i - start] = i;
  result->_data = data;
  return result;
}

long size(_ArrayHandle* a) { return a->_size; }

long string2bool(char* s) {
  if (0 == strcmp(s, "") || 0 == strcmp(s, "0") || 0 == strcmp(s, "false"))
    return 0;
  return 1;
}
long string2int(char* s) { return atoi(s); }

long stringEqual(char* lhs, char* rhs) { return 0 == strcmp(lhs, rhs); }
