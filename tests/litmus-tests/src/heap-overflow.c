#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void good(const int *src, int *dst, size_t len) {
    memcpy(dst, src, len);
}

void bad_read(const int *q, size_t len) {
    void *p = memchr(q, 1, len);
    if (!p)
        printf("Not found!\n");
}

void bad_write(const int *src, int *dst, size_t len) {
    memcpy(dst, src, len);
}

int main() {
    size_t big = 42;
    size_t small = 41;
    int *src = malloc(big);
    int *dst = malloc(small);
    good(src, dst, small);
    bad_read(dst, big);
    bad_write(src, dst, big);
    free(src);
    free(dst);
    return 0;
}
