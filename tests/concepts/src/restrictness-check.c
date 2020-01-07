#include <stdio.h>
#include <stdlib.h>

int foo(int * restrict r, int * q) {
    return 42;
}

void good() {
    int a = 4;
    int b = 2;
    printf("%d\n", foo(&a, &b));
}

void bad1() {
    int a = 4;
    printf("%d\n", foo(&a, &a));
}

void bad2() {
    int * ptr = malloc(10);
    printf("%d\n", foo(ptr, ptr + 2));
    free(ptr);
}

int main () {
    good();
    bad1();
    bad2();
    return 0;
}
