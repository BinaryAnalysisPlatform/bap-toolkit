#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>


jmp_buf buf;

void fun(int n) {
    printf("%d\n", n);
    longjmp(buf, n + 1);
}

void bad () {
    int i = 0;
    if (setjmp(buf) != 5) {
        fun(++i);
    }
}

int main() {
    bad();
    return 0;
}
