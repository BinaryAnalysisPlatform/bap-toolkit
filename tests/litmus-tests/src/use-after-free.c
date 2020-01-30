#include <stdlib.h>
#include <stdio.h>

static int MyTrue = 1;

void good() {
    int *p = malloc(42);
    if (MyTrue) {
        free(p);
        return;
    }
    printf("%d\n", *p);
}

void bad() {
    int *p = malloc(42);
    if (MyTrue) {
        free(p);
    }
    printf("%d\n", *p);
}

int main() {
    good();
    bad();
    return 0;
}
