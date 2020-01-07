#include <stdlib.h>

static int MyTrue = 1;

void good() {
    int *p = malloc(42);
    if (MyTrue) {
        free(p);
        return;
    }
    free(p);
}

void bad() {
    int *p = malloc(42);
    if (MyTrue) {
        free(p);
    }
    free(p);
}

int main() {
    good();
    bad();
    return 0;
}
