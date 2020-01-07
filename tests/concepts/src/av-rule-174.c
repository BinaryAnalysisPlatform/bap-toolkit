#include <stdio.h>

void good() {
    int *x = NULL;
    int b = 0;
    if (!x)
        b = *x;
}

void bad() {
    int *x = NULL;
    int b = 0;
    b = *x;
}

int main() {
    good();
    bad();
    return 0;
}
