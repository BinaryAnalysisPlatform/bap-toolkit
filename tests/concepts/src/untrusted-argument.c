#include <stdlib.h>
#include <stdio.h>

void bad() {
    int x = atoi("42");
    int *ptr = malloc(x);
    free(ptr);
}

void good() {
    int x = atoi("42");
    if (x <= 0)
        return;
    int *ptr = malloc(x);
    free(ptr);
}

int main() {
    good();
    bad();
    return 0;
}
