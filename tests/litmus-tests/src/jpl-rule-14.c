#include <stdlib.h>
#include <stdio.h>

void good() {
    int x = printf("good");
    if (x != 4) {
        exit(1);
    }
}

void bad() {
    int x = printf("bad!\n");
}

int main() {
    good();
    bad();
    return 0;
}
