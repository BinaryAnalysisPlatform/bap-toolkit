#include <stdio.h>
#include <stdlib.h>


int bad(int n) {
    if (n < 0)
        bad(n + 1);
    printf("%d\n", n);
}

int main() {
    bad (42);
    return 0;
}
