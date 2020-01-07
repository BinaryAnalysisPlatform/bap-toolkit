#include <stdio.h>
#include <stdlib.h>

void bad (int x) {
    if (x > 10)
        abort();
    if (x > 5)
        exit(1);

    char buf[10];

    char *a = getenv("AA");
    system("echo");
}

int main() {
    bad(2);
    return 0;
}
