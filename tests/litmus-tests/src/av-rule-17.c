#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

void bad() {
    FILE * fp;
    fp = fopen("file-not-exists.txt", "r");
    printf("Errno: %d\n ", errno);
}


int main() {
    bad();
    return 0;
}
