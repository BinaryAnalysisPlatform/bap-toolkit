#include <stdio.h>
#include <stdlib.h>

void bad () {
    int x = atoi("42");
    float y = atof("42.2");
    long z = atol("42");
}

int main() {
    bad();
    return 0;
}
