#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>

void bad() {
    char s[100];
    printf("%s\n", getcwd(s, 100));

    chdir("..");
    printf("%s\n", getcwd(s, 100));
}

void good() {
    int x = chdir("..");
    if (x == -1)
        printf("failed to change dir");
}

int main() {
    good ();
    bad();
    return 0;
}
