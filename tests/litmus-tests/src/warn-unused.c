#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* used as an argument of system call */
void good1 () {
    int x = atoi("42");
    putchar(x);
}

/* has an influence to a control flow */
void good2 () {
    int x = atoi("42");
    if (x > 0) {
        return;
    }
    printf("good!\n");
}

void bad1 () {
    int x = atoi("42");
}

void even_is_used_as_argument(int arg) {
    printf("but not as argument of any system call\n");
}

void bad2() {
    int x = atoi("42");
    even_is_used_as_argument(x);
}

int main() {
    good1();
    good2();
    bad1();
    bad2();
    return 0;
}
