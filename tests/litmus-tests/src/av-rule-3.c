#include <stdlib.h>
#include <stdio.h>


#define GT(x, y, code)                           \
    if (x > y) {                                 \
        code;                                    \
    }                                            \

#define GT_PLUS_5(x, y, code) \
    GT(x, y, GT(x, y + 1, GT(x, y + 2, GT(x, y + 3, GT(x, y + 4, code)))))

#define GT_PLUS_10(x, y, code) \
    GT_PLUS_5(x, y, GT_PLUS_5(x, y + 5, code))


void good(int x) {
    GT_PLUS_10(x, 0,
               GT_PLUS_10(x, 10,
                          GT_PLUS_10(x, 20,
                                     GT_PLUS_10(x, 30,
                                                printf("ok!\n")))));
}

void bad(int x) {
    GT_PLUS_10(x, 0,
               GT_PLUS_10(x, 10,
                          GT_PLUS_10(x, 20,
                                     GT_PLUS_10(x, 30,
                                                GT_PLUS_10(x, 40,
                                                           printf("ok!\n"))))));
}


int main() {
    good(42);
    bad(42);
    return 0;
}
