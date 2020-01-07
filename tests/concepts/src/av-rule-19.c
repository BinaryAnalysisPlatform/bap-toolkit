#include <stdio.h>
#include <stdlib.h>
#include <locale.h>

void bad () {
    setlocale(LC_MESSAGES, "de_DE.utf8");
}

int main() {
    bad();
    return 0;
}
