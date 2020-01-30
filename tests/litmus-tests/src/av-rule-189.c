#include <stdio.h>
#include <stdlib.h>

void bad(int argc, char **argv) {
    int optind = 3;

    if (argc <= optind) {
        argv[2] = "y";
    }

    while (1) {
        int i;
        for (i = optind; i < argc; i++)
            if (fputs (argv[i], stdout) == EOF)
                printf("error!\n");
    }
}


int main(int argc, char **argv) {
    bad(argc,argv);
    return 0;
}
