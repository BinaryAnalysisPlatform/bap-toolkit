#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

void sighandler(int signum) {
   printf("Caught signal %d\n", signum);
   exit(0);
}

void bad (int n) {
    if (n > 10)
        raise(SIGINT);
}

int main() {
    signal(SIGINT, sighandler);
    bad(42);
    return 0;
}
