#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void bad () {
    time_t timer;
    time(&timer);
    struct tm y2k = {0};
    y2k.tm_hour = 0;   y2k.tm_min = 0; y2k.tm_sec = 0;
    y2k.tm_year = 100; y2k.tm_mon = 0; y2k.tm_mday = 1;
    int seconds = difftime(timer,mktime(&y2k));

    struct tm *loc, *gmt;
    loc = localtime (&timer);
    gmt = gmtime(&timer);
    char *ta = ctime(&timer);
    char *tb = asctime(loc);
    char buf[20];

    strftime (buf,20,"Now it's %I:%M%p.", loc);

    clock_t t;
    t = clock();
}

int main() {
    bad();
    return 0;
}
