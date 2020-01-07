#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>


void with_file() {
    char buf[10];

    FILE *f = fopen("foo.txt", "rw");
    rewind(f);
    fprintf(f,"ff %d", 42);
    int p = ftell(f);
    fseek(f,p, SEEK_END);
    fread(buf, 1, 3, f);
    fwrite(buf, 1, 3, f);
    fputc('f',f);
    fputs("foo",f);
    fgetc(f);
    fgets(buf,2,f);

    fclose(f);
    f = freopen("foo.txt", "rw", f);
    char buffer[BUFSIZ];
    setbuf (f, buffer);
    setvbuf (f, buffer, _IOFBF , 1024);
    fpos_t pos;
    fgetpos(f, &pos);
    fsetpos(f, &pos);
    fscanf(f, buf, "%s");

    feof(f);
    ferror(f);
    clearerr(f);
    fflush(f);
    rename("foo.txt", "bar.txt");
    remove("bar.txt");
    fclose(f);
}

void with_va_list ( const char * format, ... ) {
    char buf[10];
    va_list args;
    va_start (args, format);
    vprintf (format, args);
    vsprintf(buf, format, args);

    FILE *f = fopen("bar.txt", "rw");
    vfprintf(f, format, args);
    fclose(f);

    va_end (args);
}

void with_io() {
    char buf[10];

    printf("foo\n");
    sprintf("foo, %s", "aa");

    putc('b', stdout);
    puts("bar");
    putchar('a');
    char a = getc(stdin);
    ungetc(a,stdout);
    getchar();

    char c;
    sscanf(buf, "%c", &c);
    scanf(buf, "%c", &c);
    perror("BUG!\n");
}

void bad () {
    with_file();
    with_va_list("%s, %s", "foo", "bar");
    with_io();

    char buf[10];
    char *c = tmpnam(buf);
    FILE *f = tmpfile();
}

int main() {
    bad();
    return 0;
}
