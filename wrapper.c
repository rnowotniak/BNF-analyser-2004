#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

#define SEKUNDY 4
#define ANALIZ "analysebnf"
#ifndef ANALIZ_PATH
#define ANALIZ_PATH "../bin"
#endif

int main(int argc, char **argv)
{
        struct rlimit LIM;

        LIM.rlim_cur = SEKUNDY;
        LIM.rlim_max = SEKUNDY;

        setrlimit(RLIMIT_CPU, &LIM);

        argv[0] = ANALIZ;
        execv(ANALIZ_PATH "/" ANALIZ, argv);

        perror("execv");
        exit(253);
}


