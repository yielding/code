#include <stdio.h>
#include <sys/sdt.h>

int main(int argc, char *argv[])
{
    int i;
    int characters, lines, words;
    characters = lines = words = 0;

    while (1) {
        if ((i = getchar()) == EOF) {
            DTRACE_PROBE1(simple, saw__line, lines);
            break;
        }

        characters++;
        if (i == '\\n') {
            lines++;
            DTRACE_PROBE1(simple, saw__line, lines);
            continue;
        }

        if (isblank(i)) 
            continue;

        words++; 
        while (1) {
            if ((i = getchar()) == EOF) {
                DTRACE_PROBE1(simple, saw__word, words);
                break;
            }

            characters++;
            if (i == '\\n') { 
                DTRACE_PROBE1(simple, saw__word, words);
                lines++;
                DTRACE_PROBE1(simple, saw__line, lines);
                break;
            }

            if (isblank(i)) { 
                DTRACE_PROBE1(simple, saw__word, words);
                break;
            }
        }
    }

    printf("%8d %8d %8d\\n", lines, words, characters);

    exit(0);
}
