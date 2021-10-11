#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint64_t *halt;

void
printInt(uint64_t* env, int64_t n)
{
	printf("%i\n", n);
	exit(0);
}
