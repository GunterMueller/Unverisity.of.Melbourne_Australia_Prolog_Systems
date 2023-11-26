#include <stdio.h>

rerror(s)
char *s;
{
	fprintf(stdout, "?-db_error(%s).\n", s);
	fflush(stdout);
}
