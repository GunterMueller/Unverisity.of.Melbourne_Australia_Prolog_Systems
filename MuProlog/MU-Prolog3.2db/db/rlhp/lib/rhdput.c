#include "rel.h"

rhdput(rel)
REL *rel;
{
	rewind(rel->descf);
	fwrite(&rel->hd, sizeof(struct header), 1, rel->descf);
}
