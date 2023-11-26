/*
 * fact.c  -  deductive database package (fact operations)
 *
 * $Header: fact.c,v 1.5 85/12/06 15:09:55 jas Exp $
 * $Log:	fact.c,v $
 * Revision 1.5  85/12/06  15:09:55  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:56  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:17  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:50:47  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:33  jas
 * Initial revision
 * 
 * 
 */

#include "muddlib.h"

/*
 * fact_read:
 *	Read a fact as null-separated char strings from query input stream
 *	Returns number of key values actually read in
 *	Assumes that the Fact structure passed has a big enough buffer!
 *
 *	Input has form: key1\0key2\0key3\0...keyn\0
 *	Output has form: "rel(key1,key2,key3,...,keyn)"
 */
Int
fact_read(stream, fact, rel_name)
FileP stream;
String fact;
String rel_name;
{
	r_Int	i, ch, nchars;
	r_Char	*buf = fact;

	/*
	 * Read & check relation name
	 */
	i = 0;
	while ((ch = getc(stream)) != '\0')
		if (feof(stream))
			return(-1);
		or (i++ < MAXRNAME-1)
			*buf++ = ch;
	*buf = '\0';
	if (!streq(rel_name,fact)) {
		error("wrong relation");
		return(-1);
	}
	*buf++ = '(';

	/*
	 * Read key values, one at a time
	 */
	for (i = 0; /*forever*/; i++) {
		nchars = 0;
		while ((*buf++ = getc(stream)) != ChNULL) {
			nchars++;
			if (feof(stream))
				return(-1);
		}
		if (nchars == 0)
			break;
		buf--;
		*buf++ = ',';
	}
	buf--;
	buf--;
	*buf++ = ')';
	*buf = ChNULL;

	return(i);
}

/*
 * fact_write:
 *	Write a fact as dot-terminated term on answer output stream
 */
void
fact_write(stream, fact)
FileP stream;
String fact;
{
	fprintf(stream, "%s.\n", fact);
}
