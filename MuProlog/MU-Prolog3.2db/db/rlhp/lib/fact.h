#define MAXKEYS	15	/* maximum number of keys */
#define MAXID	32	/* maximum identifier length */
#define BUFSZ	128	/* internal buffer size */

typedef struct {
	char *fname;			/* fact (relation) name */
	int nkeys;			/* number of fields */
	char *field[MAXKEYS];		/* fields */
	int fsz[MAXKEYS];		/* field sizes - incl nulls */
} FACT;

extern FACT *factget();
extern FACT *factin();
