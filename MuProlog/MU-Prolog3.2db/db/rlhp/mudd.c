#include <stdio.h>
#include <signal.h>
#include "lib/rel.h"
#include "mudd.h"

main(argc, argv)
int argc;
char *argv[];
{
	REL *rel;
	FACT *fact;
	register i;
	int type;

	signal(SIGINT, SIG_IGN);
	if (argc != 3) {
		rerror("usage");
		/* Usage: mudd db rel */
		exit(1);
	}
	if ((rel = ropen(argv[1], argv[2])) == NULL) {
		rerror("open");
		exit(1);
	}
	while ((type = getchar()) != EOF && (fact = factget(stdin)) != NULL) {
		switch ((char) type) {
		case DB_QY:
			rquery(rel, fact);
			printf("?-db_end.\n");
			break;
		case DB_QYALL:
			rqueryall(rel, fact);
			printf("?-db_end.\n");
			break;
		case DB_RT:
			retract(rel, fact);
			printf("db_end.\n");
			break;
		case DB_RTALL:
			retractall(rel, fact);
			printf("db_end.\n");
			break;
		case DB_ASS:
			rassert_sync(rel, fact);
			break;
		case DB_ASA:
			rassert_async(rel, fact);
			break;
		default:
			rerror("command");
			rclose(rel);
			exit(1);
		}
		fflush(stdout);
	}
	rclose(rel);
}
