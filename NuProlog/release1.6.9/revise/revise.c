#include <stdio.h>

#define ED		"/usr/bin/vi"
#define NC		"/mip/jas/bin/nc"
#define NA		"/mip/jas/bin/na"
#define BIN		"/mip/jas/nl/revise"

/* trap "/bin/rm -f @.nl @.ns" 0 1 2 3 15 */

main(argc, argv)
	int argc;
	char *argv[];
{
	char *make_list();

	char *fs[50], ans, *tmp, *name, *srcs, *asms, cmd[512], line[100];
	int i, filec = 0, tag = 0, file = 0;

	argc--, argv++;
	for( ; argc > 0 ; argc--, argv++)
		if(!strcmp(argv[0], "-t")) {
			tag = 1; name = argv[1];
			argc--, argv++;
		} else if(!strcmp(argv[0], "-f")) {
			file = 1; name = argv[1];
			argc--, argv++;
		} else {
			fs[filec] = argv[0];
			filec++;
		}
	if(filec == 0) {
		printf("Usage : revise [-t tag | -f file] base_names\n");
		exit(1);
	}
	srcs = make_list(fs, ".nl", filec);
	asms = make_list(fs, ".ns", filec);
	sprintf(cmd, "%s/nlcp -m%s %s", BIN, "\'%--\'", srcs);
#ifdef Debug
	printf("%s\n", cmd);
#else
	system(cmd);
#endif
	printf("Edit tag? [y] ");
	gets(line); ans = *line; *line = '\0';

restart:
	if(file == 0 && tag == 0) {
		if(ans == 'n')
			file = 1;
		else
			tag = 1;
		for(name = line ; *name == '\0' ; ) {
			if(file == 1)
				printf("File name? ");
			else
				printf("Tag name? ");
			gets(line);
			for( ; *name == ' ' ; name++)
				;
		}
		for(tmp = name ; *tmp != ' ' && *tmp != '\0' ; tmp++)
			;
		*tmp = '\0';
	}
	if(file == 1) {
		for(i = 0 ; i < filec ; i++)
			if(!strcmp(fs[i], name))
				file = 0;
		if(file == 1) {
			printf("Invalid file name - valid names are\n\t");
			for(i = 0 ; i < filec ; i++)
				printf("%s ", fs[i]);
			printf("\n");
			file = 0; *name = '\0';
			goto restart;
		}
		sprintf(cmd, "%s %s.nl", ED, name);
	} else
		sprintf(cmd, "%s -t %s", ED, name);
#ifdef Debug
	printf("%s\n", cmd);
#else
	system(cmd);
#endif
	printf("Edit tag? ");
	gets(line); ans = *line; *line = '\0';
	if(ans != '\0') {
		tag = file = 0;
		goto restart;
	}
	sprintf(cmd, "%s/nldiffs %s > @.nl", BIN, srcs);
#ifdef Debug
	printf("%s\n", cmd);
#else
	system(cmd);
#endif
	sprintf(cmd, "%s -c @.nl", NC);
#ifdef Debug
	printf("%s\n", cmd);
#else
	system(cmd);
#endif
	sprintf(cmd, "%s/nsfix @.ns %s", BIN, asms);
#ifdef Debug
	printf("%s\n", cmd);
#else
	system(cmd);
#endif
	sprintf(cmd, "%s %s", NA, asms);
#ifdef Debug
	printf("%s\n", cmd);
#else
	system(cmd);
#endif
}


char *make_list(fs, tag, c)
	char *fs[], *tag;
	int c;
{
	char *malloc(), *init, *tmp;
	unsigned l = 0, tl, num;

	tl = strlen(tag) + 1;
	for(num = 0 ; num < c ; num++) {
		l += tl + strlen(fs[num]);
	}
	init = tmp = malloc(l);
	for(num = 0 ; num < c ; num++) {
		strcpy(tmp, fs[num]); tmp += strlen(fs[num]);
		strcpy(tmp, tag); tmp += strlen(tag);
		*tmp++ = ' ';
	}
	*(--tmp) = '\0';
	return(init);
}
