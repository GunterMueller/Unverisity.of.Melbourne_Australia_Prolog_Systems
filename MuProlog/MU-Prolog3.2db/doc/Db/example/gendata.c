#include <stdio.h>
#include <ctype.h>

main()
{
	int	i;
	FILE	*fp;
	char	word[32];

	if ((fp = fopen("/usr/dict/words","r")) == NULL) {
		fprintf(stderr,"Can't find words file\n");
		exit(1);
	}

	printf("lib db.\n db_cons(db).\n consult(user).\n");
	for (i = 0; i < 999; i++)
	{
		if ((i%5) == 0) {
			while (fgets(word,30,fp))
				if (islower(word[0]) && strlen(word) > 10)
					break;
			word[strlen(word)-1] = '\0';
		}
		printf("simc(%d,%c,%s).\n",i,'a'+(i%26),word);
		printf("rlhp(%d,%c,%s).\n",i,'a'+(i%26),word);
	}
}
