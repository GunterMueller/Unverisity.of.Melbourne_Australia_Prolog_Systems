/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"
#include <sys/stat.h>
#ifndef MACHINE_SUN4_SOLARIS
#	include <sys/dir.h>
#endif
#include <pwd.h>
#include <sys/times.h>
#include <signal.h>
#ifdef BSD4
#	include <sys/time.h>
#	include <sys/resource.h>
#else
#	ifndef MACHINE_SGI
#		include <dirent.h>
#	endif
#	include <time.h>
#	include <sys/utsname.h>
#endif

/*
 * Note that vfork is not suitable for the Prolog fork/* predicates,
 * but would be desirable for exec/2 and system/3.
 */

#ifndef PROTO_GETPWUID
	struct passwd *getpwuid(uid_t);
#endif
#ifndef PROTO_GETPWNAM
	struct passwd *getpwnam(const char *);
#endif
#ifndef PROTO_LOCALTIME
	struct tm *localtime();
#endif

/*
 * BUG!  All the direct calls to unix library predicates will probably
 * have to be changed on systems where Word and int are different.
 */
#define vtNON 0
#define vtINT 1
#define vtATM 2
#define vtOBJ 3
#define vtLST 4

Word unix_environ(), unix_getgroups(), unix_hostname(), unix_mkdir();
Word unix_random(), unix_rename(), unix_rmdir(), unix_stat();
Word unix_getlogin(), unix_getwd(), unix_getpwuid(), unix_getpwnam();
Word unix_sleep(), unix_time(), unix_truncate(), unix_umask(), unix_wait();
Word unix_readdir(), unix_exec(), unix_statistics(), unix_fseek();
Word unix_ftell(), unix_fesq(), unix_setSignal(), unix_chmod();

SysCall SystemCalls[NSYSCALLS] = {
	{access,			vtINT,	2,	{vtATM, vtINT, vtNON}},
	{chdir,				vtINT,	1,	{vtATM, vtNON, vtNON}},
	{unix_chmod,		vtINT,	2,	{vtATM, vtINT, vtNON}},
	{unix_environ,		vtOBJ,	0,	{vtNON, vtNON, vtNON}},
	{getegid,			vtINT,	0,	{vtNON, vtNON, vtNON}},
	{getgid,			vtINT,	0,	{vtNON, vtNON, vtNON}},
	{unix_getgroups,	vtOBJ,	0,	{vtNON, vtNON, vtNON}},
	{unix_getlogin,		vtATM,	0,	{vtNON, vtNON, vtNON}},
	{getpid,			vtINT,	0,	{vtNON, vtNON, vtNON}},
	{getppid,			vtINT,	0,	{vtNON, vtNON, vtNON}},
	{unix_getpwuid,		vtOBJ,	1,	{vtINT, vtNON, vtNON}},
	{unix_getpwnam,		vtOBJ,	1,	{vtATM, vtNON, vtNON}},
	{getuid,			vtINT,	0,	{vtNON, vtNON, vtNON}},
	{geteuid,			vtINT,	0,	{vtNON, vtNON, vtNON}},
	{unix_getwd,		vtATM,	0,	{vtNON, vtNON, vtNON}},
	{unix_hostname,		vtATM,	0,	{vtNON, vtNON, vtNON}},
	{kill,				vtINT,	2,	{vtINT, vtINT, vtNON}},
	{link,				vtINT,	2,	{vtATM, vtATM, vtNON}},
	{unix_mkdir,		vtINT,	2,	{vtATM, vtINT, vtNON}},
	{unix_random,		vtINT,	0,	{vtNON, vtNON, vtNON}},
	{unix_rename,		vtINT,	2,	{vtATM, vtATM, vtNON}},
	{unix_rmdir,		vtINT,	1,	{vtATM, vtNON, vtNON}},
	{unix_sleep,		vtNON,	1,	{vtINT, vtNON, vtNON}},
	{unix_stat,			vtOBJ,	1,	{vtATM, vtNON, vtNON}},
	{unix_time,			vtOBJ,	2,	{vtINT, vtINT, vtNON}},
	{unix_truncate,		vtINT,	2,	{vtATM, vtINT, vtNON}},
	{unix_umask,		vtINT,	1,	{vtINT, vtNON, vtNON}},
	{unlink,			vtINT,	1,	{vtATM, vtNON, vtNON}},
	{unix_wait,			vtOBJ,	0,	{vtNON, vtNON, vtNON}},
	{unix_readdir,		vtOBJ,	1,	{vtATM, vtNON, vtNON}},
	{fork,				vtINT,	0,	{vtNON, vtNON, vtNON}},
	{system,			vtINT,	1,	{vtATM, vtNON, vtNON}},
	{unix_exec,			vtINT,	2,	{vtATM, vtLST, vtNON}},
	{unix_statistics,	vtOBJ,	0,	{vtNON, vtNON, vtNON}},
	{unix_fseek,		vtINT,	3,	{vtINT, vtINT, vtINT}},
	{unix_ftell,		vtINT,	1,	{vtINT, vtNON, vtNON}},
	{unix_fesq,			vtOBJ,	0,	{vtNON, vtNON, vtNON}},
	{unix_setSignal,	vtINT,	2,	{vtINT, vtOBJ, vtNON}},
};

int
runv(file, argv)
char *file, *argv[];
{
	int status;
	register int pid;

	switch(pid = fork()) {
	when -1:
		warning("Can't fork()");
		return(-1);
	when 0: {
		register int i;

		for(i = 3; i < nstreams; i++)
			close(i);
		execv(file, argv);
		warning("Can't exec");
		exit(1);		/* Not a panic in the child */
	}

	break;
	default: {
		register int w;

		do {
			w = wait(&status);
			if(w == -1)
				panic("System error in runv()");
		} while(w != pid);
		return(status);
	}
	}
}

#ifndef BSD4
static int
ex0(file, arg0)
char *file, *arg0;
{
	char *argv[2];

	argv[0] = arg0;
	argv[1] = (char *) NULL;
	return(runv(file, argv));
}

static int
ex1(file, arg0, arg1)
char *file, *arg0, *arg1;
{
	char *argv[3];

	argv[0] = arg0;
	argv[1] = arg1;
	argv[2] = (char *) NULL;
	return(runv(file, argv));
}

static int
ex2(file, arg0, arg1, arg2)
char *file, *arg0, *arg1, *arg2;
{
	char *argv[4];

	argv[0] = arg0;
	argv[1] = arg1;
	argv[2] = arg2;
	argv[3] = (char *) NULL;
	return(runv(file, argv));
}
#endif /* BSD4 */

Object 
pushFloat(d)
double d;
{
	register double *t;
	register Machine *cmr = CMR;

	heapOverflowCheck(cmr->mr_h, wordsof(double));
	t = (double *) cmr->mr_h;
	cmr->mr_h += wordsof(double);
	*t = d;
	return(StarToFloat(t));
}

Object 
pushString(s)
char *s;
{
	if(s[0] == '\0')
		return(NIL);
	else {
		register Machine *cmr = CMR;
		register int len;
		register char *t;

		len = NWORDS(strlen(s) + 1);
		heapOverflowCheck(cmr->mr_h, len);
		t = (char *) cmr->mr_h;
		cmr->mr_h += len;
		strcpy(t, s);
		return(StarToString(t));
	}
}

Object 
pushAssoc(key, value, assoc)
Atom *key;
Object value, assoc;
{
	register Object *a;
	register Machine *cmr = CMR;

	a = cmr->mr_h;
	cmr->mr_h = a + 5;
	heapOverflowCheck(a, 5);
	a[0] = StarToStr(a + 2);
	a[1] = assoc;
	a[2] = StrHeader2Equal;
	a[3] = StarToAtom(key);
	a[4] = value;
	return(StarToList(a));
}

Object 
pushCons(car, cdr)
Object car, cdr;
{
	register Object *a;

	a = CMR->mr_h;
	CMR->mr_h += 2;
	heapOverflowCheck(a, 2);
	a[0] = car;
	a[1] = cdr;
	return(StarToList(a));
}

Object 
pushList2(e1, e2)
Object e1, e2;
{
	return(pushCons(e1, pushCons(e2, NIL)));
}

static char **
pushArgs(v)
register Object v;
{
	register Object *l, car, *a;
	int len;
	char **args;

	a = (Object *) (CMR->mr_h);
	args = (char **) a;
	len = length(v);
	heapOverflowCheck(a, len + 1);
	CMR->mr_h += len + 1;
	for(;;) {
		DeRef(v);
		if(IsList(v)) {
			l = eRef(v);
			car = l[0]; DeRef(car);
			v = l[1];
			if(IsString(car))
				*a++ = (Object) eRef(pushString((char *) eRef(car)));
			else if(IsAtom(car))
				*a++ = (Object) eCharStar(((Atom *) eRef(car))->a_pname);
			else {
				warning("Atom or string expected in pushArgs()");
				return((char **) NULL);
			}
		} else if(IsNIL(v)) {
			*a = 0;
			break;
		} else {
			warning("List expected in pushArgs()");
			return((char **) NULL);
		}
	}
	return(args);
}

int
sysCall(sn, X1, X2, X3, r)
int sn;
Object X1, X2, X3;
Object *r;
{
	register int n;
	register Word v;
	int args;
	Word Arg[3];

	args = SystemCalls[sn].sys_n;
	if(args > 0)
		Arg[0] = deRef(X1);
	if(args > 1)
		Arg[1] = deRef(X2);
	if(args > 2)
		Arg[2] = deRef(X3);

	for(n = 0; n < args; n++) {
		switch(SystemCalls[sn].sys_args[n]) {
		when vtINT:
			if(IsInt(Arg[n]))
				Arg[n] = eInt(Arg[n]);
			else {
				warning("Integer expected in system call");
				return(0);
			}
		when vtATM:
			if(IsAtom(Arg[n]))
				Arg[n] = (Word) eCharStar(((Atom *) eRef(Arg[n]))->a_pname);
			else if(IsString(Arg[n]))
				Arg[n] = (Word) eRef(Arg[n]);
			else {
				warning("Atom or string expected in system call");
				return(0);
			}
		when vtLST:
			Arg[n] = (Word) pushArgs(Arg[n]);
			if(Arg[n] == 0)
				return(0);
		when vtOBJ:
			if(Arg[n] == 0)
				return(0);

		break;
		default:
			panic("Impossible argument type in SystemCalls table");
		}
	}

	switch(args) {
	when 0:
		v = (*SystemCalls[sn].sys_f)();
	when 1:
		v = (*SystemCalls[sn].sys_f)(Arg[0]);
	when 2:
		v = (*SystemCalls[sn].sys_f)(Arg[0], Arg[1]);
	when 3:
		v = (*SystemCalls[sn].sys_f)(Arg[0], Arg[1], Arg[2]);

	break;
	default:
		panic("Impossible number of arguments in SystemCalls table");
	}

	switch(SystemCalls[sn].sys_value) {
	when vtNON:
		*r = MakeSmallInt(0);
	when vtINT:
		*r = MakeInt(v, CMR->mr_h);
	when vtATM:
		if(v == (Word) NULL)
			*r = NIL;
		else
			*r = StarToAtom(enterAtom((char *) v, stab, (Atom *) NULL));
	when vtOBJ:
		*r = v;

	break;
	default:
		panic("Impossible value type in SystemCalls table");
	}
	return(1);
}

Word
unix_environ()
{
	register char **e;
	register Object l;
	register int len;
	register char *eq;
	char name[1024];

	l = NIL;
	for(e = environment; *e != (char *) NULL; e++) {
		for(eq = *e; *eq != '\0'; eq++)
			if(*eq == '=' || *eq == '(')
				break;
		/* Ignore shell functions (of the form "name(){definition}") */
		if(*eq != '=')
			continue;
		len = eq - *e;
		strncpy(name, *e, len);
		name[len] = '\0';
		l = pushAssoc(
				enterAtom(name, stab, (Atom *) NULL),
				pushString(*e + len + 1),
				l);
	}
	return(l);
}

Word
unix_getgroups()
{
#ifdef BSD4
	int groups[NGROUPS];
	register int n, ngroups;
	register Object l;

	ngroups = getgroups(NGROUPS, groups);
	l = NIL;
	for(n = 0; n < ngroups; n++)
		l = pushCons(MakeSmallInt(groups[n]), l);
	return(l);
#else /* BSD4 */
	return(pushCons(MakeSmallInt(getgid()), NIL));
#endif /* BSD4 */
}

Word
unix_getlogin()
{
	register char *l;
	register struct passwd *pw;

	l = getlogin();
	if(l == (char *) NULL) {
		pw = getpwuid(getuid());
		if(pw == (struct passwd *) NULL) {
			warning("Can't get entry from passwd file");
			return((Word) NULL);
		}
		l = pw->pw_name;
	}
	return((Word) strcpy((char *)CMR->mr_h, l));
}

static Object
burstPWEnt(pw)
register struct passwd *pw;
{
	register Object l;

	if(pw == (struct passwd *) NULL) {
		warning("Can't get entry from passwd file");
		return(NIL);
	}
	l = pushAssoc(&SymShell, pushString(pw->pw_shell), NIL);
	l = pushAssoc(&SymDir, pushString(pw->pw_dir), l);
	l = pushAssoc(&SymGecos, pushString(pw->pw_gecos), l);
	l = pushAssoc(&SymGid, MakeSmallInt(pw->pw_gid), l);
	l = pushAssoc(&SymUid, MakeSmallInt(pw->pw_uid), l);
	l = pushAssoc(&SymPasswd, pushString(pw->pw_passwd), l);
	l = pushAssoc(&SymName, pushString(pw->pw_name), l);
	endpwent();
	return(l);
}

Word
unix_chmod(name, mode)
char *name;
Word mode;
{
	return chmod(name, mode);
}

Word
unix_getpwuid(uid)
Word uid;
{
	return(burstPWEnt(getpwuid(uid)));
}

Word
unix_getpwnam(name)
char *name;
{
	return(burstPWEnt(getpwnam(name)));
}

#ifndef BSD4
char *
getwd(base)
char *base;
{
	register char *s;
	register int i, c;
	register FILE *pp;

	pp = popen("/bin/pwd", "r");
	if(pp == (FILE *) NULL) {
		warning("Can't do popen in unix_getwd()");
		return((Word) NULL);
	}

	s = base;
	for(i = MAXPATHLEN; i > 0; i--) {
		if(feof(pp)) {
			pclose(pp);
			return((Word) NULL);
		}
		c = getc(pp);
		if(c == '\n')
			break;
		*s++ = c;
	}
	*s = '\0';
	pclose(pp);
	return(base);
}
#endif /* BSD4 */

Word
unix_getwd()
{
	register Machine *cmr = CMR;

	/* BUG!  Don't need to put this on the heap at all. */
	getwd((char *) cmr->mr_h);
	strcpy(currentDirectory, (char *) cmr->mr_h);
	return((Word) cmr->mr_h);
}

Word
unix_hostname()
{
#ifdef BSD4
	gethostname((char *)CMR->mr_h, MAXPATHLEN);
	return((Word) CMR->mr_h);
#else /* BSD4 */
	struct utsname u;

	uname(&u);
	return((Word) strcpy((char *)CMR->mr_h, u.nodename));
#endif /* BSD4 */
}

Word
unix_mkdir(dir)
char *dir;
{
#ifdef BSD4
	return(mkdir(dir, 0777));
#else /* BSD4 */
	return(ex1("/bin/mkdir", "mkdir", dir));
#endif /* BSD4 */
}

Word
unix_random()
{
	register Word r;

#ifdef BSD4
	r = random();
#else /* BSD4 */
	r = lrand48();
#endif /* BSD4 */
	if(r < 0)
		r = -(r + 1);
	return(r);
}

Word
unix_rename(f1, f2)
char *f1, *f2;
{
#ifdef BSD4
	return(rename(f1, f2));
#else /* BSD4 */
	return(ex2("/bin/mv", "mv", f1, f2));
#endif /* BSD4 */
}

Word
unix_rmdir(dir)
char *dir;
{
#ifdef BSD4
	return(rmdir(dir));
#else /* BSD4 */
	return(ex1("/bin/rmdir", "rmdir", dir));
#endif /* BSD4 */
}

Word
unix_sleep(s)
Word s;
{
	sleep((unsigned int) s);
	return(0);
}

Word
unix_stat(filename)
char *filename;
{
	struct stat sb;
	register Object l;

	if(stat(filename, &sb) == -1)
		return(NIL);

	l = pushAssoc(&SymDev, MakeSmallInt(sb.st_dev), NIL);
	l = pushAssoc(&SymIno, MakeSmallInt(sb.st_ino), l);
	l = pushAssoc(&SymMode, MakeSmallInt(sb.st_mode), l);
	l = pushAssoc(&SymNLink, MakeSmallInt(sb.st_nlink), l);
	l = pushAssoc(&SymUid, MakeSmallInt(sb.st_uid), l);
	l = pushAssoc(&SymGid, MakeSmallInt(sb.st_gid), l);
	l = pushAssoc(&SymSize, MakeInt(sb.st_size, CMR->mr_h), l);
	l = pushAssoc(&SymAtime, MakeInt(sb.st_atime, CMR->mr_h), l);
	l = pushAssoc(&SymMtime, MakeInt(sb.st_mtime, CMR->mr_h), l);
	l = pushAssoc(&SymCtime, MakeInt(sb.st_ctime, CMR->mr_h), l);
#if defined(BSD4) && !defined(MACHINE_SGI)
	l = pushAssoc(&SymBlocks, MakeSmallInt(sb.st_blocks), l);
#endif /* defined(BSD4) && !defined(MACHINE_SGI) */
	return(l);
}

static Atom *days[] =
	{&SymSun, &SymMon, &SymTue, &SymWed, &SymThu, &SymFri, &SymSat};

Word
unix_time(clock, flag)
long clock;			/* BUG!  sizeof(long) had better equal sizeof(int) */
int flag;
{
	register struct tm *t;
	register Object l;

	if(!flag)
		time(&clock);
	t = localtime(&clock);
	l = pushAssoc(&SymYear, MakeSmallInt(1900 + t->tm_year), NIL);
	l = pushAssoc(&SymMonth, MakeSmallInt(1 + t->tm_mon), l);
	l = pushAssoc(&SymDate, MakeSmallInt(t->tm_mday), l);
	l = pushAssoc(&SymDay, StarToAtom(days[t->tm_wday]), l);
	l = pushAssoc(&SymYearDate, MakeSmallInt(t->tm_yday), l);
	l = pushAssoc(&SymSecond, MakeSmallInt(t->tm_sec), l);
	l = pushAssoc(&SymMinute, MakeSmallInt(t->tm_min), l);
	l = pushAssoc(&SymHour, MakeSmallInt(t->tm_hour), l);
	return(l);
}

Word
unix_truncate(path, length)
char *path;
register int length;
{
#ifdef BSD4
	return(truncate(path, length));
#else /* BSD4 */
	register FILE *fp, *nfp;
	register int c;
	char temp[32];

	fp = fopen(path, "r");
	if(fp == (FILE *) NULL) {
		warning("Unable to open file in unix_truncate()");
		return(0);
	}
	strcpy(temp, "/tmp/nepXXXXXX");
	mktemp(temp);
	nfp = fopen(temp, "w+");
	if(nfp == (FILE *) NULL) {
		warning("Unable to open tempfile in unix_truncate()");
		fclose(fp);
		return(0);
	}
	for( ; length > 0; length--) {
		c = getc(fp);
		if(c == EOF)
			break;
		putc(c, nfp);
	}
	freopen(path, "w", fp);
	if(fp == (FILE *) NULL) {
		warning("Unable to open file for writing in unix_truncate()");
		fclose(nfp);
		return(0);
	}
	rewind(nfp);
	for(;;) {
		c = getc(nfp);
		if(c == EOF)
			break;
		putc(c, fp);
	}
	fclose(fp); fclose(nfp);
	unlink(temp);
	return(length >= 0 ? 0 : -1);
#endif /* BSD4 */
}

Word
unix_umask(u)
register int u;
{
	return(umask(u));
}

Word
unix_wait()
{
	register int pid;
	int status;

	pid = wait(&status);

	return(pushCons(MakeSmallInt(pid), MakeSmallInt(status)));
}

Word
unix_readdir(path)
char *path;
{
	register DIR *dp;
#ifdef MACHINE_SUN4_SOLARIS
	register struct dirent *d;
#else
	register struct direct *d;
#endif
	register Object l;

	dp = opendir(path);
	if(dp == (DIR *) NULL) {
		warning("Unable to open directory for reading in unix_readdir()");
		return(NIL);
	}
	l = NIL;
	while((d = readdir(dp)) != NULL)
		l = pushCons(pushString(d->d_name), l);
	closedir(dp);
	return(l);
}

Word
unix_exec(name, argv)
char *name, **argv;
{
	return(execvp(name, argv));
}

/* cputime()
 * Get user and system time in milliseconds
 * (to the extent that the OS is capable of it)
 */
#ifdef BSD4
void
cputime(u, s)
Word *u, *s;
{
	struct rusage rb;

	getrusage(RUSAGE_SELF, &rb);
	*u = rb.ru_utime.tv_sec * 1000 + (rb.ru_utime.tv_usec + 500) / 1000;
	*s = rb.ru_stime.tv_sec * 1000 + (rb.ru_stime.tv_usec + 500) / 1000;
}

#else /* BSD4 */
void
cputime(u, s)
Word *u, *s;
{
	struct tms tb;

	times(&tb);
	*u = tb.tms_utime * (1000 / (double) HZ);
	*s = tb.tms_stime * (1000 / (double) HZ);
}
#endif /* BSD4 */

Word
unix_statistics()
{
	static Word lastUtime = 0;
	static Word lastStime = 0;
	register Machine *cmr = CMR;
	register Object l;
	Word ums, lums;
	Word sms, lsms;

	l = NIL;
	l = pushAssoc(&SymMemory,
			pushList2(MakeInt((Word)highWater - (Word)lowWater, cmr->mr_h),
				MakeSmallInt(0)), l);
	l = pushAssoc(
			&SymProgram,
			pushList2(MakeInt(programSize * sizeof(Word), cmr->mr_h),
				MakeSmallInt(0)),
			l);
	l = pushAssoc(
			&SymTrail,
			pushList2(
				MakeInt((cmr->mr_tr - trailBase) * sizeof(TrailRecord),
					cmr->mr_h),
				MakeInt((trailTop - cmr->mr_tr) * sizeof(TrailRecord),
					cmr->mr_h)),
			l);
	l = pushAssoc(
			&SymLocal,
			pushList2(
				MakeInt((cmr->mr_a - stackBase) * sizeof(Word), cmr->mr_h),
				MakeInt((stackTop - cmr->mr_a) * sizeof(Word), cmr->mr_h)),
			l);
	l = pushAssoc(
			&SymGlobal,
			pushList2(
				MakeInt((cmr->mr_h - heapBase) * sizeof(Word), cmr->mr_h),
				MakeInt((heapTop - cmr->mr_h) * sizeof(Word), cmr->mr_h)),
			l);
	cputime(&ums, &sms);
	lums = ums - lastUtime;
	lastUtime = ums;
	lsms = sms - lastStime;
	lastStime = sms;
	l = pushAssoc(&SymUtime,
			pushList2(MakeInt(ums, cmr->mr_h), MakeInt(lums, cmr->mr_h)), l);
	l = pushAssoc(&SymStime,
			pushList2(MakeInt(sms, cmr->mr_h), MakeInt(lsms, cmr->mr_h)), l);
	l = pushAssoc(&SymTime,
			pushList2(MakeInt(ums+sms, cmr->mr_h),
				MakeInt(lums+lsms, cmr->mr_h)), l);
	return(l);
}

Word
unix_fseek(sd, offset, from)
Word sd, offset, from;
{
	return(fseek(streams[sd].s_fp, offset, from));
}

Word
unix_ftell(sd)
Word sd;
{
	return(ftell(streams[sd].s_fp));
}

Word
unix_fesq()
{
	register long h;
#ifdef sun
#	ifdef MACHINE_SUN4_SOLARIS
		return NIL;
#	else
	extern long gethostid();

	h = gethostid();
	return(pushCons(MakeSmallInt(h & 0xffff),
			MakeSmallInt((unsigned long) h >> 16)));
#	endif
#else
	return NIL;
#endif
}

Word
unix_setSignal(Sig, Action)
register Word Sig;
register Object Action;
{
	register Atom *a;

	if(Sig <= 0 || Sig >= NSIG) {
		warning("signal number out of range in signal/2");
		return(-1);
	}

	DeRef(Action);
	if(!IsAtom(Action)) {
		warning("atom expected in signal/2");
		return(-1);
	}

	a = (Atom *) eRef(Action);
	SignalActions[Sig] = enterFunctor(3, a);
	SignalsPending[Sig] = 0;
	if(a == &SymDefault)
		(void) signal(Sig, SIG_DFL);
	else if(a == &SymIgnore)
		(void) signal(Sig, SIG_IGN);
	else
		(void) signal(Sig, receiveSignal);
	return(0);
}
