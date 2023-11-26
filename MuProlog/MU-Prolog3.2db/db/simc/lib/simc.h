/*
 * simc.h  -  deductive database package (general library definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 */

#ifndef	SIMC_H
#define	SIMC_H

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sgtty.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/file.h>
#include <sys/stat.h>

#include "params.h"
#include "defs.h"
#include "bits.h"
#include "addr.h"
#include "cache.h"
#include "dfile.h"
#include "split.h"
#include "rdesc.h"
#include "sdesc.h"
#include "rec.h"
#include "seg.h"
#include "parse.h"
#include "reln.h"
#include "trans.h"
#include "util.h"

/*
 * Hacks for BSD / SysV incompatibilities
 */

#ifdef L_SET
/* must be BSD */
#include <strings.h>
#else
#include <string.h>
#define	index	strchr
#define	rindex	strrchr
#endif

#ifndef L_SET
#define L_SET 0
#define L_INCR 1
#define L_XTND 2
#endif

#ifndef O_RDONLY
#include <fcntl.h>
#endif

/*
 * Lint avoiders
 */
extern char *malloc();

/*
 * Get rid of flock'ing until elxsi bsd is fixed
 */
#ifdef L_SET
#ifdef elxsi
#define	flock(x,y) 0
#endif
#endif

#endif	SIMC_H
