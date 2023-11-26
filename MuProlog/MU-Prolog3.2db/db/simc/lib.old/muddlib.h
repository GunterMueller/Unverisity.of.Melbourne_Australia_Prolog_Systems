/*
 * muddlib.h  -  deductive database package (general library definitions)
 *
 * $Header: muddlib.h,v 1.5 85/12/06 15:09:47 jas Exp $
 * $Log:	muddlib.h,v $
 * Revision 1.5  85/12/06  15:09:47  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:26  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:30:26  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:51:32  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:23  jas
 * Initial revision
 * 
 * 
 */

#ifndef	MUDDLIB_H
#define	MUDDLIB_H

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
#ifdef L_SET
/* must be BSD */
#include <strings.h>
#else
#include <string.h>
#define	index	strchr
#define	rindex	strrchr
#endif
#include "defs.h"
#include "bits.h"
#include "files.h"
#include "rec.h"
#include "seg.h"
#include "parse.h"
#include "reln.h"
#include "fact.h"
#include "trans.h"
#include "util.h"

/*
 * Get rid of flock'ing until elxsi bsd is fixed
 */
#ifdef L_SET
#ifdef elxsi
#define	flock(x,y)	0
#endif
#endif

/*
 * Hacks for BSD / SysV incompatibilities
 */

#ifndef L_SET
#define L_SET 0
#define L_INCR 1
#define L_XTND 2
#endif

#ifndef O_RDONLY
#include <fcntl.h>
#endif

#endif	MUDDLIB_H
