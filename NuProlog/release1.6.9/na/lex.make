#! /bin/sh
#
# Please note that this code is the property of the University
# of Melbourne and is Copyright 1985, 1986, 1987 by it.
# 
# All rights are reserved.
#
# Author: John Shepherd
#
# 
#
# Reconstruct "lex.c" if "bytecodes.c" changes
#

tmp=.lex.c
trap "/bin/rm -f $tmp" 0 1 2 3 15 

sed -n -e '1,/Start Opcodes/p' lex.c >> $tmp

cat << XXX >> $tmp

static opentry opcodes[] =
{
XXX

awk -f lex.c.awk defines="$1" ../nep/bytecodes.c | sort >> $tmp

cat << XXX >> $tmp
};

static int nopcodes = sizeof(opcodes) / sizeof(opentry);

XXX

sed -n -e '/End Opcodes/,$p' lex.c >> $tmp

mv $tmp lex.c
