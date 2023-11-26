#! /bin/sh
#
# nc:	NU-Prolog compiler
#	(behaves vaguely like cc(1) to make things simple)

#
# location of important binaries
#
###mybin="BIN"
mybin="/bip/usr/bin"
###nubin="NUBIN"
nubin="/bip/usr/lib/nuprolog/bin"
nuc="$nubin/nuc"
na="$mybin/na"
nep="$nubin/nep"
nsys="$nubin/nuprolog"
###nulib="NULIB"
nulib="/mip/usr/lib/nuprolog/lib"
testprog=test		# some systems don't have /bin/test anymore!

objfile="a.out"		# default "object" file
objgiven="NO"		# Was -o specified?

nlfiles=""
nsfiles=""
nofiles=""
nplibs=""
nolibs=""

cflag="OFF"		# flag to stop after making .no files
sflag="OFF"		# flag to stop after making .ns files
dflag="OFF"		# flag to run the .nl files through the DCG xlator
npopts=""
basename=`basename $0`
ffopts="-F $basename"

errs=0

#
# if no args, don't bother trying anything else
#
if $testprog $# -eq 0
then
	echo "Usage: nc [-o obj_file] [-e entry_pred] [-F option ] [-S] [-c] *.nl *.ns *.no {-l library}"
	exit $errs
fi

#
# scan args, looking for options, setting arg lists
#
while $testprog $# -gt 0
do
	case "$1" in
	*.nl)	nlfiles="$nlfiles $1" ;;
	*.ns)	nsfiles="$nsfiles $1" ;;
	*.no)	nofiles="$nofiles $1" ;;
	-c)	cflag="ON" ;;
	-S)	sflag="ON" ;;
	-l)	shift; nplibs="$nplibs $1" ;;
	-o)	shift; objfile="$1"
		objgiven="YES"
		case "$objfile" in
		*.nl|-*) 
			echo "nc: Dangerous object file name: $objfile"
			errs=1 ;;
		esac ;;
	-e)	shift; npopts="$npopts -e '$1'" ;;
	-D)	dflag="ON" ;;
	-F)	shift; ffopts="$ffopts -F $1" ;;
	-u)	shift; npopts="$npopts -u '$1'" ;;
	-v)	shift; npopts="$npopts -v '$1'" ;;
	-w)	shift; npopts="$npopts -w '$1'" ;;
	-x)	shift; npopts="$npopts -x '$1'" ;;
	-*)	echo "nc: Unrecognized option: $1"; errs=1 ;;
		# maybe should be: npopts="$npotps $1" ?
	*)	echo "nc: Unrecognised argument: $1"; errs=1 ;;
	esac
	if $testprog $# -gt 0; then shift; fi
done

if $testprog $sflag = ON -a $cflag = ON
then
	echo "nc: cannot specify both -c and -S"
	errs=1
fi

if expr "$errs" = 0 \& "$objgiven" = YES \& \
	\( \( "$sflag" = ON \) != \( "$objfile" : .\*\\.ns \> 0 \) \| \
	   \( "$cflag" = ON \) != \( "$objfile" : .\*\\.no \> 0 \) \) >/dev/null
then
	echo "nc: Dangerous object file name: $objfile"
	errs=1
fi

#
# if args wrong, don't bother trying anything else
#
if $testprog $errs = 1
then
	echo "Usage: nc [-o obj_file] [-e entry_pred] [-S] [-c] *.nl *.ns *.no {-l library}"
	exit $errs
fi

#
# run .nl files through the DCG translator
#
if $testprog $dflag = ON
then
	for arg in $nlfiles
	do
		$nep -R $nuc -a -D <$arg
	done
	exit $errs
fi
if $testprog $sflag = ON -a $objgiven = YES
then	if $testprog $?nlfiles -gt 1
	then
		echo "nc: You can't use 'nc -S -o ?.ns' with more than one source file."
		exit 1
	else
		ffopts="$ffopts -o '$objfile'"
		asobj="$objfile"
	fi
else
	asobj=""
fi

#
# first pass, compile (.nl -> .ns)
#
for arg in $nlfiles
do
	name=`basename $arg`
###	if expr BSD4 != 1 \& $name : '................*' > /dev/null
	if expr 1 != 1 \& $name : '................*' > /dev/null
	then
		echo "nc: filename \"$arg\" too long ... ignoring it"
		continue
	fi
	a=`expr $arg : '\(.*\).nl`
	if $testprog -r $arg
	then
		#
		# Add a test for being able to create .ns file
		#
		if $nep -T -R $nuc -a $ffopts $arg
		then
			nsfiles="$nsfiles $a.ns"
		else
			echo "nc: compilation errors in \"$arg\""
			errs=1
		fi
	else
		echo "nc: can't find \"$arg\""
		errs=1
	fi
done
#echo "nsfiles=$nsfiles, asobj=$asobj, a=$a"
if $testprog $sflag = ON; then exit $errs;  fi
if $testprog $cflag = ON -a $objgiven = YES
then	if $testprog $?nsfiles -gt 1
	then
		echo "nc: You can't use 'nc -c -o ?.no' with more than one source file."
		exit 1
	else
		objoutput="-o $objfile"
	fi
else
	objoutput=""
fi

#
# second pass, assemble (.ns -> .no)
#
for arg in $nsfiles
do
	name=`basename $arg`
###	if expr BSD4 != 1 \& $name : '................*' > /dev/null
	if expr 1 != 1 \& $name : '................*' > /dev/null
	then
		echo "nc: filename \"$arg\" too long ... ignoring it"
		continue
	fi
	a=`expr $arg : '\(.*\).ns`
	if $testprog -r $arg
	then
		#
		# Add a test for being able to create .no file
		#
		if $na $arg
		then
			nofiles="$nofiles $a.no"
			if $testprog "$objoutput"; then mv $a.no $objfile; fi
		else
			echo "nc: assembly errors in \"$arg\""
			errs=1
		fi
	else
		echo "nc: can't find \"$arg\""
		errs=1
	fi
done
if $testprog $cflag = ON; then exit $errs; fi

#
# add libraries to list of .no files
#
for arg in $nplibs
do
	lib="$nulib/$arg.no"
	if $testprog -r $lib
	then
		nolibs="$nolibs $lib"
	else
		echo "nc: no such library: $arg"
		errs=1
	fi
done

#
# make sure all specified .no files actually exist
#
for arg in $nofiles
do
	if $testprog ! -r $arg
	then
		echo "nc: no such object file: $arg"
		errs=1
	fi
done

#
# no point going further if compilation errors
#
if $testprog $errs = 1
then
	echo "nc: no object file produced"
	exit $errs
fi

#
# third pass, make "object" file and save file
# we must also ensure that all path names used in the
#  "object" file are absolute => object works in any dir
#

# Under BSD, the rest of the script becomes:
#
# $nep -R $nsys -S $objfile $nofiles $nolibs
# chmod 755 $objfile

# make an absolute version of the save file name

srcdir=`pwd`
if $testprog `expr "$objfile" : '/.*'` -eq 0
then
	aobjfile="$srcdir/$objfile"
else
	aobjfile="$objfile"
fi
savefile="${aobjfile}.save"

# generate object and save files

$nep -R $nsys -u 64 -v 32 -w 16 -x 4 -S $savefile $nofiles $nolibs
echo "exec $nep -R $savefile -P \"\$0\" $npopts -a \"\$@\"" > $objfile
#/bin/rm -f $nofiles
chmod 755 $objfile
