#! /bin/sh
#
#  revise - incremental compilation system for NU-Prolog
#
#  This shell script orchestrates the process of editing some NU-Prolog
#  source files, determining what changes were made, and re-compiling
#  only those (pieces of) source files which need to be re-created
#
#  Should be cleaned up more - modified from previous version by Lee
# most work is now done by redit and rnc

###mybin="BIN"
mybin="/home/mungunni/jws/bin"

redit=$mybin/redit
rnc=$mybin/rnc

case $#
in
0) echo "Usage: revise [ -p pred | -t tag | -f file] source_files"
   exit ;;
esac

flag=$1
case $1
in
-p) tag="$2" ; file="$3" ; shift ; shift ; shift ;;
-t) tag="$2" ; file="$3" ; shift ; shift ; shift ;;
-f) tag="" ; file="$2" ; shift ; shift ;;
*)  tag="" ; file="$1" ;;
esac

files=`echo $file $* | tr ' ' '\012' | sort -u`

#	$bin/nlcp -m'%--' $srcs

ncargs=""
edargs=""

for f in $files
do
	if f1=`expr $f : '\(.*\).nl'`
	then
		ncargs="$ncargs $f1.nl@"
		edargs="$edargs $f"
	else
		ncargs="$ncargs $f.nl@"
		edargs="$edargs $f.nl"
	fi
done

case "$flag"
in
-p) $redit "+/^$tag\\>" "$edargs" ;;
-t)  $redit -t "$tag" ;;
*) $redit "$edargs" ;;
esac

$rnc -c $ncargs
