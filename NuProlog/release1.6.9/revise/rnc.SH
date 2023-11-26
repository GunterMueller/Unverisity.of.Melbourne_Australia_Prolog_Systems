#! /bin/sh
#
# Script for (re)compiling a file(s) in new revise system
#
# If backup copy of .nl file does not exist, just compile
# If it does exists, use it to update .ns file etc and remove backup
# If file.nl@ is specified, also produce file@.no and do the
# compilation of non-@.n? files in background

rm=/bin/rm
cp=/bin/cp

###mybin="BIN"
mybin="/home/mungunni/jws/bin"
nc=$mybin/nc
nldiffs=$mybin/nldiffs
nsfix=$mybin/nsfix

# flag set if there is a file.nl@ arg which exists
somenla=0
ncerrs=0
ncargs=""

for f in $*
do
#		Don't try to be clever when there are funny
#		compilation flags.  Should remove backups here
#		also, at least in some cases (FIX)
	if [ x$f = "x-F" -o x$f = "x-D" -o x$f = "x-U" ]
	then
		echo Warning: -F -D -U options not fully supported -
		echo backup files may get out of date
		$nc $*
		exit
	fi
done
for f in $*
do
#	check for .nl, .nl@ and set flags
#	isnl set if arg is .nl or .nl@
#	isnla set if arg is .nl@
#	somenla set (later) if any of the args are .nl@ & there is a .ns
#
	isnl=1
	isnla=1
	f1=`expr $f : '\(.*\).nl@'`
	if [ -z "$f1" ]
	then
		isnla=0
		f1=`expr $f : '\(.*\).nl'`
		if [ -z "$f1" ]
		then
			isnl=0
		fi
	fi
	# now f1 is path/filename without extension
	if [ $isnl = 0 ]
	then
		# not source file -> no funny business
		ncargs="$ncargs $f"
	else
		src="$f1.nl"
		backup="$f1.nl@"
		modsrc="$f1@.nl"
		asm="$f1.ns"
		modasm="$f1@.ns"
		# should check file modify times here also
		if [ -f $backup -a -f $f1.ns ]
		then
				# get diffs for .nl and .nl@
			$nldiffs $src $backup > $modsrc
			if [ $isnla = 1 ]
			then
				somenla=1
				ncflag='-c'
			else
				ncflag='-S'
			fi
			if $nc $ncflag $modsrc
			then
#					compilation ok
				$rm -f $backup
					# make .ns up to date
				$nsfix $modasm $asm
				echo $asm updated
				ncargs="$ncargs $asm"
#
#				should we create backup here?
#				probably - we know they are being used
#				this way as long as a backup is created
#				initially, everything is maintained by
#				rnc (redit is not necessary, though it
#				does clean up some files and could check
#				file modify times etc)
#
				$cp $src $backup
				echo Backup file $backup updated
			else
#					compilation failed
				echo Backup file $backup not modified
				ncerrs=1
			fi
		else
#			No backup/old .ns -> normal compile.
#			We could just use the following line:
#				ncargs="$ncargs $src"
#			But creation of backups is more complex.
#			Instead, we do the compilation here and
#			only the assembling is delayed to the end.
#			$nc -S could be optimised to $nep .....
#			(could be done above also)
#
			if $nc -S $src
			then
#					compilation ok
				ncargs="$ncargs $asm"
#
#				should we create backup here?
#				advantage is that redit is not needed
#				disadvantage is that backups are always
#				created, even when they are not wanted
#				If rnc stays a separate command, it probably
#				should be created, if it gets combined with
#				nc, probably not (-> use rm, not cp below)
#
				# $rm -f $backup
				if [ -f $backup ]
				then
					$cp $src $backup
					echo Backup file $backup updated
				else
					$cp $src $backup
					echo Backup file $backup created
				fi
			else
#					compilation failed
				ncerrs=1
			fi
		fi
	fi
done

if [ $ncerrs = 0 ]
then
	if [ $somenla = 1 ]
	then
		# if any f.nl@ files were specified, do final nc in background
		# (f.nl@ comes from revise/dEdit normally, and only f@.no is
		# wanted immediately - f.no can wait)
#		echo nc $ncargs "&"
		$nc $ncargs &
	else
#		echo nc $ncargs
		$nc $ncargs
	fi
fi
