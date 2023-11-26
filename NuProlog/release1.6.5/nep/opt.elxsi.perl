#!/mip/usr/bin/perl

$file = $ARGV[0];

open(FILE, $file) || die "Can't open file $file";
while(<FILE>) {
	next unless(/^(\.\w*):/);
	for($i = 0; /^(\.\w*):/; $i = $i+1) {
		$labels[$i] = $1;
		$_ = <FILE>;
	}
	next unless($_ =~ /^	jmp..(\.\w*)/);
	$target = $1;
	for($i = $i - 1; $i >= 0; $i = $i - 1) {
		$jumps{$labels[$i]} = $target;
	}
}

#	The following is probably only needed on the Sun4 and elxsi
@labels = keys(jumps);
while($#labels >= 0) {
	$k = pop(@labels);
	for($t = $k; $jumps{$t}; $t = $jumps{$t})
		{ }
	$jumps{$k} = $t;
}

@labels = keys(jumps);
while($#labels >= 0) {
	$l = pop(@labels);
	$targets{$jumps{$l}} .= "$l:\n";
}

#@keys = keys(jumps);
#@values = values(jumps);
#while($#keys >= 0) {
#	print pop(@keys),' = ',pop(@values),"\n";
#}
#
#@keys = keys(targets);
#@values = values(targets);
#while($#keys >= 0) {
#	print pop(@keys),' = ',pop(@values),"\n";
#}

open(FILE, $file) || die "Can't open file $file";
while(<FILE>) {
	$acc = '';
	if(/^(\.\w*):/) {
		next if($jumps{$1});
		print $targets{$1} if($targets{$1});
		print;
		next;
	}
	if(/extract	\.r([0-9]*),\.r([0-9]*):bit 32,32$/) {
		next if($1 == $2);
	}
	if(/extract.*:bit 32,32$/){
		s/extractz/ldz.32/;
		s/extract/ld.32/;
		s/:bit 32,32//;
		print;
		next;
	}
	$acc = $_;
	next unless (/^	cmpu\.br\.64	\.r([0-9]*),=255:jgt	/);
	$r = $1;
	$_ = <FILE>;
	$keep = $_;
	$acc .= $_;
	$s = "^	sll2.64	.r$r,.r$r";
	next unless ($_ =~ $s);
	$_ = <FILE>;
	$keep .= $_;
	$acc .= $_;
	$s = "^	ld.32	.r$r,..r$r.";
	next unless ($_ =~ $s);
	$_ = <FILE>;
	$keep .= $_;
	$acc .= $_;
	$s = "^	br.reg	.r$r";
	next unless ($_ =~ $s);
	$_ = <FILE>;
	$keep .= $_;
	$acc .= $_;
	next unless (/^	\.const$/);
	$acc = $keep."	.align	4\n";
} continue {
	print $acc;
}

#@keys = keys(jumps);
#@values = values(jumps);
#while($#keys >= 0) {
#	print pop(@keys),' = ',pop(@values),"\n";
#}
#
#@keys = keys(targets);
#@values = values(targets);
#while($#keys >= 0) {
#	print pop(@keys),' = ',pop(@values),"\n";
#}
