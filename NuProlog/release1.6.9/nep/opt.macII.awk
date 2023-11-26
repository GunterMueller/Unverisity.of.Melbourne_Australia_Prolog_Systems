BEGIN {
	collapsing = 0;
}

/^L%[0-9]*:/ {
	if(!collapsing) {
		l = $1;
		collapsing = 1;
	}
	labels[l] = $1;
	next;
}

// {
	collapsing = 0;
	next;
}

END {
	
}
