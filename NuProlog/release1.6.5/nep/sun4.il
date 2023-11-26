/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1990 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */
	.inline	_trailEntry2,12
	st	%o1,[%o0]
	st	%o2,[%o0+4]
	inc	8,%o0
	.end

	.inline	_trailEntry1,8
	st	%o1,[%o0]
	st	%o1,[%o0+4]
	inc	8,%o0
	.end

	.inline	_deRef,4
	tst	%o0
	bl	2f
	nop
	set	0x3ffffff,%o2
1:
	and	%o0,%o2,%o1
	ld	[%o1],%o3
	cmp	%o0,%o3
	be	2f
	nop
	addcc	%o3,%g0,%o0
	bge	1b
	nop
2:
	.end

%	.inline	_deRef,4
%	srl	%o0,3,%o1
%	andcc	%o1,3,%g0
%	bne	2f
%	nop
%1:					! [internal]
%	srl	%o0,5,%o5
%	ld	[%o5],%o3
%	cmp	%o0,%o3
%	be	2f
%	nop
%	mov	%o3,%o0
%	srl	%o0,3,%o1
%	andcc	%o1,3,%g0
%	be	1b
%	nop
%2:
%	.end

	.inline	_wcopy,12
	deccc	%o2
	bneg	2f
	nop
	sll	%o2,2,%o2
1:
	ld	[%o1+%o2],%o3
	st	%o3,[%o0+%o2]
	deccc	4,%o2
	bpos	1b
	nop
2:
	.end

	.inline	_failure,8
	cmp	%o0,%o1
	bleu	2f
	nop
1:
	dec	8,%o0
	ld	[%o0],%o2
	ld	[%o0+4],%o3
	st	%o3,[%o2]
	cmp	%o0,%o1
	bgu	1b
	nop
2:
	.end

/*
	.inline	_getreg_g3,0
	mov	%g3,%o0
	.end

	.inline	_setreg_g3,4
	mov	%o0,%g3
	.end
*/
