/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1990 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

	.inline _ExtField_1_2,4
	movl	sp@+,d1
	bfextu	d1{#1:#2},d0
	.end

	.inline _ExtField_1_3,4
	movl	sp@+,d1
	bfextu	d1{#1:#3},d0
	.end

	.inline _ExtField_3_1,4
	movl	sp@+,d1
	bfextu	d1{#3:#1},d0
	.end

	.inline _ExtField_3_3,4
	movl	sp@+,d1
	bfextu	d1{#3:#3},d0
	.end

	.inline _ExtField_4_2,4
	movl	sp@+,d1
	bfextu	d1{#4:#2},d0
	.end

	.inline	_deRef,4
	movl	sp@+,d0
	jlt	2f
	movl	d0,d1
1:
	andl	#67108863,d0
	movl	d0,a0
	movl	a0@,d0
	cmpl	d0,d1
	jeq	2f
	movl	d0,d1
	jge	1b
2:
	.end

	.inline _wcopy,12
	movl	sp@+,a1
	movl	sp@+,a0
	movl	sp@+,d0
	jle	2f
1:
	movl	a0@+,a1@+
	subql	#1,d0
	jgt	1b
2:
	.end

	.inline _trailEntry1,8
	movl	sp@+,a0
	movl	sp@+,d0
	movl	d0,a0@+
	movl	d0,a0@+
	movl	a0,d0
	.end

	.inline _trailEntry2,12
	movl	sp@+,a0
	movl	sp@+,d0
	movl	d0,a0@+
	movl	sp@+,d1
	movl	d1,a0@+
	movl	a0,d0
	.end

	.inline _failure,8
	movl    sp@+,a0
	movl    sp@+,d0
	cmpl	a0,d0
	jge	2f
1:
	subql	#8,a0
	movl	a0@,a1
	movl	a0@(4),a1@
	cmpl	a0,d0
	jlt	1b
2:
	movl	a0,d0
	.end
