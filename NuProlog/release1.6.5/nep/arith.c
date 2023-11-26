/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"
#include <math.h>
	
static Instruction WPlus[] = {
	MakeIns3(cPLUS, 0, 1, 2),
	MakeIns0(cPRO)
};

#define PI 3.14159265358979323846

static char NonNumberError[] = "Non-number found in eval()";

int ArithError;		/* Type of arithmetic error encountered */
Object DelayArith;	/* Variable delaying the current arithmetic expression */
Arith *ArithStack;                  /* Arithmetic stack top (on local stack) */
Arith *ArithStackMax;               /* Arithmetic stack max */

Instruction WEvaluate[] = {
	MakeIns0(cWEVAL)
};

#define ARITHCHECKNEW(a, check, error) \
	if(		(a).ar_type == arINT && ((a).ar_int check) \
		||	(a).ar_type == arFLT && ((a).ar_float check)) { \
		ArithError = error; \
		return; \
	}

void f_add()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_int += AS[0].ar_int;
		else {
			AS[-1].ar_type = arFLT;
			AS[-1].ar_float = AS[-1].ar_int + AS[0].ar_float;
		}
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float += AS[0].ar_int;
		else
			AS[-1].ar_float += AS[0].ar_float;
	}
}

void f_sub()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_int -= AS[0].ar_int;
		else {
			AS[-1].ar_type = arFLT;
			AS[-1].ar_float = AS[-1].ar_int - AS[0].ar_float;
		}
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float -= AS[0].ar_int;
		else
			AS[-1].ar_float -= AS[0].ar_float;
	}
}

void f_mult()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_int *= AS[0].ar_int;
		else {
			AS[-1].ar_type = arFLT;
			AS[-1].ar_float = AS[-1].ar_int * AS[0].ar_float;
		}
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float *= AS[0].ar_int;
		else
			AS[-1].ar_float *= AS[0].ar_float;
	}
}

void f_div()
{
	register Arith *AS = --ArithStack;

	ARITHCHECKNEW(AS[0], == 0, areDIVZERO);
	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_type = arFLT;
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float = AS[-1].ar_int / (double) AS[0].ar_int;
		else
			AS[-1].ar_float = AS[-1].ar_int / AS[0].ar_float;
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float /= AS[0].ar_int;
		else
			AS[-1].ar_float /= AS[0].ar_float;
	}
}

void f_intdiv()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT && AS[0].ar_type == arINT) {
		if(AS[0].ar_int == 0) {
			ArithError = areDIVZERO;
			return;
		}
		AS[-1].ar_int /= AS[0].ar_int;
	} else
		ArithError = areINT;
}

void f_mod()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT && AS[0].ar_type == arINT) {
		register Word mod, rem;

		mod = AS[0].ar_int;
		if(mod == 0) {
			ArithError = areDIVZERO;
			return;
		}
		rem = AS[-1].ar_int % mod;
		if(rem < 0 && mod > 0)
			rem += mod;
		AS[-1].ar_int = rem;
	} else
		ArithError = areINT;
}

void f_bitand()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT && AS[0].ar_type == arINT) {
		AS[-1].ar_int &= AS[0].ar_int;
	} else
		ArithError = areINT;
}

void f_bitor()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT && AS[0].ar_type == arINT) {
		AS[-1].ar_int |= AS[0].ar_int;
	} else
		ArithError = areINT;
}

void f_bitxor()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT && AS[0].ar_type == arINT) {
		AS[-1].ar_int ^= AS[0].ar_int;
	} else
		ArithError = areINT;
}

void f_lsh()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT && AS[0].ar_type == arINT) {
		AS[-1].ar_int <<= AS[0].ar_int;
	} else
		ArithError = areINT;
}

void f_rsh()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT && AS[0].ar_type == arINT) {
		AS[-1].ar_int >>= AS[0].ar_int;
	} else
		ArithError = areINT;
}

void f_logand()
{
	register Arith *AS = --ArithStack;

	if(	(	AS[-1].ar_type == arINT && AS[-1].ar_int != 0
			||	AS[-1].ar_type == arFLT && AS[-1].ar_float != 0.0)
		&&
		(	AS[0].ar_type == arINT && AS[0].ar_int != 0
			||	AS[0].ar_type == arFLT && AS[0].ar_float != 0.0)
		)
		AS[-1].ar_int = 1;
	else
		AS[-1].ar_int = 0;
	AS[-1].ar_type = arINT;
}

void f_logor()
{
	register Arith *AS = --ArithStack;

	if(		AS[-1].ar_type == arINT && AS[-1].ar_int != 0
		||	AS[-1].ar_type == arFLT && AS[-1].ar_float != 0.0)
		AS[-1].ar_int = 1;
	else if(		AS[0].ar_type == arINT && AS[0].ar_int != 0
		||	AS[0].ar_type == arFLT && AS[0].ar_float != 0.0)
		AS[-1].ar_int = 1;
	else
		AS[-1].ar_int = 0;
	AS[-1].ar_type = arINT;
}

void f_lognot()
{
	register Arith *AS = ArithStack;

	if(		AS[-1].ar_type == arINT && AS[-1].ar_int != 0
		||	AS[-1].ar_type == arFLT && AS[-1].ar_float != 0.0)
		AS[-1].ar_int = 1;
	else
		AS[-1].ar_int = 0;
	AS[-1].ar_type = arINT;
}

void f_minus()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_int = -AS[-1].ar_int;
	when arFLT:
		AS[-1].ar_float = -AS[-1].ar_float;
	}
}

void f_bitcomp()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_int = ~AS[-1].ar_int;
	} else
		ArithError = areINT;
}

void f_sin()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = sin((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = sin(AS[-1].ar_float);
	}
}

void f_cos()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = cos((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = cos(AS[-1].ar_float);
	}
}

void f_tan()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = tan((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = tan(AS[-1].ar_float);
	}
}

void f_asin()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = asin((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = asin(AS[-1].ar_float);
	}
}

void f_acos()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = acos((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = acos(AS[-1].ar_float);
	}
}

void f_atan()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = atan((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = atan(AS[-1].ar_float);
	}
}

void f_atan2()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_type = arFLT;
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= atan2((double)AS[-1].ar_int, (double)AS[0].ar_int);
		else
			AS[-1].ar_float
				= atan2((double)AS[-1].ar_int, AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= atan2(AS[-1].ar_float, (double)AS[0].ar_int);
		else
			AS[-1].ar_float
				= atan2(AS[-1].ar_float, AS[0].ar_float);
	}
}

void f_exp()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = exp((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = exp(AS[-1].ar_float);
	}
}

void f_log()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = log((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = log(AS[-1].ar_float);
	}
}

void f_log10()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = log10((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = log10(AS[-1].ar_float);
	}
}

void f_power()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_type = arFLT;
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= pow((double)AS[-1].ar_int, (double)AS[0].ar_int);
		else
			AS[-1].ar_float
				= pow((double)AS[-1].ar_int, AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= pow(AS[-1].ar_float, (double)AS[0].ar_int);
		else
			AS[-1].ar_float
				= pow(AS[-1].ar_float, AS[0].ar_float);
	}
}

void f_sqrt()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = sqrt((double)AS[-1].ar_int);
	when arFLT:
		AS[-1].ar_float = sqrt(AS[-1].ar_float);
	}
}

void f_integer()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arFLT) {
		AS[-1].ar_type = arINT;
		AS[-1].ar_int = AS[-1].ar_float;
	}
}

void f_float()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = AS[-1].ar_int;
	}
}

void f_round()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_type = arFLT;
		AS[-1].ar_float = AS[-1].ar_int;
	when arFLT:
		AS[-1].ar_float = round(AS[-1].ar_float);
	}
}

void f_lt()
{
	register Arith *AS = --ArithStack;

	switch(AS[-1].ar_type & AS[0].ar_type) {
	when arINT:
		AS[-1].ar_int = (AS[-1].ar_int < AS[0].ar_int);
	when arFLT:
		if(AS[-1].ar_type == arINT) {
			AS[-1].ar_int = (AS[-1].ar_int < AS[0].ar_float);
		} else if(AS[0].ar_type == arINT) {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float < AS[0].ar_int);
		} else {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float < AS[0].ar_float);
		}
	}
}

void f_le()
{
	register Arith *AS = --ArithStack;

	switch(AS[-1].ar_type & AS[0].ar_type) {
	when arINT:
		AS[-1].ar_int = (AS[-1].ar_int <= AS[0].ar_int);
	when arFLT:
		if(AS[-1].ar_type == arINT) {
			AS[-1].ar_int = (AS[-1].ar_int <= AS[0].ar_float);
		} else if(AS[0].ar_type == arINT) {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float <= AS[0].ar_int);
		} else {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float <= AS[0].ar_float);
		}
	}
}

void f_gt()
{
	register Arith *AS = --ArithStack;

	switch(AS[-1].ar_type & AS[0].ar_type) {
	when arINT:
		AS[-1].ar_int = (AS[-1].ar_int > AS[0].ar_int);
	when arFLT:
		if(AS[-1].ar_type == arINT) {
			AS[-1].ar_int = (AS[-1].ar_int > AS[0].ar_float);
		} else if(AS[0].ar_type == arINT) {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float > AS[0].ar_int);
		} else {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float > AS[0].ar_float);
		}
	}
}

void f_ge()
{
	register Arith *AS = --ArithStack;

	switch(AS[-1].ar_type & AS[0].ar_type) {
	when arINT:
		AS[-1].ar_int = (AS[-1].ar_int >= AS[0].ar_int);
	when arFLT:
		if(AS[-1].ar_type == arINT) {
			AS[-1].ar_int = (AS[-1].ar_int >= AS[0].ar_float);
		} else if(AS[0].ar_type == arINT) {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float >= AS[0].ar_int);
		} else {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float >= AS[0].ar_float);
		}
	}
}

void f_eq()
{
	register Arith *AS = --ArithStack;

	switch(AS[-1].ar_type & AS[0].ar_type) {
	when arINT:
		AS[-1].ar_int = (AS[-1].ar_int == AS[0].ar_int);
	when arFLT:
		if(AS[-1].ar_type == arINT) {
			AS[-1].ar_int = (AS[-1].ar_int == AS[0].ar_float);
		} else if(AS[0].ar_type == arINT) {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float == AS[0].ar_int);
		} else {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float == AS[0].ar_float);
		}
	}
}

void f_ne()
{
	register Arith *AS = --ArithStack;

	switch(AS[-1].ar_type & AS[0].ar_type) {
	when arINT:
		AS[-1].ar_int = (AS[-1].ar_int != AS[0].ar_int);
	when arFLT:
		if(AS[-1].ar_type == arINT) {
			AS[-1].ar_int = (AS[-1].ar_int != AS[0].ar_float);
		} else if(AS[0].ar_type == arINT) {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float != AS[0].ar_int);
		} else {
			AS[-1].ar_type = arINT;
			AS[-1].ar_int = (AS[-1].ar_float != AS[0].ar_float);
		}
	}
}

void f_eval()
{
	return;
}

void f_sub_r()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_int = AS[0].ar_int - AS[-1].ar_int;
		else {
			AS[-1].ar_type = arFLT;
			AS[-1].ar_float = AS[0].ar_float - AS[-1].ar_int;
		}
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float = AS[0].ar_int - AS[-1].ar_float;
		else
			AS[-1].ar_float = AS[0].ar_float - AS[-1].ar_float;
	}
}

void f_div_r()
{
	register Arith *AS = --ArithStack;

	ARITHCHECKNEW(AS[-1], == 0, areDIVZERO);
	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_type = arFLT;
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float = AS[0].ar_int / AS[-1].ar_int;
		else
			AS[-1].ar_float = AS[0].ar_float / AS[-1].ar_int;
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float = AS[0].ar_int / AS[-1].ar_float;
		else
			AS[-1].ar_float = AS[0].ar_float / AS[-1].ar_float;
	}
}

void f_intdiv_r()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type & AS[0].ar_type == arINT) {
		if(AS[-1].ar_int == 0) {
			ArithError = areDIVZERO;
			return;
		}
		AS[-1].ar_int = AS[0].ar_int / AS[-1].ar_int;
	} else
		ArithError = areINT;
}

void f_mod_r()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type & AS[0].ar_type == arINT) {
		if(AS[-1].ar_int == 0) {
			ArithError = areDIVZERO;
			return;
		}
		AS[-1].ar_int = AS[0].ar_int % AS[-1].ar_int;
	} else
		ArithError = areINT;
}

void f_lsh_r()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type & AS[0].ar_type == arINT) {
		AS[-1].ar_int = AS[0].ar_int << AS[-1].ar_int;
	} else
		ArithError = areINT;
}

void f_rsh_r()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type & AS[0].ar_type == arINT) {
		AS[-1].ar_int = AS[0].ar_int >> AS[-1].ar_int;
	} else
		ArithError = areINT;
}

void f_atan2_r()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_type = arFLT;
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= atan2((double)AS[0].ar_int, (double)AS[-1].ar_int);
		else
			AS[-1].ar_float
				= atan2(AS[0].ar_float, (double)AS[-1].ar_int);
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= atan2((double)AS[0].ar_int, AS[-1].ar_float);
		else
			AS[-1].ar_float
				= atan2(AS[0].ar_float, AS[-1].ar_float);
	}
}

void f_power_r()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_type = arFLT;
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= pow((double)AS[0].ar_int, (double)AS[-1].ar_int);
		else
			AS[-1].ar_float
				= pow(AS[0].ar_float, (double)AS[-1].ar_int);
	} else {
		if(AS[0].ar_type == arINT)
			AS[-1].ar_float
				= pow((double)AS[0].ar_int, AS[-1].ar_float);
		else
			AS[-1].ar_float
				= pow(AS[0].ar_float, AS[-1].ar_float);
	}
}

void f_add1()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_int += 1;
	when arFLT:
		AS[-1].ar_float += 1;
	}
}

void f_sub1()
{
	register Arith *AS = ArithStack;

	switch(AS[-1].ar_type) {
	when arINT:
		AS[-1].ar_int -= 1;
	when arFLT:
		AS[-1].ar_float -= 1;
	}
}

void f_byte()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_int = *(char *) (AS[-1].ar_int);
	} else
		ArithError = areINT;
}

void f_ubyte()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_int = *(unsigned char *) (AS[-1].ar_int);
	} else
		ArithError = areINT;
}

void f_half()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_int = *(short *) (AS[-1].ar_int);
	} else
		ArithError = areINT;
}

void f_uhalf()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_int = *(unsigned short *) (AS[-1].ar_int);
	} else
		ArithError = areINT;
}

void f_word()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_int = *(Word *) (AS[-1].ar_int);
	} else
		ArithError = areINT;
}

void f_address()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_int = *(Word *) (AS[-1].ar_int);
	} else
		ArithError = areINT;
}

void f_single()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_float = *(float *) (AS[-1].ar_int);
		AS[-1].ar_type = arFLT;
	} else
		ArithError = areINT;
}

void f_double()
{
	register Arith *AS = ArithStack;

	if(AS[-1].ar_type == arINT) {
		AS[-1].ar_float = *(double *) (AS[-1].ar_int);
		AS[-1].ar_type = arFLT;
	} else
		ArithError = areINT;
}

/*
 * Arithmetic predicates.
 *
 * These return 0 is false, and 1 otherwise.  Errors should return 0
 * if silent (there aren't any of these) and 1 otherwise.
 */

int p_and()
{
	register Arith *AS = --ArithStack;
	register int t;

	t = AS[-1].ar_type;
	if(		t == arINT && AS[-1].ar_int == 0
		||	t == arFLT && AS[-1].ar_float == 0.0)
		return(0);
	t = AS[0].ar_type;
	if(		t == arINT && AS[0].ar_int == 0
		||	t == arFLT && AS[0].ar_float == 0.0)
		return(0);
	else
		return(1);
}

int p_or()
{
	register Arith *AS = --ArithStack;
	register int t;

	t = AS[-1].ar_type;
	if(		t == arINT && AS[-1].ar_int != 0
		||	t == arFLT && AS[-1].ar_float != 0.0)
		return(1);
	t = AS[0].ar_type;
	if(		t == arINT && AS[0].ar_int != 0
		||	t == arFLT && AS[0].ar_float != 0.0)
		return(1);
	else
		return(0);
}

int p_lt()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_int < AS[0].ar_int);
		else
			return(AS[-1].ar_int < AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_float < AS[0].ar_int);
		else
			return(AS[-1].ar_float < AS[0].ar_float);
	}
}

int p_le()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_int <= AS[0].ar_int);
		else
			return(AS[-1].ar_int <= AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_float <= AS[0].ar_int);
		else
			return(AS[-1].ar_float <= AS[0].ar_float);
	}
}

int p_gt()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_int > AS[0].ar_int);
		else
			return(AS[-1].ar_int > AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_float > AS[0].ar_int);
		else
			return(AS[-1].ar_float > AS[0].ar_float);
	}
}

int p_ge()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_int >= AS[0].ar_int);
		else
			return(AS[-1].ar_int >= AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_float >= AS[0].ar_int);
		else
			return(AS[-1].ar_float >= AS[0].ar_float);
	}
}

int p_eq()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_int == AS[0].ar_int);
		else
			return(AS[-1].ar_int == AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_float == AS[0].ar_int);
		else
			return(AS[-1].ar_float == AS[0].ar_float);
	}
}

int p_ne()
{
	register Arith *AS = --ArithStack;

	if(AS[-1].ar_type == arINT) {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_int != AS[0].ar_int);
		else
			return(AS[-1].ar_int != AS[0].ar_float);
	} else {
		if(AS[0].ar_type == arINT)
			return(AS[-1].ar_float != AS[0].ar_int);
		else
			return(AS[-1].ar_float != AS[0].ar_float);
	}
}

/*
 * Evaluate a term as an arithmetic expression.
 *
 * The arithmetic stack delineated by ArithStack and ArithStackMax
 * is used and the result is left on the top of it.  (The caller
 * may want to post-increment the stack pointer to take account of this.)
 *
 * Eval() returns NIL if v is ground, the atom $evalFailed if an
 * error occurs, or the first variable encountered otherwise.
 *
 * DelayArith and ArithError are set, but not cleared, if appropriate.
 */
Object
eval(x)
register Object x;
{
	register Object v;
	register Arith *AS = ArithStack;
	Arith *ASTOP;					/* Place to put input term on error */

	if(AS >= ArithStackMax)
		panic("Arithmetic stack overflow in eval()");
	ASTOP = AS;
	DeRef(x);
	/*
	 * If successful, or delayed, or some specific error,
	 * return immediately, otherwise fall through and report areNON.
	 */
	switch(eType(x)) {
	when tREF:
	case tDEL:
		DelayArith = x;
		AS->ar_type = arNAN;
		AS->ar_value = x;
		return(x);
	when tICN:
		switch(eCType(x)) {
		when ctINT:
			AS->ar_type = arINT;
			AS->ar_int = eSmallInt(x);
			return(NIL);
		when ctATM:
			switch(systemAtomIndex(((Atom *) eRef(x)))) {
			when indSymMaxint:
				AS->ar_type = arINT;
				AS->ar_int = PMAXINT;
				return(NIL);
			when indSymMinint:
				AS->ar_type = arINT;
				AS->ar_int = PMININT;
				return(NIL);
			when indSymPi:
				AS->ar_type = arFLT;
				AS->ar_float = PI;
				return(NIL);
			}
		}
	when tUCN:
		if(IsFloat(x)) {
			AS->ar_type = arFLT;
			AS->ar_float = eFloat(x);
			return(NIL);
		} else if(IsInt32(x)) {
			AS->ar_type = arINT;
			AS->ar_int = eInt32(x);
			return(NIL);
		}
	when tLST: {
		register Object p = deRef(((Object *)eRef(x))[1]);
		if(IsVar(p)) {
			DelayArith = p;
			AS->ar_type = arNAN;
			AS->ar_value = x;
			return p;
		} else if(p != NIL)
			goto error;
		v = eval(((Object *)eRef(x))[0]);
		if(v != NIL) {
			AS->ar_type = arNAN;
			AS->ar_value = x;
		}
		return v;
	}
	when tCHR: {
		register char *p = (char *)eRef(x);
		if(/* Can't happen p[0] == '\0' || */ p[1] != '\0')
			goto error;
		AS->ar_type = arINT;
		AS->ar_int = p[0];
		return(NIL);
	}
	when tSTR: {
		register Object *S;
		register PFVoid f;

		S = eRef(x);
		switch(eNArgs(*(Structure *)S)) {
		when 1:
			ArithStack = AS;
			v = eval(S[1]);
			if(v != NIL) {
				AS->ar_type = arNAN;
				AS->ar_value = x;
				return(v);
			}
			ArithStack = AS + 1;
			switch(systemAtomIndex(eFunctor(*(Structure *)S))) {
			when indSymAcos: f = f_acos;
			when indSymAdd: f = f_eval;
			when indSymAdd1: f = f_add1;
			when indSymAddressAt:
			case indSymAddress_at: f = f_address;
			when indSymAsin: f = f_asin;
			when indSymAtan: f = f_atan;
			when indSymBackslash: f = f_bitcomp;
			when indSymCos: f = f_cos;
			when indSymDoubleAt:
			case indSymDouble_at: f = f_double;
			when indSymExp: f = f_exp;
			when indSymFloat: f = f_float;
			when indSymInteger: f = f_integer;
			when indSymInteger8At:
			case indSymInteger_8_at: f = f_byte;
			when indSymInteger16At:
			case indSymInteger_16_at: f = f_half;
			when indSymIntegerAt:
			case indSymInteger_at: f = f_word;
			when indSymLog: f = f_log;
			when indSymLog10: f = f_log10;
			when indSymMinus: f = f_minus;
			when indSymMinus1: f = f_sub1;
			when indSymNot: f = f_lognot;
			when indSymRound: f = f_round;
			when indSymSin: f = f_sin;
			when indSymSingleAt:
			case indSymSingle_at: f = f_single;
			when indSymSqrt: f = f_sqrt;
			when indSymTan: f = f_tan;
			when indSymUnsigned8At:
			case indSymUnsigned_8_at: f = f_ubyte;
			when indSymUnsigned16At:
			case indSymUnsigned_16_at: f = f_uhalf;
			break;
			default:
				goto error;
			}
		when 2:
			ArithStack = AS;
			v = eval(S[1]);
			if(v != NIL) {
				AS->ar_type = arNAN;
				AS->ar_value = x;
				return(v);
			}
			ArithStack = AS + 1;
			v = eval(S[2]);
			if(v != NIL) {
				ArithStack = AS;
				AS->ar_type = arNAN;
				AS->ar_value = x;
				return(v);
			}
			ArithStack = AS + 2;
			switch(systemAtomIndex(eFunctor(*(Structure *)S))) {
			when indSymAdd: f = f_add;
			when indSymAnd: f = f_logand;
			when indSymAtan2: f = f_atan2;
			when indSymBitAnd: f = f_bitand;
			when indSymBitOr: f = f_bitor;
			when indSymBitXOr: f = f_bitxor;
			when indSymDiv: f = f_div;
			when indSymEQ: f = f_eq;
			when indSymGE: f = f_ge;
			when indSymGT: f = f_gt;
			when indSymIntDiv: f = f_intdiv;
			when indSymLE: f = f_le;
			when indSymLSH: f = f_lsh;
			when indSymLT: f = f_lt;
			when indSymMinus: f = f_sub;
			when indSymMod: f = f_mod;
			when indSymMult: f = f_mult;
			when indSymNE: f = f_ne;
			when indSymOr: f = f_logor;
			when indSymRSH: f = f_rsh;
			when indSymStarStar: f = f_power;
			break;
			default:
				goto error;
			}
		break;
		default:
			goto error;
		}
		(*f)();
		ArithStack = AS;
		if(ArithError == areOK)
			return(NIL);
		else {
			AS->ar_type = arNAN;
			AS->ar_value = x;
			return(StarToAtom(&Sym_EvalFailed));
		}
	}
	when tBMV:
		/* Fall through */
		;
	}
error:
	ArithStack = AS;
	ArithError = areNON;
	AS->ar_type = arNAN;
	AS->ar_value = x;
	return(StarToAtom(&Sym_EvalFailed));
}

p_plus(X1, X2, X3)
register Object X1, X2, X3;
{
	register Object *X;
	register Delay *CD;
	Object XS0, XS1, XS2;

	DeRefPure(X1); DeRefPure(X2); DeRefPure(X3);

	if(IsInt(X1)) {
		if(IsInt(X2)) {
			X1 = eInt(X1) + eInt(X2);
			return(unify(X3, MakeInt(X1, CMR->mr_h)));
		} else if(IsInt(X3)) {
			X3 = eInt(X3) - eInt(X1);
			return(unify(X2, MakeInt(X3, CMR->mr_h)));
		}
	} else if(IsInt(X2) && IsInt(X3)) {
		X3 = eInt(X3) - eInt(X2);
		return(unify(X1, MakeInt(X3, CMR->mr_h)));
	}

	X = CMR->mr_x;
	XS0 = X[0]; XS1 = X[1]; XS2 = X[2];
	X[0] = X1; X[1] = X2; X[2] = X3;
	/* SaveForBuilding; */
	CD = makeDelay(3, WPlus);
	/* Note that plus(X, Y, Z) will get all 3 variables marked. */
	if(IsVar(X1))
		mark(X1, CD);
	else if(!IsInt(X1))
		return(0);
	if(IsVar(X2))
		mark(X2, CD);
	else if(!IsInt(X2))
		return(0);
	if(IsVar(X3))
		mark(X3, CD);
	else if(!IsInt(X3))
		return(0);
	/* RestoreAfterBuilding; */
	X[0] = XS0; X[1] = XS1; X[2] = XS2;
	return(1);
}

/*
 * On some systems, this should just be rint().
 */
double
round(x)
register double x;
{
	if(x >= 0)
		return(floor(x + 0.5));
	else
		return(-floor(-x + 0.5));
}
