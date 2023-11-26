/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * labels.c  -  Nepolog assembler (label table functions)
 */

#include "na.h"

Label	labels[LABTABSIZE];

/*
 * find_label:
 *	Return a pointer to a Label entry for a label in the table
 *	Makes a new entry if one doesn't already exist
 */
Label *
find_label(lab)
int lab;
{
	_int	i;
	_Label	*l, *lprev;

	i = lab % LABTABSIZE;
	if (labels[i].label == -1)
		l = &labels[i];
	else {
		for (l = &labels[i]; l != LabelNULL; lprev = l, l = l->lab_next)
			if (l->label == lab)
				return(l);
		if ((l= mkLabel()) == LabelNULL)
			fatal("run out of room to make new labels");
		lprev->lab_next = l;
	}
	l->label = lab; l->lab_next = LabelNULL;
	l->lab_code = l->lab_data = l->lab_heap = lUNUSED;
	return(l);
}

/*
 * clear_labels:
 *	Clear out the label table
 */
void
clear_labels()
{
	_int	i;
	_Label	*l, *lnext;

	for (i = 0; i < LABTABSIZE; i++)
	{
		for (l = labels[i].lab_next; l != LabelNULL; l = lnext)
		{
			if ((label_used(l->lab_code) && !label_defined(l->lab_code)) ||
			    (label_used(l->lab_data) && !label_defined(l->lab_data)) ||
			    (label_used(l->lab_heap) && !label_defined(l->lab_heap))  )
				err("label %d undefined in module %s", i, module_name);
			lnext = l->lab_next;
			free(l);
		}
		labels[i].label = -1;
		labels[i].lab_next = LabelNULL;
	}
}

/*
 * init_labels:
 *	Initialise label hash table
 */
void
init_labels()
{
	_int	i;

	for (i = 0; i < LABTABSIZE; i++)
	{
		labels[i].label = -1;
		labels[i].lab_next = LabelNULL;
	}
}
