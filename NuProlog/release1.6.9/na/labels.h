/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * labels.h  -  Nepolog assembler (label table definitions)
 */

#ifndef LABELS_H
#define LABELS_H

#include "defs.h"

#define	LABTABSIZE	1000

struct Label {
	int	label;
	Word	lab_code;
	Word	lab_heap;
	Word	lab_data;
	struct	Label	*lab_next;
};

typedef	struct Label	Label;
#define	_Label		register Label
#define	LabelNULL	((Label *)NULL)

extern	Label	labels[];	/* label hash table */

#define	label_used(a)		((a) != lUNUSED)
#define	label_defined(a)	(((a) & lUNDEFINED) == 0)
#define	label_addr(a)		((a) & lADDRESS)
#define	label_forward(a)	((a) | lUNDEFINED)

#define	mkLabel()		((Label *)malloc(sizeof(Label)))

extern	Label	*find_label();
extern	void	clear_labels();
extern	void	init_labels();

#endif /* LABELS_H */
