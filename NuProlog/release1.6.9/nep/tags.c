/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "machdep.h"

#define ExtSignedField(x, base, len) \
	((Word)((x) << (WORDSIZE-(base)-(len))) >> (WORDSIZE-(len)))
#define FormField(x, base, len) (((x) & ~(-1<<(len))) << (base))
#define MakeMask(base, len) (~(-1<<(len)) << (base))

/* Object Word
typedef union {
	struct {
		Indirect: 1;
		Type: 3;
		CType: 2;
		Ref: 26;
	} v1;
	struct {
		dummy1: 1;
		SType: 2;
		DType: 1;
		Value: 28;
	} v2;
	struct {
		Tag: 4;
		dummy2: 2;
		CValue: 26;
	} v3;
	struct {
		dummy3: 1;
		TypedValue: 31;
	} v4;
	struct {
		dummy4: 3;
		UType: 3;
		dummy5: 26;
	} v5;
	struct {
		FType: 6;
		dummy6: 26;
	} v6;
} Object;
*/

#ifdef LBT
#define lofIndirect 0
#define bofIndirect 0
#define lofTypedValue WORDSIZE
#define bofTypedValue 0
#define lofType 3
#define bofType lofCType
#define lofTag lofType
#define bofTag bofType
#define lofDType 1
#define bofDType lofCType
#define lofSType 2
#define bofSType (lofDType + bofType)
#define lofCType 2
#define bofCType 0
#define lofUType 3
#define bofUType bofCType
#define lofFType (lofType + lofCType)
#define bofFType bofCType
#define lofRef (WORDSIZE - lofFType)
#define bofRef lofFType
#define lofValue lofRef
#define bofValue bofRef
#define lofCValue lofRef
#define bofCValue bofRef
#else /* LBT */
#ifdef LARGEMODEL
#define lofIndirect 0
#else /* LARGEMODEL */
#define lofIndirect 1
#endif /* LARGEMODEL */
#define bofIndirect (WORDSIZE-lofIndirect)
#define lofTypedValue bofIndirect
#define bofTypedValue 0
#define lofType 3
#define bofType (bofIndirect-lofType)
#define lofTag (lofIndirect+lofType)
#define bofTag (WORDSIZE-lofTag)
#define lofDType 1
#define bofDType bofType
#define lofSType 2
#define bofSType (bofDType+lofDType)
#define lofCType 2
#define bofCType (bofType-lofCType)
#define lofUType 3
#define bofUType bofCType
#define lofFType (lofIndirect + lofType + lofCType)
#define bofFType bofCType
#define lofRef bofCType
#define bofRef 0
#define lofValue bofType
#define bofValue 0
#define lofCValue bofCType
#define bofCValue 0
#endif /* LBT */

typedef struct {
	int cr_value;
	char *cr_reg;
	char *cr_type;
} CReg;

static CReg cregs[] = {
	{ MakeMask(bofRef, lofRef), "g3", "UWord"},
};

void
defineTag(name, value)
char *name;
int value;
{
	printf("#define %s %d\n", name, value);
}

void
defineE(name, type, base, length)
char *name, *type;
int base, length;
{
	if(base + length == WORDSIZE)
		printf("#define %s(x) (%s(((UWord)(x)) >> %d))\n", name, type, base);
	else if(base == 0) {
#if 0
#ifdef INLINEsparc
		register CReg *c;

		for(c = cregs; c < cregs + sizeof(cregs)/sizeof(CReg); c++)
			if(c->cr_value == MakeMask(base, length)) {
				printf("#define %s(x) (%s(((UWord)(x)) & getreg_%s()))\n",
						name, type, c->cr_reg);
				return;
			}
#endif /* INLINEsparc */
#endif /* 0 */
#ifdef sparc
		if(length > 12) {
			printf("#define %s(x) (%s((((UWord)(x)) << %d) >> %d))\n",
					name, type, 32 - length, 32 - length);
			return;
		}
#endif /* sparc */
		printf("#define %s(x) (%s(((UWord)(x)) & 0x%x))\n",
				name, type, ~(-1 << length));
	} else {
#ifdef INLINEmc68020
		printf("#define %s(x) (%sExtField_%d_%d((UWord)(x)))\n",
				name, type, 32 - base - length, length);
#else /* INLINEmc68020 */
		printf("#define %s(x) (%s((((UWord)(x)) >> %d) & 0x%x))\n",
				name, type, base, ~(-1 << length));
#endif /* INLINEmc68020 */
	}
}

main()
{
#if 0
#ifdef INLINEsparc
	{
		register CReg *c;

		for(c = cregs; c < cregs + sizeof(cregs)/sizeof(CReg); c++) {
			printf("extern %s getreg_%s();\n", c->cr_type, c->cr_reg);
			printf("extern void setreg_%s();\n", c->cr_reg);
			printf("#define INITREG_%s setreg_%s(0x%x)\n",
				c->cr_reg, c->cr_reg, c->cr_value);
		}
	}
#endif /* INLINEsparc */
#endif /* 0 */

	defineTag("lofIndirect", lofIndirect);
	defineTag("bofIndirect", bofIndirect);
	defineTag("lofTypedValue", lofTypedValue);
	defineTag("bofTypedValue", bofTypedValue);
	defineTag("lofType", lofType);
	defineTag("bofType", bofType);
	defineTag("lofTag", lofTag);
	defineTag("bofTag", bofTag);
	defineTag("lofDType", lofDType);
	defineTag("bofDType", bofDType);
	defineTag("lofSType", lofSType);
	defineTag("bofSType", bofSType);
	defineTag("lofCType", lofCType);
	defineTag("bofCType", bofCType);
	defineTag("lofUType", lofUType);
	defineTag("bofUType", bofUType);
	defineTag("lofFType", lofFType);
	defineTag("bofFType", bofFType);
	defineTag("lofRef", lofRef);
	defineTag("bofRef", bofRef);
	defineTag("lofValue", lofValue);
	defineTag("bofValue", bofValue);
	defineTag("lofCValue", lofCValue);
	defineTag("bofCValue", bofCValue);

#ifndef LBT
	defineE("eIndirect", "", bofIndirect, lofIndirect);
#endif /* LBT */
	defineE("eTypedValue", "", bofTypedValue, lofTypedValue);
	defineE("eType", "", bofType, lofType);
	defineE("eSType", "", bofSType, lofSType);
	defineE("eDType", "", bofDType, lofDType);
	defineE("eCType", "", bofCType, lofCType);
	defineE("eUType", "", bofUType, lofUType);
	defineE("eFType", "", bofFType, lofFType);
	defineE("eTag", "", bofTag, lofTag);
	defineE("eRef", "(Object *)", bofRef, lofRef);
	defineE("eValue", "", bofValue, lofValue);
	defineE("eCValue", "", bofCValue, lofCValue);

	exit(0);
}

#if 0
#define fIndirect(x) FormField(x, bofIndirect, lofIndirect)
#define fTypedValue(x) FormField(x, bofTypedValue, lofTypedValue)
#define fType(x) FormField(x, bofType, lofType)
#define fSType(x) FormField(x, bofSType, lofSType)
#define fDType(x) FormField(x, bofDType, lofDType)
#define fCType(x) FormField(x, bofCType, lofCType)
#define fUType(x) FormField(x, bofUType, lofUType)
#define fFType(i, t, c) \
	FormField(((i) << lofType | (t)) << lofCType | (c), bofFType, lofFType)
#define fTag(i, x) FormField((i) << lofType | (x), bofTag, lofTag)
#define fRef(x) FormField(x, bofRef, lofRef)
#define fValue(x) FormField(x, bofValue, lofValue)
#define fCValue(x) FormField(x, bofCValue, lofCValue)
#endif
