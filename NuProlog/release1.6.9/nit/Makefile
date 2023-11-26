#
# NU-Prolog 1.4 NIT Makefile
#
# To make "nit", NU-Prolog must be *installed*

# Configuration Parameters:
#
#	These definitions are local only.
#	The real definitions will be passed in
#	at installation time by the main NU-Prolog Makefile.

SRC	= /nip/jws/release

BIN	= /nip/usr/bin

LIB	= /nip/usr/lib/nuprolog

SHELL = /bin/sh

CDEFS	=

CONFIG	= SRC="$(SRC)" BIN="$(BIN)" LIB="$(LIB)" CDEFS="$(CDEFS)" NUVERSION="$(NUVERSION)"

# End Configuration Parameters


# Files needed to build Nit

NC	= $(BIN)/nc
NIT	= $(BIN)/nit
NITS	= $(LIB)/bin.$(NUVERSION)/nit.save

.SUFFIXES: .no .ns .nl

.nl.ns: ; $(NC) -S $*.nl
.ns.no: ; $(NC) -c $*.ns

NLSRCS	= pos.nl pred.nl strat.nl var.nl args.nl main.nl
SRCS	= pos.ns pred.ns strat.ns var.ns args.ns main.ns
OBJS	= pos.no pred.no strat.no var.no args.no main.no

# Configure and compile Nit

all:  nit
	@-echo Nit built

nit nit.save:  $(OBJS) $(LIB)/bin.$(NUVERSION)/nep $(LIB)/lib.$(NUVERSION)/debug.no
	@-rm -f nit.save
	$(NC) -o nit $(OBJS) -l debug

args.no: args.ns
main.no: main.ns
pos.no: pos.ns
pred.no: pred.ns
strat.no: strat.ns
var.no: var.ns

$(LIB)/lib.$(NUVERSION)/debug.no: $(LIB)/lib.$(NUVERSION)/debug.nl
	@-echo Recompile the debug library!
	exit 1

# Install Nit

install:  $(NITS)
	@-echo Nit installed

$(NIT):  nit
	cp nit $(BIN)

$(NITS): nit.save
	echo $(LIB)/bin.$(NUVERSION)/nep -R $(NITS) -a '$$*' > $(NIT)
	cp nit.save $(NITS)

# Other things to do with Nit

# Clean up some mess

clean:
	-rm -f *.no *.ns
	rm -f nit nit.save core .errs ERRS
