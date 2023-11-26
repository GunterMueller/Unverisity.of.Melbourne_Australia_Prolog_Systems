OBJS=	bdesc.o bget.o bmatch.o bput.o bsplit.o fact.o factio.o fquery.o desc.o hash.o rassert.o rclose.o rcreat.o rerror.o rhdput.o ropen.o rtrans.o
.c.o:
	${CC} ${CFLAGS} -c $*.c

../dblib: ${OBJS}
	rm -f ../dblib
	ar cr ../dblib ${OBJS}
	-ranlib ../dblib

bdesc.o: bucket.h desc.h rel.h fact.h
bget.o: bucket.h desc.h rel.h
bmatch.o: bucket.h rel.h fact.h
bput.o: bucket.h desc.h rel.h
bsplit.o: bucket.h desc.h rel.h
desc.o: desc.h rel.h
fact.o: fact.h
factio.o: desc.h fact.h
fquery.o: rel.h fact.h
hash.o: desc.h fact.h rel.h
rassert.o: rel.h bucket.h fact.h desc.h
rclose.o: rel.h bucket.h fact.h desc.h
rcreat.o: rel.h bucket.h fact.h desc.h
rerror.o:
rhdput.o: rel.h bucket.h fact.h desc.h
ropen.o: rel.h bucket.h fact.h desc.h
rtrans.o: rel.h bucket.h fact.h desc.h

lint:
	lint -abchx *.c

clean:
	rm -f ${OBJS} core 
