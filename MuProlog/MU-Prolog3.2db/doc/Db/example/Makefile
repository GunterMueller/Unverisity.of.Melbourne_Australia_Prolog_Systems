#
# Database test example
#

all: mkdb gendata
	sh mkdb

gendata: gendata.c
	$(CC) -o gendata gendata.c

clean:
	rm -f gendata gendata.o
	rm -fr db
