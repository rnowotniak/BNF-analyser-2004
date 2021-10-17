#
# Copyright (C) 2004   Robert Nowotniak <robercik@toya.net.pl>
#

CFLAGS=-O3
BIN=../bin

all: analizator wrapper

analizator: *.adb *.ads
	@gnatmake ${CFLAGS} analysebnf

wrapper: wrapper.c
	@gcc -DANALIZ_PATH=\"${BIN}\" ${CFLAGS} -Wall $> -o $@

install: all
	@cp -f analysebnf wrapper ${BIN}
	@chmod 755 ${BIN}/analysebnf ${BIN}/wrapper

clean:
	@rm -f *.o *.ali analysebnf analysebnf.core tags wrapper

tags: all
	@gnatxref -v *.adb *.ads > tags

package: clean
	@cd ..; A=$$(basename `pwd`); TMP=`mktemp -t $$A`; \
	tar -C .. --exclude $$A/tmp/[gn]* --exclude $$A/$$A.tgz -czpvf $$TMP $$A; \
	mv $$TMP $$A.tgz; chmod 604 $$A.tgz

#	@cd ..; MP=$$(mktemp -t $$0); A=`basename $$0'; echo $$TMP; echo $$A

#echo tar -C .. -czpvf  $$A

