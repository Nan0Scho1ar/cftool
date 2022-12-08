# cftool
#
# Manage AWS CloudFormation Stacks
# Copyright (C) 2022 Nan0Scho1ar (Christopher Mackinga)
# Licence: GPL-v3
# See LICENSE file for copyright and license details.

include config.mk

SRC = cftool

cftool:
	raco exe cftool.rkt

install:
	mkdir -p ${DESTDIR}${PREFIX}/bin
	cp -f ${SRC} ${DESTDIR}${PREFIX}/bin/cftool
	chmod 755 ${DESTDIR}${PREFIX}/bin/cftool
# optional if manpage added down the road
# mkdir -p ${DESTDIR}${MANPREFIX}/man1
# sed "s/VERSION/${VERSION}/g" < cftool.1 > ${DESTDIR}${MANPREFIX}/man1/cftool.1
# chmod 644 ${DESTDIR}${MANPREFIX}/man1/cftool.1

uninstall:
	rm -f ${DESTDIR}${PREFIX}/bin/cftool
# optional if manpage added down the road
#	rm -f ${DESTDIR}${MANPREFIX}/man1/cftool.1

clean:
	rm ${SRC}

.PHONY: cftool install uninstall clean



# end
