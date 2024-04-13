SUB_DIRECTORIES=frontend
CLEAN_TARGETS=$(addsuffix clean,$(SUB_DIRECTORIES))

.PHONY: all clean ${SUB_DIRECTORIES} ${CLEAN_TARGETS}

all: ${SUB_DIRECTORIES}
${SUB_DIRECTORIES}:
	${MAKE} -C $@

clean: ${CLEAN_TARGETS}
%clean: %
	${MAKE} -C $< clean
