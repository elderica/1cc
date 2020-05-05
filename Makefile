ifeq ($(LISP),)
	LISP=ccl
endif

test:
	$(LISP) --eval '(progn (asdf:test-system :1cc) (uiop:quit 0))'

clean:
	rm -f *.o *~ tmp* *.FASL *.fasl *.*fsl

.PHONY: test clean
