PACKAGENAME=tilda
COLLECTS=tilda

all: setup

clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf docs

setup:
	raco setup $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

test: setup testonly

testonly:
	raco test -p $(PACKAGENAME)

docs:
	raco scribble \
		--html \
		--dest docs \
		--dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		\
		tilda.scrbl
