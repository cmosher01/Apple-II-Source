.SUFFIXES: .asciidoc .html

.asciidoc.html :
	asciidoc -b html5 $<

all : doc

doc : index.html

clean :
	rm -f index.html

install :
	mkdir -p apple2
	cp index.html apple2
	cp warning.png apple2
	cp redhat.png apple2
	cp debian.png apple2
	cp windows.gif apple2
	cp make.png apple2
