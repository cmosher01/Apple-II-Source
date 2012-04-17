.SUFFIXES: .asciidoc .html

.asciidoc.html :
	asciidoc -b html5 $<

all : doc

doc : index.html

clean :
	rm -f index.html
