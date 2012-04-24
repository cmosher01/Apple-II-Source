.SUFFIXES: .asciidoc .html

.asciidoc.html :
	asciidoc -b html5 $<

HTMLS=index.html breakout.html
IMGS=warning.png

all : doc

doc : $(HTMLS) $(IMGS)

clean :
	rm -f $(HTMLS)

install :
	mkdir -p apple2
	cp $(HTMLS) apple2
	cp $(IMGS) apple2

uninstall :
	rm -Rf apple2
