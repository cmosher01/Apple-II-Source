.SUFFIXES: .asciidoc .html

.asciidoc.html :
	asciidoc -b html5 $<

HTMLS=index.html breakout.html
IMGS=warning.png breakout.png

.PHONY: all doc clean site

all : doc

doc : $(HTMLS) $(IMGS)

clean :
	rm -f $(HTMLS)
	rm -Rf site

site :
	mkdir -p site
	cp $(HTMLS) site
	cp $(IMGS) site
