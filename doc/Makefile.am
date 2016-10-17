#@configure_input@

.asciidoc.html:
	asciidoc -b html5 $<



HTMLS=index.html breakout.html
IMGS=warning.png breakout.png

EXTRA_DIST = $(HTMLS) $(IMGS)

.PHONY: all doc

all: doc

doc: $(HTMLS) $(IMGS)
