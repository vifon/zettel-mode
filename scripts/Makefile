CACHES = .cache/backrefs.el .cache/refs.el

.PHONY: all
all: $(CACHES)

.PHONY: clean
clean:
	$(RM) $(CACHES)

.cache/backrefs.el: $(wildcard *.org) | .cache
	emacsclient --eval "(progn (require 'zettel-mode) (zettel--get-all #'zettel--get-backrefs))" > $@

.cache/refs.el: $(wildcard *.org) | .cache
	emacsclient --eval "(progn (require 'zettel-mode) (zettel--get-all #'zettel--get-refs))" > $@

.cache:
	mkdir -p .cache
