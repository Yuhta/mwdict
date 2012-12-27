BIN = mwdict mwdict-suggest
COMP_SRC = mwdict-comp
COMP_DST = ~/.mwdict-comp

.PHONY: install script completion

install: script completion

script:
	@if [ -d ~/bin ]; then \
	  cp $(BIN) ~/bin/; \
	else \
	  cp $(BIN) /usr/local/bin/; \
	fi
completion:
	@cp $(COMP_SRC) $(COMP_DST); \
	echo ". $(COMP_DST)" >>~/.bashrc
