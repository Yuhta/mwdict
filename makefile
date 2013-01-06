BIN = mwdict mwdict-suggest
COMP = mwdict-comp.sh
CFG_DIR = ~/.mwdict/
ENTR_DIR = $(CFG_DIR)entries/
INSTALL = script completion entries

.PHONY: install $(INSTALL)

install: $(INSTALL)

script:
	@chmod 755 $(BIN); \
	if [ -d ~/bin ]; then \
	  cp $(BIN) ~/bin/; \
	else \
	  cp $(BIN) /usr/local/bin/; \
	fi

completion:
	@if [ ! -e $(CFG_DIR) ]; then \
	  mkdir $(CFG_DIR) && \
	  cp $(COMP) $(CFG_DIR) && \
	  echo ". $(CFG_DIR)$(COMP)" >>~/.bashrc; \
	fi

entries:
	@if [ ! -e $(ENTR_DIR) ]; then mkdir $(ENTR_DIR); fi
