BIN = mwdict

.PHONY: install

install:
	@if [ -d ~/bin ]; then cp $(BIN) ~/bin/; \
	else cp $(BIN) /usr/local/bin/; fi
