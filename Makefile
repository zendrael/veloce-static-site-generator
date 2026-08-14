# Makefile para Veloce SSG

FPC = fpc
FLAGS = -O3 -XX -Xs -vewnhiq
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
STRIP_CMD = strip -x
else
STRIP_CMD = strip --strip-all
endif

all: veloce

veloce: veloce.pas veloce_utils.pas veloce_config.pas veloce_markdown.pas veloce_template.pas veloce_builder.pas
	$(FPC) $(FLAGS) veloce.pas
	$(STRIP_CMD) veloce
	upx --best --ultra-brute veloce

clean:
	rm -f veloce
	rm -f *.o
	rm -f *.ppu
	rm -rf dist/ dev/

install: veloce
	cp veloce /usr/local/bin/

.PHONY: all clean install
