# Makefile para Veloce SSG

FPC = fpc
FLAGS = -O3 -XX -Xs -vewnhiq

all: veloce

veloce: veloce.pas veloce_utils.pas veloce_config.pas veloce_markdown.pas veloce_template.pas veloce_builder.pas
	$(FPC) $(FLAGS) veloce.pas

clean:
	rm -f veloce
	rm -f *.o
	rm -f *.ppu
	rm -rf dist/ dev/

install: veloce
	cp veloce /usr/local/bin/

.PHONY: all clean install
