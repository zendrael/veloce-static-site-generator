#!/bin/sh

# Veloce builder

fpc veloce.pas -Os -CX -O3 -Ooregvar -Xg -Xs -XX \
-Fu../lib

case "$1" in
	'clean')
		echo "Limpando arquivos..."
		find ./ -name \*.a | xargs rm -f 
		find ./ -name \*.o | xargs rm -f 
		find ./ -name \*.ppu | xargs rm -f 
		find ./ -name \*.or | xargs rm -f 
		find ./ -name \*.compiled | xargs rm -f 
		find ./ -name \*.tmp | xargs rm -f
		find ./ -name \*.dbg | xargs rm -f  
	;;
#	'win')
#		echo "Building for Win..."
#		fpc ...
#	;;
#	'linux')
#		echo "Building for Linux..."
#		fpc ...
#	;;
	*)
		echo "Também pode usar como: $0 {clean|win|linux}"
esac
