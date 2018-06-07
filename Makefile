test:
	ocamlbuild -r -pkgs oUnit,str,unix,ANSITerminal,graphics,yojson test.byte && ./test.byte

play:
	ocamlbuild -r -pkgs oUnit,str,unix,ANSITerminal,graphics,yojson main.byte && ./main.byte

clean:
	ocamlbuild -clean

gui:
	ocamlbuild -r -pkgs oUnit,str,unix,ANSITerminal,graphics,yojson guitester.byte && ./guitester.byte