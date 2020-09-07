all:
	@ocamlc -c func.ml
	@ocamllex lexer.mll
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c assignment4.ml
	@ocamlc -o assignment4 str.cma func.cmo lexer.cmo parser.cmo assignment4.cmo
	@./assignment4 sheet.csv 9 25 input.txt
	@rm lexer.ml
	@rm parser.mli
	@rm parser.ml
	@rm *.cmi
	@rm *.cmo
	@rm assignment4
