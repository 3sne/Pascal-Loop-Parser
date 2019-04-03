parser: lalr_parser.tab.c lex.yy.c
	gcc lalr_parser.tab.c lex.yy.c -o parser

lalr_parser.tab.c :lalr_parser.y
	bison -d lalr_parser.y

lex.yy.c: tokenizer.l lalr_parser.tab.h
	flex tokenizer.l

clean:
	rm lalr_parser.tab.h
	rm lalr_parser.tab.c
	rm lex.yy.c