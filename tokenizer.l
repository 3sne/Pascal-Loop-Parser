%{
    #include <stdio.h>
    #include "lalr_parser.tab.h"
    int line_no = 1;
    void commenteof();
%}

A [aA]
B [bB]
C [cC]
D [dD]
E [eE]
F [fF]
G [gG]
H [hH]
I [iI]
J [jJ]
K [kK]
L [lL]
M [mM]
N [nN]
O [oO]
P [pP]
Q [qQ]
R [rR]
S [sS]
T [tT]
U [uU]
V [vV]
W [wW]
X [xX]
Y [yY]
Z [zZ]
NQUOTE [^']

%%

{A}{N}{D}                           { printf("FOUND >> %s\n", yytext); return(AND); }
{D}{I}{V}                           { printf("FOUND >> %s\n", yytext); return(DIV); }
{D}{O}                          { printf("FOUND >> %s\n", yytext); return(DO); }
{D}{O}{W}{N}{T}{O}                          { printf("FOUND >> %s\n", yytext); return(DOWNTO); }
{E}{L}{S}{E}                        { printf("FOUND >> %s\n", yytext); return(ELSE); }
{E}{N}{D}                           { printf("FOUND >> %s\n", yytext); return(END); }
{F}{O}{R}                           { printf("FOUND >> %s\n", yytext); return(FOR); }
{G}{O}{T}{O}                        { printf("FOUND >> %s\n", yytext); return(GOTO); }
{I}{F}                          { printf("FOUND >> %s\n", yytext); return(IF); }
{I}{N}                          { printf("FOUND >> %s\n", yytext); return(IN); }
{M}{O}{D}                           { printf("FOUND >> %s\n", yytext); return(MOD); }
{N}{I}{L}                           { printf("FOUND >> %s\n", yytext); return(NIL); }
{N}{O}{T}                           { printf("FOUND >> %s\n", yytext); return(NOT); }
{O}{R}                          { printf("FOUND >> %s\n", yytext); return(OR); }
{B}{E}{G}{I}{N}                         { printf("FOUND >> %s\n", yytext); return(PBEGIN); }
{R}{E}{P}{E}{A}{T}                          { printf("FOUND >> %s\n", yytext); return(REPEAT); }
{T}{H}{E}{N}                        { printf("FOUND >> %s\n", yytext); return(THEN); }
{T}{O}                          { printf("FOUND >> %s\n", yytext); return(TO); }
{U}{N}{T}{I}{L}                         { printf("FOUND >> %s\n", yytext); return(UNTIL); }
{W}{H}{I}{L}{E}                         { printf("FOUND >> %s\n", yytext); return(WHILE); }
[a-zA-Z]                                { printf("FOUND >> %s\n", yytext); return(CHR); }
[a-zA-Z_]([a-zA-Z0-9_])+                        { printf("FOUND >> %s\n", yytext); return(IDENT); }
'({NQUOTE}|'')+'                        { printf("FOUND >> %s\n", yytext); return(STRING_LITERAL); }
":"                         { printf("FOUND >> %s\n", yytext); return(COLON); }
":="                            { printf("FOUND >> %s\n", yytext); return(ASSIGN); }
","                         { printf("FOUND >> %s\n", yytext); return(COMMA); }
".."                            { printf("FOUND >> %s\n", yytext); return(DOTDOT); }
"="                         { printf("FOUND >> %s\n", yytext); return(EQUAL); }
">="                            { printf("FOUND >> %s\n", yytext); return(GE); }
">"                         { printf("FOUND >> %s\n", yytext); return(GT); }
"["                         { printf("FOUND >> %s\n", yytext); return(LBRACK); }
"<="                            { printf("FOUND >> %s\n", yytext); return(LE); }
"("                         { printf("FOUND >> %s\n", yytext); return(LPAREN); }
"<"                         { printf("FOUND >> %s\n", yytext); return(LT); }
"-"                         { printf("FOUND >> %s\n", yytext); return(MINUS); }
"<>"                            { printf("FOUND >> %s\n", yytext); return(NOT_EQUAL); }
"+"                         { printf("FOUND >> %s\n", yytext); return(PLUS); }
"]"                         { printf("FOUND >> %s\n", yytext); return(RBRACK); }
[0-9]+"."[0-9]+                         { printf("FOUND >> %s\n", yytext); return(NUM_INT); }
")"                         { printf("FOUND >> %s\n", yytext); return(RPAREN); }
";"                         { printf("FOUND >> %s\n", yytext); return(SEMI); }
"/"                         { printf("FOUND >> %s\n", yytext); return(SLASH); }
"*"                         { printf("FOUND >> %s\n", yytext); return(STAR); }
"(*"|"{"    {   register int c;
                while ((c = input())){
                if (c == '}')
                    break;
                else if (c == '*') {
                    if ((c = input()) == ')')
                        break;
                    else
                        unput (c);
                }
                else if (c == '\n')
                    line_no++;
                else if (c == 0)
                    commenteof();
                }
            }

[ \t\f]    ;

\n    line_no++;

.   { fprintf (stderr,"'%c' (0%o): illegal charcter at line %d\n", yytext[0], yytext[0], line_no);}

%%

void commenteof() {
    fprintf (stderr, "unexpected EOF inside comment at line %d\n",
    line_no);
    exit (EXIT_FAILURE);
}

int yywrap() {
    return (1);
}


