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

{A}{N}{D}   return(AND);
{D}{I}{V}   return(DIV);
{D}{O}    return(DO);
{D}{O}{W}{N}{T}{O}  return(DOWNTO);
{E}{L}{S}{E}   return(ELSE);
{E}{N}{D}   return(END);
{F}{O}{R}   return(FOR);
{G}{O}{T}{O}   return(GOTO);
{I}{F}    return(IF);
{I}{N}    return(IN);
{M}{O}{D}   return(MOD);
{N}{I}{L}   return(NIL);
{N}{O}{T}   return(NOT);
{O}{R}    return(OR);
{B}{E}{G}{I}{N}   return(BEGIN);
{R}{E}{P}{E}{A}{T}  return(REPEAT);
{T}{H}{E}{N}   return(THEN);
{T}{O}    return(TO);
{U}{N}{T}{I}{L}   return(UNTIL);
{W}{H}{I}{L}{E}   return(WHILE);
[a-zA-Z_]([a-zA-Z0-9_])+  return(IDENT);
":="    return(ASSIGN);
":"    return(COLON);
","    return(COMMA);
"="    return(EQUAL);
">="    return(GE);
">"    return(GT);
"["    return(LBRACK);
"<="    return(LE);
"("    return(LPAREN);
"<"    return(LT);
"-"    return(MINUS);
"<>"    return(NOT_EQUAL);
"+"    return(PLUS);
"]"    return(RBRACK);
[0-9]+"."[0-9]+   return(NUM_INT);
")"    return(RPAREN);
";"    return(SEMI);
"/"    return(SLASH);
"*"    return(STAR);
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

.   { fprintf (stderr,"'%c' (0%o): illegal charcter at line %d\n", yytext[0], yytext[0], line_no); }

%%

void commenteof() {
    fprintf (stderr, "unexpected EOF inside comment at line %d\n",
    line_no);
    exit (EXIT_FAILURE);
}

int yywrap() {
    return (1);
}


