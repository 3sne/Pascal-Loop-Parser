%{

    #include <stdio.h>
    #include<string.h>
    #include "lalr_parser.tab.h"
    int rownum = 1;
    int colnum = 1;
    char s[1000]={'\0'};
        void yyerror(char *msg);
        int lastupdate = 0;

%}

%option yylineno
%option bison-locations
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

{A}{N}{D}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(AND);}
{A}{R}{R}{A}{Y}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(ARRAY);}
{C}{O}{N}{S}{T}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(CONST);}
{C}{A}{S}{E}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;;return(CASE);}
{D}{I}{V}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(DIV);}
{D}{O}    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(DO);}
{D}{O}{W}{N}{T}{O}  {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(DOWNTO);}
{E}{L}{S}{E}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(ELSE);}
{E}{N}{D}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(END);}
{F}{O}{R}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(FOR);}
{F}{U}{N}{C}{T}{I}{O}{N}  {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(FOR);}
{F}{O}{R}{W}{A}{R}{D} {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(FOR);}
{G}{O}{T}{O}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(GOTO);}
{I}{F}    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(IF);}
{O}{F}    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(OF);}
{I}{N}    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(IN);}
{M}{O}{D}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(MOD);}
{N}{I}{L}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(NIL);}
{N}{O}{T}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(NOT);}
{F}{I}{L}{E}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(PFILE);}
{O}{R}    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(OR);}
{S}{E}{T}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(SET);}
{R}{E}{C}{O}{R}{D}  {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6; printf("%s", yytext);return(RECORD);}
{B}{E}{G}{I}{N}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(PBEGIN);}
{P}{A}{C}{K}{E}{D}  {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(PACKED);}
{L}{A}{B}{E}{L}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(LABEL);}
{T}{Y}{P}{E}       {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(TYPE);}
{R}{E}{P}{E}{A}{T}  {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(REPEAT);}
{T}{H}{E}{N}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(THEN);}
{P}{R}{O}{C}{E}{D}{U}{R}{E} {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(THEN);}
{T}{O}    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(TO);}
{U}{N}{T}{I}{L}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(UNTIL);}
{W}{H}{I}{L}{E}   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(WHILE);}
{E}{X}{T}{E}{R}{N}{A}{L} {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(WHILE);}
{V}{A}{R} {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3; return VAR;}
[a-zA-Z_]([a-zA-Z0-9_])*  {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(IDENT);}
'({NQUOTE}|'')+'  {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(STRING_LITERAL);}
":="    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(ASSIGN);}
":"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(COLON);}
","    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(COMMA);}
".."    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(DOTDOT);}
"="    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(EQUAL);}
">="    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(GE);}
">"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(GT);}
"["    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LBRACK);}
"<="    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LE);}
"("    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LPAREN);}
"<"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LT);}
"-"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(MINUS);}
"<>"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(NOT_EQUAL);}
"+"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(PLUS);}
"]"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(RBRACK);}
[0-9]+   {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(NUM_INT);}
")"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(RPAREN);}
";"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(SEMI);}
"/"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(SLASH);}

"**"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(STARSTAR);}
"*"    {lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return STAR;}
"\n"   {strcat(s,"\n");
if(rownum>2) 
{
    int nlpos = 0;
for(int i=0;i<strlen(s);i++)
{
    if(s[i]=='\n')
    {
        nlpos=i;
        break;
    }
}
char temp[1000];
strcpy(temp, &s[nlpos+1]);
memset(s, 1000, '\0');
strcpy(s, temp);
}
rownum++;
colnum=1;

}
" "    {lastupdate = 1;colnum++;strcat(s, yytext);}


%%

void yyerror(char *msg)
{
    colnum-=lastupdate;
    printf("\nERROR\n");
    printf("---------------------------\n");
    printf("ROW: %d COL: %d\n", rownum, colnum);
    printf("%s ...\n", &s[1]);
    for(int i=0;i<colnum-1;i++)
    {
        printf(" ");
    }    
    printf("^\n--------------------------\n");

    printf("%s\n--------------------------\n", msg);

}
int yywrap() {
    return (1);
}


