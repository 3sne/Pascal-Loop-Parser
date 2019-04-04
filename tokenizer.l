%{

    #include <stdio.h>
    #include<string.h>
    #include "lalr_parser.tab.h"
    #define ANSI_COLOR_RESET   "\x1b[0m"

    #define ANSI_COLOR_CYAN    "\x1b[36m"
    
    #define SAVE_TOKEN yylval.string = (char*)malloc(sizeof(char)*(yyleng+1));\
                   strcpy(yylval.string,yytext);
    #define TOKEN(t) (yylval.token = t);
    typedef struct token {
    char tName[256];
    char lName[256];
    int index;
    int line;
    int column;
} token;

    token* TokenList[1000];
    int tInd = 0;

 

    
    
    int gl = 1, gc = 1;
    int localizer = 0;
    int rownum = 1;
    int colnum = 1;
    char s[1000]={'\0'};
    int lastupdate = 0;
    //Prototypes
    token* newToken(char lex[], char tn[], int i, int l, int c);
   
   int yywrap(); 
   void putTokenListInFile();
   void printTokenList();   

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


{A}{N}{D}                                           { TokenList[tInd++] = newToken(yytext, "AND", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(AND);}
{T}{Y}{P}{E}                                        { TokenList[tInd++] = newToken(yytext, "TYPE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(TYPE);}
{E}{X}{T}{E}{R}{N}{A}{L}                            { TokenList[tInd++] = newToken(yytext, "EXTERNAL", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(EXTERNAL);}
{A}{R}{R}{A}{Y}                                     { TokenList[tInd++] = newToken(yytext, "ARRAY", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(ARRAY);}
{C}{A}{S}{E}                                        { TokenList[tInd++] = newToken(yytext, "CASE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);;return TOKEN(CASE);}
{C}{O}{N}{S}{T}                                     { TokenList[tInd++] = newToken(yytext, "CONST", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(CONST);}
{D}{I}{V}                                           { TokenList[tInd++] = newToken(yytext, "DIV", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(DIV);}
{D}{O}                                              { TokenList[tInd++] = newToken(yytext, "DO", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(DO);}
{D}{O}{W}{N}{T}{O}                                  { TokenList[tInd++] = newToken(yytext, "DOWNTO", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(DOWNTO);}
{E}{L}{S}{E}                                        { TokenList[tInd++] = newToken(yytext, "ELSE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(ELSE);}
{E}{N}{D}                                           { TokenList[tInd++] = newToken(yytext, "END", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(END);}
{F}{U}{N}{C}{T}{I}{O}{N}                            { TokenList[tInd++] = newToken(yytext, "FUNCTION", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(FUNCTION);}
{F}{O}{R}{W}{A}{R}{D}                               { TokenList[tInd++] = newToken(yytext, "FORWARD", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(FORWARD);}
{F}{O}{R}                                           { TokenList[tInd++] = newToken(yytext, "FOR", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(FOR);}
{G}{O}{T}{O}                                        { TokenList[tInd++] = newToken(yytext, "GOTO", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(GOTO);}
{I}{F}                                              { TokenList[tInd++] = newToken(yytext, "IF", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(IF);}
{O}{F}                                              { TokenList[tInd++] = newToken(yytext, "OF", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(OF);}
{I}{N}                                              { TokenList[tInd++] = newToken(yytext, "IN", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(IN);}
{I}{N}{T}{E}{G}{E}{R}                               { SAVE_TOKEN;TokenList[tInd++] = newToken(yytext, "INTEGER", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(INTEGER);}
{R}{E}{A}{L}                                        { SAVE_TOKEN;TokenList[tInd++] = newToken(yytext, "REAL", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(REAL);}
{B}{O}{O}{L}{E}{A}{N}                               { SAVE_TOKEN;TokenList[tInd++] = newToken(yytext, "BOOLEAN", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(BOOLEAN);}
{C}{H}{A}{R}                                        { SAVE_TOKEN;TokenList[tInd++] = newToken(yytext, "CHAR", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(CHAR);}
{M}{O}{D}                                           { TokenList[tInd++] = newToken(yytext, "MOD", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(MOD);}
{N}{I}{L}                                           { TokenList[tInd++] = newToken(yytext, "NIL", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(NIL);}
{N}{O}{T}                                           { TokenList[tInd++] = newToken(yytext, "NOT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(NOT);}
{F}{I}{L}{E}                                        { TokenList[tInd++] = newToken(yytext, "PFILE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(PFILE);}
{O}{R}                                              { TokenList[tInd++] = newToken(yytext, "OR", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(OR);}
{S}{E}{T}                                           { TokenList[tInd++] = newToken(yytext, "SET", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(SET);}
{R}{E}{C}{O}{R}{D}                                  { TokenList[tInd++] = newToken(yytext, "RECORD", -1, rownum, colnum); lastupdate = strlen(yytext); strcat(s, yytext);colnum+=strlen(yytext); return TOKEN(RECORD);}
{B}{E}{G}{I}{N}                                     { TokenList[tInd++] = newToken(yytext, "PBEGIN", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(PBEGIN);}
{P}{A}{C}{K}{E}{D}                                  { TokenList[tInd++] = newToken(yytext, "PACKED", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(PACKED);}
{R}{E}{P}{E}{A}{T}                                  { TokenList[tInd++] = newToken(yytext, "REPEAT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(REPEAT);}
{T}{H}{E}{N}                                        { TokenList[tInd++] = newToken(yytext, "THEN", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(THEN);}
{P}{R}{O}{C}{E}{D}{U}{R}{E}                         { TokenList[tInd++] = newToken(yytext, "PROCEDURE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(PROCEDURE);}
{T}{O}                                              { TokenList[tInd++] = newToken(yytext, "TO", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(TO);}
{U}{N}{T}{I}{L}                                     { TokenList[tInd++] = newToken(yytext, "UNTIL", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(UNTIL);}
{W}{H}{I}{L}{E}                                     { TokenList[tInd++] = newToken(yytext, "WHILE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(WHILE);}
{V}{A}{R}                                           { TokenList[tInd++] = newToken(yytext, "VAR", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext); return TOKEN(VAR);}
[a-zA-Z_]([a-zA-Z0-9_])*                            { SAVE_TOKEN;TokenList[tInd++] = newToken(yytext, "IDENT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return (IDENT);}
'({NQUOTE}|'')+'                                    { TokenList[tInd++] = newToken(yytext, "STRING_LITERAL", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(STRING_LITERAL);}
":="                                                { TokenList[tInd++] = newToken(yytext, "ASSIGN", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(ASSIGN);}
":"                                                 { TokenList[tInd++] = newToken(yytext, "COLON", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(COLON);}
","                                                 { TokenList[tInd++] = newToken(yytext, "COMMA", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(COMMA);}
".."                                                { TokenList[tInd++] = newToken(yytext, "DOTDOT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(DOTDOT);}
"."                                                 { TokenList[tInd++] = newToken(yytext, "DOT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(DOT);}
"="                                                 { TokenList[tInd++] = newToken(yytext, "EQUAL", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(EQUAL);}
">="                                                { TokenList[tInd++] = newToken(yytext, "GE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(GE);}
">"                                                 { TokenList[tInd++] = newToken(yytext, "GT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(GT);}
"["                                                 { TokenList[tInd++] = newToken(yytext, "LBRACK", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(LBRACK);}
"<="                                                { TokenList[tInd++] = newToken(yytext, "LE", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(LE);}
"("                                                 { TokenList[tInd++] = newToken(yytext, "LPAREN", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(LPAREN);}
"<"                                                 { TokenList[tInd++] = newToken(yytext, "LT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(LT);}
"-"                                                 { TokenList[tInd++] = newToken(yytext, "MINUS", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(MINUS);}
"<>"                                                { TokenList[tInd++] = newToken(yytext, "NOT_EQUAL", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(NOT_EQUAL);}
"+"                                                 { TokenList[tInd++] = newToken(yytext, "PLUS", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(PLUS);}
"]"                                                 { TokenList[tInd++] = newToken(yytext, "RBRACK", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(RBRACK);}
[0-9]+                                              { SAVE_TOKEN;TokenList[tInd++] = newToken(yytext, "NUM_INT", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return NUM_INT;}
")"                                                 { TokenList[tInd++] = newToken(yytext, "RPAREN", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(RPAREN);}
";"                                                 { TokenList[tInd++] = newToken(yytext, "SEMI", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(SEMI);}
"/"                                                 { TokenList[tInd++] = newToken(yytext, "SLASH", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(SLASH);}
"**"                                                { TokenList[tInd++] = newToken(yytext, "STARSTAR", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(STARSTAR);}
"*"                                                 { TokenList[tInd++] = newToken(yytext, "STAR", -1, rownum, colnum); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return TOKEN(STAR);}
"\n"                                                {   
                                                        strcat(s,"\n");
                                                        if(rownum>2) {
                                                            int nlpos = 0;
                                                            for(int i=0;i<strlen(s);i++) {
                                                                if(s[i]=='\n') {
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
" "                                                 { lastupdate = 1;colnum++;strcat(s, yytext);}




%%

//Flex Essentials

void lexerror(char msg[1000])
{
    colnum-=lastupdate;
    printf("\nERROR\n");
    printf("---------------------------\n");
    printf("ROW: %d COL: %d\n", rownum, colnum);
    printf("%s ...\n", s);
    for(int i=0;i<colnum-1;i++) {
        printf(" ");
    }    
    printf("^\n--------------------------\n");

    printf("%s\n--------------------------\n", msg);
}

int yywrap() {
    printTokenList();
    
    printSymTable();
    putTokenListInFile();
    return (1);
}

//Function bodies
token* newToken(char lex[], char tn[], int i, int l, int c) {
       

    token* temp = (token*)malloc(sizeof(token));
    strcpy(temp->tName, tn);
    strcpy(temp->lName, lex);
    temp->index = i;
    temp->line = l;
    temp->column = c;
    return temp;
}



void printTokenList() {
    printf("\n");
    printf(ANSI_COLOR_CYAN"      LEX  |   TOKEN   | INDEX | LN | COL  \n");
    printf("-----------------------------------------------\n");
    for ( int i = 0; i < tInd; i++ ) {
        printf("<%10s %10s   %6d %3d %3d >\n", TokenList[i]->lName, TokenList[i]->tName, TokenList[i]->index, TokenList[i]->line, TokenList[i]->column);
    }
}

void putTokenListInFile() {
    FILE *o = fopen("tokens.txt", "w");
    for ( int i = 0; i < tInd; i++ ) {
        fprintf(o, "<%70s %10s %10d %10d %10d >\n", TokenList[i]->lName, TokenList[i]->tName, TokenList[i]->index, TokenList[i]->line, TokenList[i]->column);
    }
    printf(ANSI_COLOR_RESET "\n");
    fclose(o);
}



