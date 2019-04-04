%{

    #include <stdio.h>
    #include<string.h>
    #include "symtable.h"
    #include "lalr_parser.tab.h"
    
    token* TokenList[1024];
    symTableEntry* st[10];
    int tInd = 0, scnt = -1;
    int gl = 1, gc = 1;
    int localizer = 0;
    int rownum = 1;
    int colnum = 1;
    char s[1000]={'\0'};
        void yyerror(char *msg);
        int lastupdate = 0;

    //Prototypes
    token* newToken(char lex[], char tn[], int i, int l, int c);
    symTableEntry* newSte(char l[]);
    void printTokenList();
    void putTokenListInFile();
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

{A}{N}{D}                                           { TokenList[tInd++] = newToken(yytext, "AND", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(AND);}
{A}{R}{R}{A}{Y}                                     { TokenList[tInd++] = newToken(yytext, "ARRAY", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(ARRAY);}
{C}{A}{S}{E}                                        { TokenList[tInd++] = newToken(yytext, "CASE", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;;return(CASE);}
{D}{I}{V}                                           { TokenList[tInd++] = newToken(yytext, "DIV", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(DIV);}
{D}{O}                                              { TokenList[tInd++] = newToken(yytext, "DO", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(DO);}
{D}{O}{W}{N}{T}{O}                                  { TokenList[tInd++] = newToken(yytext, "DOWNTO", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(DOWNTO);}
{E}{L}{S}{E}                                        { TokenList[tInd++] = newToken(yytext, "ELSE", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(ELSE);}
{E}{N}{D}                                           { TokenList[tInd++] = newToken(yytext, "END", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(END);}
{F}{O}{R}                                           { TokenList[tInd++] = newToken(yytext, "FOR", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(FOR);}
{G}{O}{T}{O}                                        { TokenList[tInd++] = newToken(yytext, "GOTO", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(GOTO);}
{I}{F}                                              { TokenList[tInd++] = newToken(yytext, "IF", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(IF);}
{O}{F}                                              { TokenList[tInd++] = newToken(yytext, "OF", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(OF);}
{I}{N}                                              { TokenList[tInd++] = newToken(yytext, "IN", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(IN);}
{M}{O}{D}                                           { TokenList[tInd++] = newToken(yytext, "MOD", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(MOD);}
{N}{I}{L}                                           { TokenList[tInd++] = newToken(yytext, "NIL", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(NIL);}
{N}{O}{T}                                           { TokenList[tInd++] = newToken(yytext, "NOT", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(NOT);}
{F}{I}{L}{E}                                        { TokenList[tInd++] = newToken(yytext, "PFILE", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(PFILE);}
{O}{R}                                              { TokenList[tInd++] = newToken(yytext, "OR", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(OR);}
{S}{E}{T}                                           { TokenList[tInd++] = newToken(yytext, "SET", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3;return(SET);}
{R}{E}{C}{O}{R}{D}                                  { TokenList[tInd++] = newToken(yytext, "RECORD", -1, gl, gc); lastupdate = strlen(yytext); strcat(s, yytext);colnum+=6; return(RECORD);}
{B}{E}{G}{I}{N}                                     { TokenList[tInd++] = newToken(yytext, "PBEGIN", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(PBEGIN);}
{P}{A}{C}{K}{E}{D}                                  { TokenList[tInd++] = newToken(yytext, "PACKED", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(PACKED);}
{R}{E}{P}{E}{A}{T}                                  { TokenList[tInd++] = newToken(yytext, "REPEAT", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=6;return(REPEAT);}
{T}{H}{E}{N}                                        { TokenList[tInd++] = newToken(yytext, "THEN", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=4;return(THEN);}
{T}{O}                                              { TokenList[tInd++] = newToken(yytext, "TO", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=2;return(TO);}
{U}{N}{T}{I}{L}                                     { TokenList[tInd++] = newToken(yytext, "UNTIL", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(UNTIL);}
{W}{H}{I}{L}{E}                                     { TokenList[tInd++] = newToken(yytext, "WHILE", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=5;return(WHILE);}
{V}{A}{R}                                           { TokenList[tInd++] = newToken(yytext, "VAR", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=3; return VAR;}
[a-zA-Z_]([a-zA-Z0-9_])*                            { TokenList[tInd++] = newToken(yytext, "IDENT", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(IDENT);}
'({NQUOTE}|'')+'                                    { TokenList[tInd++] = newToken(yytext, "STRING_LITERAL", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(STRING_LITERAL);}
":="                                                { TokenList[tInd++] = newToken(yytext, "ASSIGN", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(ASSIGN);}
":"                                                 { TokenList[tInd++] = newToken(yytext, "COLON", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(COLON);}
","                                                 { TokenList[tInd++] = newToken(yytext, "COMMA", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(COMMA);}
".."                                                { TokenList[tInd++] = newToken(yytext, "DOTDOT", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(DOTDOT);}
"="                                                 { TokenList[tInd++] = newToken(yytext, "EQUAL", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(EQUAL);}
">="                                                { TokenList[tInd++] = newToken(yytext, "GE", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(GE);}
">"                                                 { TokenList[tInd++] = newToken(yytext, "GT", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(GT);}
"["                                                 { TokenList[tInd++] = newToken(yytext, "LBRACK", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LBRACK);}
"<="                                                { TokenList[tInd++] = newToken(yytext, "LE", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LE);}
"("                                                 { TokenList[tInd++] = newToken(yytext, "LPAREN", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LPAREN);}
"<"                                                 { TokenList[tInd++] = newToken(yytext, "LT", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(LT);}
"-"                                                 { TokenList[tInd++] = newToken(yytext, "MINUS", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(MINUS);}
"<>"                                                { TokenList[tInd++] = newToken(yytext, "NOT_EQUAL", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(NOT_EQUAL);}
"+"                                                 { TokenList[tInd++] = newToken(yytext, "PLUS", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(PLUS);}
"]"                                                 { TokenList[tInd++] = newToken(yytext, "RBRACK", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(RBRACK);}
[0-9]+                                              { TokenList[tInd++] = newToken(yytext, "NUM_INT", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(NUM_INT);}
")"                                                 { TokenList[tInd++] = newToken(yytext, "RPAREN", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(RPAREN);}
";"                                                 { TokenList[tInd++] = newToken(yytext, "SEMI", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(SEMI);}
"/"                                                 { TokenList[tInd++] = newToken(yytext, "SLASH", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(SLASH);}
"*"                                                 { TokenList[tInd++] = newToken(yytext, "STAR", -1, gl, gc); lastupdate = strlen(yytext);strcat(s, yytext);colnum+=strlen(yytext);return(STAR);}
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
void yyerror(char *msg) {
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

symTableEntry* newSte(char l[]) {
    symTableEntry* temp = (symTableEntry*)malloc(sizeof(symTableEntry));
    temp->id = scnt + 1;
    temp->next = NULL;
    strcpy(temp->varName, l);
    return temp;
}

void generateSymbolTable() {
    for (int i = 0; i < tInd; i++) {
        token* focusOn = TokenList[i];
        if (strcpy(focusOn->tName, "IDENT")) {
            printf("[TEST] %d : %s", i, focusOn->tName);
        }

    }
}

void printTokenList() {
    printf("\n");
    printf("      LEX  |   TOKEN   | INDEX | LN | COL  \n");
    printf("-----------------------------------------------\n");
    for ( int i = 0; i < tInd; i++ ) {
        printf("<%10s %10s   %6d %3d %3d >\n", TokenList[i]->lName, TokenList[i]->tName, TokenList[i]->index, TokenList[i]->line, TokenList[i]->column);
    }
}

void printSymTable() {
    printf("\n");
    printf("---------------------------------------------------------------------------------------------------------------------\n");
    printf("|                                                    SYMBOL TABLE                                                    \n");
    printf("---------------------------------------------------------------------------------------------------------------------\n");
    printf("|    VAR    |    TYPE   | INDEX |       VAR_SIZE       | SCOPE| ARG_CNT| RET TYPE |         ARGUMENT LIST            \n");
    printf("------------+-----------+-------+----------------------+------+--------+----------+----------------------------------\n");
    symTableEntry* temp;
    for(int i = 0; i < 10; i++) {
        for(temp = st[i]; temp; temp = temp->next) {
            printf("|%10s | %9s | %5d |  %18s  |  %3c | %6d | %8s |", temp->varName, temp->varType, temp->id, temp->varMemSize, temp->scope, temp->numArgs, temp->returnType);
            if ( temp->numArgs > 0) {
                for ( int i = 0; i < temp->numArgs; i++) {
                    printf(" %8s ", temp->args[i]->lName);
                }
            }
            printf("\n");
        }
    }
    printf("=====================================================================================================================\n");
}

void printSymTableInHashTableFormat() {
    symTableEntry* temp;
    printf("\n");
    printf("SYMBOL TABLE AS HASH TABLE\n");
    printf("--------------------------\n");
    for(int i = 0; i < 10; i++) {
        printf("[%d] -> ", i);
        for(temp = st[i]; temp; temp = temp->next) {
            printf(" {%10s, %4d} ->", temp->varName, temp->id);
        }
        printf(" NULL\n");
    }
}

void putTokenListInFile() {
    FILE *o = fopen("tokens.txt", "w");
    for ( int i = 0; i < tInd; i++ ) {
        fprintf(o, "<%70s %10s %10d %10d %10d >\n", TokenList[i]->lName, TokenList[i]->tName, TokenList[i]->index, TokenList[i]->line, TokenList[i]->column);
    }
    fclose(o);
}

