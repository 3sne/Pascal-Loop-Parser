
%{
    #include <stdio.h>
    #include <stdlib.h>
    extern FILE * yyin;
    int yyerror(char *msg);
    int yylex();
    // %token LITERAL SEMICOLON COMMA COLON ASSIGN LT GT LTE GTE EQUAL NOTEQUAL ADD MULTIPLY SUBTRACT DIVIDE COMP_AND COMP_DAND VOID COMP_OR COMP_DOR LP RP LC RC LSB RSB CHAR INT UINT SIGNED UNSIGNED SHORT LONG FLOAT DOUBLE REGISTER CONST IF ELSE FOR WHILE DO SWITCH CASE DEFAULT BREAK CONTINUE ENUM TYPEDEF EXTERN RETURN UNION GOTO ID NUM MOD;
%}

%token AND DIV DO DOWNTO ELSE END FOR GOTO IF IN MOD NIL NOT OR PBEGIN REPEAT THEN TO UNTIL WHILE IDENT ASSIGN COLON COMMA EQUAL GE GT LBRACK LE LPAREN LT MINUS NOT_EQUAL PLUS RBRACK NUM_INT RPAREN SEMI SLASH STAR CHR DOTDOT STRING_LITERAL;

%%
augment : repetetiveStatement
   ;
repetetiveStatement
   : whileStatement
   | repeatStatement
   | forStatement
   ;

whileStatement
   : WHILE expression DO statement
   ;

repeatStatement
   : REPEAT statements UNTIL expression
   ;

forStatement
   : FOR identifier ASSIGN forList DO statement
   ;

forList
   : initialValue TO finalValue | initialValue DOWNTO finalValue
   ;

initialValue
   : expression
   ;

finalValue
   : expression
   ;

statement
   : label COLON unlabelledStatement
   | unlabelledStatement
   ;

unlabelledStatement
   : simpleStatement
   | structuredStatement
   ;

simpleStatement
   : assignmentStatement
   | gotoStatement
   | emptyStatement
   ;

assignmentStatement
   : variable ASSIGN expression
   ;

variable
   : identifier
   ;




label
   : unsignedInteger
   ;


unsignedInteger
   : NUM_INT
   ;
    
structuredStatement
   : compoundStatement
   | conditionalStatement
   | repetetiveStatement
   ;


gotoStatement
   : GOTO label
   ;


emptyStatement
   :


identifier
   : IDENT
   ;

expression
   : simpleExpression
   | simpleExpression relationaloperator expression

   ;

relationaloperator
   : EQUAL
   | NOT_EQUAL
   | LT
   | LE
   | GE
   | GT
   | IN
   ;

simpleExpression
   : term
   | term additiveoperator simpleExpression
   ;


compoundStatement
   : PBEGIN statements END
   ;

statements
   : statement SEMI statements |
   ;

conditionalStatement
   : ifStatement
  
   ;

ifStatement
   : IF expression THEN statement
   | IF expression THEN statement ELSE statement
   ;



repetetiveStatement
   : whileStatement
   | repeatStatement
   | forStatement
   ;


additiveoperator
   : PLUS
   | MINUS
   | OR
   ;

term
   : signedFactor
   | signedFactor multiplicativeoperator term
   ;

multiplicativeoperator
   : STAR
   | SLASH
   | DIV
   | MOD
   | AND
   ;

signedFactor
   : factor
   | PLUS factor
   | MINUS factor
   ;

factor
   : variable
   | LPAREN expression RPAREN
   | functionDesignator
   | unsignedConstant
   | set
   | NOT factor
   ;

functionDesignator : identifier LPAREN parameterList RPAREN
   ;

parameterList : actualParameter ourBoi
   ;

ourBoi : COMMA actualParameter ourBoi
   |
   ;

actualParameter : expression paramCool
   ;

paramCool
   :parameterwidth paramCool
   |
   ;

parameterwidth
   : COLON expression
   ;

unsignedConstant
   : unsignedInteger
   | constantChr
   | string
   | NIL
   ;

constantChr
   : CHR LPAREN unsignedInteger RPAREN
   ;

set
   : LBRACK elementList RBRACK
   ;


elementList
   : element elemCool
   |
   ;

elemCool
   : COMMA element elemCool
   |
   ;

element
   : expression
   | expression DOTDOT expression
   ;

string
   : STRING_LITERAL
   ;
%%


int yyerror(char *msg) {
    printf("INVALID EXPR >> %s\n", msg);
}

void main() {
    yyin = fopen("input.txt", "r");
    (yyparse()) ? printf("VERDICT: REJECTED by grammar G\n") : printf("VERDICT: ACCEPTED by grammar G\n");
    fclose(yyin);
}
