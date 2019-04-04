
%{
   #include <stdio.h>
   #include <stdlib.h>

   #define YYERROR_VERBOSE 1
   extern FILE * yyin;

   int yylex();

    // %token LITERAL SEMICOLON COMMA COLON ASSIGN LT GT LTE GTE EQUAL NOTEQUAL ADD MULTIPLY SUBTRACT DIVIDE COMP_AND COMP_DAND VOID COMP_OR COMP_DOR LP RP LC RC LSB RSB CHAR INT UINT SIGNED UNSIGNED SHORT LONG FLOAT DOUBLE REGISTER CONST IF ELSE FOR WHILE DO SWITCH CASE DEFAULT BREAK CONTINUE ENUM TYPEDEF EXTERN RETURN UNION GOTO ID NUM MOD;
%}


%token AND "and" DIV "div" DO "do" DOWNTO "downto" ELSE "else" END "end" FOR "for" GOTO "goto" IF "if" IN "in" MOD "mod" NIL "nil" NOT "not" OR "or" PBEGIN "begin" REPEAT "repeat" 
%token THEN "then" TO "to" UNTIL "until" WHILE "while" IDENT "identifier" ASSIGN ":=" COLON ":" COMMA "," EQUAL "=" GE ">=" GT ">" LBRACK "[" LE "<=" LPAREN "("
%token LT "<" MINUS "minus" NOT_EQUAL "<>" PLUS "+" RBRACK "]" NUM_INT "number" RPAREN ")" SEMI "semicolon" SLASH "/" STAR "*"  DOTDOT ".." CHR "character"
%token PFILE "file" UPARROW "^" OF "of" STRING_LITERAL "literal" VAR "var" PACKED "packed" RECORD "record" CASE "case" SET "set" ARRAY "array";
%error-verbose
%locations
%define parse.lac full
%define api.pure true



%%
augment : variable_declaration_part repetetiveStatement
   ;

variable_declaration_part : VAR variable_declaration_list SEMI
      |
      ;

variable_declaration_list :
   variable_declaration_list SEMI variable_declaration
      | variable_declaration
      ;

variable_declaration : identifier_list COLON type_denoter
      ;


identifier_list : identifier_list COMMA identifier
      | identifier
      ;

type_denoter : identifier
      | new_type
      ;

new_type : new_ordinal_type
      | new_structured_type
      
      ;

new_ordinal_type : enumerated_type
      | subrange_type
      ;

enumerated_type : LPAREN identifier_list RPAREN
      ;

subrange_type : unsignedConstant DOTDOT unsignedConstant
      ;

new_structured_type : structured_type
      | PACKED structured_type
      ;

structured_type : array_type
      | record_type
      | set_type
      | file_type
      ;

array_type : ARRAY LPAREN index_list RPAREN OF component_type
      ;

index_list : index_list COMMA index_type
      | index_type
      ;

index_type : ordinal_type      ;

ordinal_type : new_ordinal_type
      | identifier
      ;

component_type : type_denoter      ;

record_type : RECORD record_section_list END
      | RECORD record_section_list SEMI variant_part END
      | RECORD variant_part END
      ;


variant_part : CASE variant_selector OF variant_list SEMI
      | CASE variant_selector OF variant_list
      |
      ;

variant_selector : tag_field COLON tag_type
      | tag_type
      ;

variant_list : variant_list SEMI variant
      | variant
      ;

variant : case_constant_list COLON LPAREN record_section_list RPAREN
      | case_constant_list COLON LPAREN record_section_list SEMI
  variant_part RPAREN
      | case_constant_list COLON LPAREN variant_part RPAREN
      ;

case_constant_list : case_constant_list COMMA case_constant
      | case_constant
      ;
case_constant : unsignedConstant
      | unsignedConstant DOTDOT unsignedConstant
      ;

tag_field : identifier      ;

tag_type : identifier      ;

set_type : SET OF base_type
      ;

base_type : ordinal_type      ;

file_type : PFILE OF component_type
      ;

new_pointer_type : UPARROW domain_type
      ;

domain_type : identifier      ;


record_section_list : record_section_list SEMI record_section
      | record_section
      ;


record_section : identifier_list COLON type_denoter


repetetiveStatement
   : whileStatement
   | repeatStatement
   | forStatement
   ;

whileStatement
   : WHILE expression DO PBEGIN statements END 
   ;

repeatStatement
   : REPEAT statements UNTIL expression SEMI
   ;

forStatement
   : FOR IDENT ASSIGN forList DO PBEGIN statements END SEMI
   ;

forList
   : expression TO expression | expression DOWNTO expression
   ;
statements
   : statement statements|;
statement
   : NUM_INT COLON unlabelledStatement
   | unlabelledStatement 
   ;

unlabelledStatement
   : simpleStatement
   | structuredStatement
   ;

simpleStatement
   : assignmentStatement
   | gotoStatement
   | 
   ;

assignmentStatement
   : identifier ASSIGN expression 
   ;


    
structuredStatement
   : compoundStatement
   | conditionalStatement
   | repetetiveStatement
   ;


gotoStatement
   : GOTO NUM_INT
   ;



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
   : PBEGIN statements END SEMI

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
   : identifier
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
   : NUM_INT
   | constantChr
   | STRING_LITERAL
   | NIL
   ;

constantChr
   : CHR LPAREN NUM_INT RPAREN
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


%%



void main() {
    yyin = fopen("input.pas", "r");
    (yyparse()) ? printf("VERDICT: REJECTED by grammar G\n") : printf("VERDICT: ACCEPTED by grammar G\n");
    fclose(yyin);
}
