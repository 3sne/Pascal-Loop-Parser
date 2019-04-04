
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
%token PFILE "file" UPARROW "^" OF "of" STRING_LITERAL "literal" VAR "var" PACKED "packed" RECORD "record" CASE "case" SET "set" ARRAY "array" LABEL "label" CONST "const" STARSTAR TYPE "type";
%token FORWARD "forward" EXTERNAL "external" FUNCTION "function" PROCEDURE "procedure"

%error-verbose
%locations
%define parse.lac full
%define api.pure true

%%
augment : block
   ;


block : label_declaration_part
 constant_definition_part
 type_definition_part
 variable_declaration_part
 procedure_and_function_declaration_part
 statement_part
      ;

label_declaration_part : LABEL label_list SEMI
      |
      ;

label_list : label_list COMMA label
      | label
      ;

label : NUM_INT
      ;


constant_definition_part : CONST constant_list
      |
      ;

constant_list : constant_list constant_definition
      | constant_definition
      ;

constant_definition : identifier EQUAL cexpression SEMI
      ;

/*constant : cexpression      ;  /* good stuff! */

cexpression : csimple_expression
      | csimple_expression relationaloperator csimple_expression
      ;

csimple_expression : cterm
      | csimple_expression additiveoperator cterm
      ;

cterm : cfactor
      | cterm multiplicativeoperator cfactor
      ;

cfactor : sign cfactor
      | cexponentiation
      ;

cexponentiation : cprimary
      | cprimary STARSTAR cexponentiation
      ;

cprimary : identifier
      | LPAREN cexpression RPAREN
      | unsigned_constant
      | NOT cprimary
      ;

constant : non_string
      | sign non_string
      | STRING_LITERAL
      ;

sign : PLUS
      | MINUS
      ;

non_string : NUM_INT
      | identifier
      
      ;





type_definition_part : TYPE type_definition_list
      |
      ;

type_definition_list : type_definition_list type_definition
      | type_definition
      ;

type_definition : identifier EQUAL type_denoter SEMI
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


unsigned_constant : unsigned_number
      | STRING_LITERAL
      | NIL
      ;

unsigned_number : NUM_INT       ;



domain_type : identifier      ;


record_section_list : record_section_list SEMI record_section
      | record_section
      ;


record_section : identifier_list COLON type_denoter


variable_declaration_part : VAR variable_declaration_list SEMI
      |
      ;

variable_declaration_list :
   variable_declaration_list SEMI variable_declaration
      | variable_declaration
      ;

variable_declaration : identifier_list COLON type_denoter
      ;


procedure_and_function_declaration_part :
  proc_or_func_declaration_list SEMI
      |
      ;

proc_or_func_declaration_list :
   proc_or_func_declaration_list SEMI proc_or_func_declaration
      | proc_or_func_declaration
      ;

proc_or_func_declaration : procedure_declaration
      | function_declaration
      ;

procedure_declaration : procedure_heading SEMI directive
      | procedure_heading SEMI procedure_block
      ;

procedure_heading : procedure_identification
      | procedure_identification formal_parameter_list
      ;

directive : FORWARD
      | EXTERNAL
      ;

formal_parameter_list : LPAREN formal_parameter_section_list RPAREN      ;

formal_parameter_section_list : formal_parameter_section_list SEMI formal_parameter_section
      | formal_parameter_section
      ;

formal_parameter_section : value_parameter_specification
      | variable_parameter_specification
      | procedural_parameter_specification
      | functional_parameter_specification
      ;

value_parameter_specification : identifier_list COLON identifier
      ;

variable_parameter_specification : VAR identifier_list COLON identifier
      ;

procedural_parameter_specification : procedure_heading      ;

functional_parameter_specification : function_heading      ;

procedure_identification : PROCEDURE identifier      ;

procedure_block : block      ;

function_declaration : function_heading SEMI directive
      | function_identification SEMI function_block
      | function_heading SEMI function_block
      ;

function_heading : FUNCTION identifier COLON result_type
      | FUNCTION identifier formal_parameter_list COLON result_type
      ;

result_type : identifier      ;

function_identification : FUNCTION identifier      ;

function_block : block      ;

statement_part : compound_statement      ;


compound_statement : PBEGIN statement_sequence END      ;

statement_sequence : statement_sequence SEMI statement
      | statement
      ;

statement : 
      closed_statement |
      ;


closed_statement : label COLON non_labeled_closed_statement
      | non_labeled_closed_statement
      ;

non_labeled_closed_statement : 
      assignmentStatement
      | conditionalStatement
      | procedure_statement
      | gotoStatement
      | compound_statement
      | repetetiveStatement
      ;

procedure_statement : identifier params
      | identifier
      ;

params : LPAREN actual_parameter_list RPAREN      ;

actual_parameter_list : actual_parameter_list COMMA actual_parameter
      | actual_parameter
      ;

actual_parameter : expression
      | expression COLON expression
      | expression COLON expression COLON expression
      ;

repetetiveStatement
   : whileStatement
   | repeatStatement
   | forStatement
   ;

whileStatement
   : WHILE expression DO compound_statement
   ;

repeatStatement
   : REPEAT statement_sequence UNTIL expression SEMI
   ;

forStatement
   : FOR IDENT ASSIGN forList DO compound_statement SEMI
   ;

forList
   : expression TO expression | expression DOWNTO expression
   ;


assignmentStatement
   : identifier ASSIGN expression 
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
    yyin = fopen("input.txt", "r");
    (yyparse()) ? printf("VERDICT: REJECTED by grammar G\n") : printf("VERDICT: ACCEPTED by grammar G\n");
    fclose(yyin);
}
