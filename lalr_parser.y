
%{
   #include <stdio.h>
   #include <stdlib.h>
   #include <string.h>
   #include "symtable.h"
   #define ANSI_COLOR_RED     "\x1b[31m"
   #define ANSI_COLOR_GREEN   "\x1b[32m"
   #define ANSI_COLOR_RESET   "\x1b[0m"
   #define ANSI_COLOR_BLUE    "\x1b[34m"
   #define ANSI_COLOR_YELLOW  "\x1b[33m"
   #define ANSI_COLOR_MAGENTA "\x1b[35m"



   #define YYERROR_VERBOSE 1
   extern FILE * yyin;
   void yyerror(char* msg);
   extern int rownum, colnum;
   extern char s[1000];
   extern int lastupdate;
   int yylex();
   int var_activate = 0;
   char*  identifiers[1000];
   int id_cnt = 0;
   int failure = 0;
   char* argument_buffer[1000];
   char type[1000];
   char memsize[100];
   int stcnt = 0;
   symTableEntry* st[1000];
   symTableEntry* newSte(char l[]);
   int scnt = 0;
   
   char* function[1000];
   char returntype[1000];
   int argument_buffer_activate = 0;
   int cur_function_index = 0;

   int arg_cnt = 0;

   symTableEntry* newSte(char l[]) {
    symTableEntry* temp = (symTableEntry*)malloc(sizeof(symTableEntry));
    temp->id = scnt ++;
    temp->next = NULL;
    strcpy(temp->varName, l);
   int arr1;
   int arr2;
   int num;
    return temp;
}

void printSymTable() {
    printf("\n");
    printf(ANSI_COLOR_YELLOW "---------------------------------------------------------------------------------------------------------------------\n");
    printf("|                                                    SYMBOL TABLE                                                    \n");
    printf("---------------------------------------------------------------------------------------------------------------------\n");
    printf("|    VAR    |    TYPE     | INDEX |       VAR_SIZE       |    SCOPE    |ARG_CNT|   RET TYPE   |     ARGUMENT LIST            \n");
    printf("------------+-----------+-------+----------------------+------+--------+----------+----------------------------------\n");
    symTableEntry* temp;
    for(int i = 0; i < 10; i++) {
        for(temp = st[i]; temp; temp = temp->next) {
            printf("|%10s | %9s    | %5d |  %18s  |  %10s | %6d | %8s |", temp->varName, temp->varType, temp->id, temp->varMemSize, temp->scope, temp->numArgs, temp->returnType);
            if ( temp->numArgs > 0) {
                for ( int i = 0; i < temp->numArgs; i++) {
                    printf(" %8s ", temp->args[i]);
                }
            }
            printf("\n");
        }
    }
    printf("=====================================================================================================================" ANSI_COLOR_RESET "\n");
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






    // %token LITERAL SEMICOLON COMMA COLON ASSIGN LT GT LTE GTE EQUAL NOTEQUAL ADD MULTIPLY SUBTRACT DIVIDE COMP_AND COMP_DAND VOID COMP_OR COMP_DOR LP RP LC RC LSB RSB CHAR INT UINT SIGNED UNSIGNED SHORT LONG FLOAT DOUBLE REGISTER CONST IF ELSE FOR WHILE DO SWITCH CASE DEFAULT BREAK CONTINUE ENUM TYPEDEF EXTERN RETURN UNION GOTO ID NUM MOD;
%}


%union {
    char *string;
    int token;
}


%token AND "and" DIV "div" DO "do" DOWNTO "downto" ELSE "else" END "end" FOR "for" GOTO "goto" IF "if" IN "in" MOD "mod" NIL "nil" NOT "not" OR "or" PBEGIN "begin" REPEAT "repeat" 
%token THEN "then" TO "to" UNTIL "until" WHILE "while" <string> IDENT "identifier" ASSIGN ":=" COLON ":" COMMA "," EQUAL "=" GE ">=" GT ">" LBRACK "[" LE "<=" LPAREN "("
%token LT "<" MINUS "minus" NOT_EQUAL "<>" PLUS "+" RBRACK "]" NUM_INT "number" RPAREN ")" SEMI "semicolon" SLASH "/" STAR "*"  DOTDOT ".." CHR "character"
%token PFILE "file" UPARROW "^" OF "of" STRING_LITERAL "literal" VAR "var" PACKED "packed" RECORD "record" CASE "case" SET "set" ARRAY "array" LABEL "label" CONST "const" STARSTAR "**" TYPE "type";
%token FORWARD "forward" EXTERNAL "external" FUNCTION "function" PROCEDURE "procedure" DOT "." <string> BOOLEAN "boolean" <string> CHAR "char" <string> INTEGER "integer" <string> REAL "real";

%error-verbose


%type <string> identifier



%%
augment : block DOT
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


constant_definition_part : CONST constant_list | error
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
      | type_definition | error 
      ;

type_definition : identifier EQUAL type_denoter SEMI {
         char* temp6 = (char*)malloc(sizeof(char)*50);
         temp6 = strdup($1);
         char temp3_type[] = "TYPE";

         symTableEntry*  ste = newSte(temp6);
         strcpy(ste->varType, type);
         strcpy(ste->scope, function[cur_function_index]);
         insert(st,ste,&stcnt);
      }
      ;


identifier_list : identifier_list COMMA identifier
 {
   
   if (var_activate)
   {

      identifiers[id_cnt++] = strdup($3);
      
   }else if(argument_buffer_activate)
   {
      argument_buffer[arg_cnt++] = strdup($3);
      identifiers[id_cnt++] = strdup($3);
   }
}
      | identifier
      {

         if(var_activate)
         {

            identifiers[id_cnt++] = strdup($1);

         }else if(argument_buffer_activate)
         {
            argument_buffer[arg_cnt++] = strdup($1);
            identifiers[id_cnt++] = strdup($1);
         }
      }
      | error
      ;

type_denoter :
      identifier {if(var_activate) {
         char* temp2=(char*)malloc(sizeof(char)*50);
         temp2 = strdup($1);
         
         
         if(search(st, temp2, function[cur_function_index])==-1)
         {
            char msg[1000]  = "undefined type: ";
            strcat(msg, temp2);
            strcat(msg, "\0");
            yyerror(msg);
         }
         strcpy(type,temp2);


       }}
      |REAL {if(var_activate) {strcpy(type, "real\0");memsize[0] = 1+48; memsize[1] = 6 + 48;}}
      |BOOLEAN {if(var_activate) {strcpy(type, "boolean\0");memsize[0] = 1+48;}}
      |CHAR {if(var_activate) {strcpy(type, "char\0");memsize[0] = 1 + 48;}}
      |INTEGER {if(var_activate) {strcpy(type, "integer\0");memsize[0] = 4 + 48;}}
      | new_type
      | error
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

array_type : ARRAY LBRACK index_list RBRACK OF component_type{
   strcat(type, "array");
   
}
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


variable_declaration_part : VAR {var_activate = 1;} variable_declaration_list SEMI {var_activate = 0;}
      |
      ;

variable_declaration_list :
   variable_declaration_list SEMI variable_declaration
      | variable_declaration
      ;

variable_declaration : identifier_list COLON type_denoter {

   for(int i = 0;i<id_cnt;i++)
   {
      
      if(search(st, identifiers[i], function[cur_function_index])==-1)
      {
         symTableEntry*  ste = newSte(identifiers[i]);
         strcpy(ste->varType, type);
         strcpy(ste->varMemSize, memsize);
         strcpy(ste->scope, function[cur_function_index]);
         insert(st,ste,&stcnt);
      }else
      {
         char msg[100] = "repeated or conflicting declaration of variable ";
         strcat(msg, identifiers[i]);
         yyerror(msg);
      }
   }
   id_cnt=0;
}
|error
      ;


procedure_and_function_declaration_part :
  proc_or_func_declaration_list SEMI
      |
      ;

proc_or_func_declaration_list :
   proc_or_func_declaration_list SEMI proc_or_func_declaration
      | proc_or_func_declaration |error
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

formal_parameter_list : LPAREN {argument_buffer_activate = 1;} formal_parameter_section_list 

RPAREN   |error   ;

formal_parameter_section_list : formal_parameter_section_list SEMI formal_parameter_section
      | formal_parameter_section | error 
      ;

formal_parameter_section : value_parameter_specification
      | variable_parameter_specification
      | procedural_parameter_specification
      | functional_parameter_specification
      |  error
      ;

value_parameter_specification : identifier_list COLON type_denoter{
   for(int i = 0;i<id_cnt;i++)
   {
      
      if(search(st, identifiers[i], function[cur_function_index])==-1)
      {
         symTableEntry*  ste = newSte(identifiers[i]);
         strcpy(ste->varType, type);
         strcpy(ste->varMemSize, memsize);
         strcpy(ste->scope, function[cur_function_index]);
         insert(st,ste,&stcnt);
      }else
      {
         char msg[100] = "repeated or conflicting declaration of variable ";
         strcat(msg, identifiers[i]);
         yyerror(msg);
      }
   }
   id_cnt = 0;

}
      ;

variable_parameter_specification : VAR identifier_list COLON identifier
      ;

procedural_parameter_specification : procedure_heading      ;

functional_parameter_specification : function_heading      ;

procedure_identification : PROCEDURE identifier      ;

procedure_block : block      ;

function_declaration : function_heading SEMI directive
      | function_identification SEMI function_block
      | function_heading  SEMI function_block {cur_function_index--;} | error
      ;

function_heading : FUNCTION identifier COLON result_type
      | FUNCTION identifier {function[++cur_function_index] = strdup($2);} formal_parameter_list COLON result_type {

         symTableEntry*  ste = newSte(function[cur_function_index]);

         strcpy(ste->returnType, returntype);
         
         for(int i = 0; i < arg_cnt; i++)
         {
            ste->args[i] = argument_buffer[i];
         }
         ste->numArgs = arg_cnt;
         arg_cnt = 0;
         insert(st,ste,&stcnt);
      }|error

      ;

result_type : type_denoter {strcpy(returntype, type);}     ;

function_identification : FUNCTION identifier      ;

function_block : block      ;

statement_part : compound_statement      ;


compound_statement : PBEGIN statement_sequence END      ;

statement_sequence :  statement_sequence SEMI statement
      | statement
      ;

statement : 
      closed_statement |     
      ;


closed_statement : label COLON non_labeled_closed_statement
      | non_labeled_closed_statement |error
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
   : WHILE expression DO compound_statement SEMI
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
   : IF expression THEN statement_sequence SEMI
   | IF expression THEN statement_sequence  ELSE statement_sequence SEMI
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

void yyerror(char *msg) {
    colnum-=lastupdate;
    printf(ANSI_COLOR_RED "\nERROR" ANSI_COLOR_RESET "\n" );
    printf("---------------------------\n");
    printf("ROW: %d COL: %d\n", rownum, colnum);
    printf("%s ...\n", s);
    for(int i=0;i<colnum-1;i++) {
        printf(" ");
    }    
    printf(ANSI_COLOR_GREEN"^" ANSI_COLOR_RESET "\n--------------------------\n");

    printf(ANSI_COLOR_RED"%s" ANSI_COLOR_RESET"\n--------------------------\n", msg);
    failure = 1;
}

void main() {
    yyin = fopen("input.txt", "r");
    char* m = malloc(sizeof(char)*100);
   strcpy(m,"main");
   function[0] = m;

    if(yyparse())
    {
     printf(ANSI_COLOR_RED "VERDICT: REJECTED by grammar G" ANSI_COLOR_RESET "\n"); 
   }else
  {
      if(failure)
      {
         printf(ANSI_COLOR_RED "VERDICT: REJECTED by grammar G" ANSI_COLOR_RESET "\n");
      }else
      {
         printf( ANSI_COLOR_GREEN "VERDICT: ACCEPTED by grammar G"ANSI_COLOR_RESET"\n");
      }
  }
    fclose(yyin);
}
