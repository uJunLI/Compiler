%{
/*********************************************
将所有的词法分析功能均放在 yylex 函数内实现，为 +、-、*、\、(、 ) 每个运算符及整数分别定义一个单词类别，在 yylex 内实现代码，能
识别这些单词，并将单词类别返回给词法分析程序。
实现功能更强的词法分析程序，可识别并忽略空格、制表符、回车等
空白符，能识别多位十进制整数。
YACC file
**********************************************/
#include<stdio.h>
#include<stdlib.h>
#include<ctype.h>
#include<string.h>
#define LEN (10)
int yylex();
extern int yyparse();
FILE* yyin;
void yyerror(const char* s);

struct LINK{
    char    *name[LEN];   // Store names
    char    *type[LEN];   // Store types
    int     vals[LEN];
    int     numName;
    int     numChild;
    struct LINK    *father;
    struct LINK    *child[10];
    } root;
char *curType;
struct LINK *CUR = &root;

struct INFO{
    int val;
    int place;
    struct LINK* block;
};

// Function declarations
void insert(struct LINK* newChild);
void addName(char* name, int val);
void addType(char* type);
struct INFO find(char* name);
int id(int t);
%}

// Define YYSTYPE with union
%union 
{
    char*   str;
    char*   type;
    int     num;
    double  dou;
}

// Define types for tokens
%type<num> expr ids_val DEF_expr
%type<num> NUMBER
%type<type> BType
%type<str> ID

//TODO:给每个符号定义一个单词类别
%token ADD MINUS    // Define '+' '-'
%token MUL DIV      // Define '*', '/'
%token IDIV         // Define '\'
%token LPAR RPAR    // Define '(' ')'
%token NUMBER       // Define integer
%token EQUAL        // Define '='
%token ID           // Define identifier
%token COMMA 
%token RBIG
%token LBIG
%token INT FLOAT

// Set precedence
%right EQUAL
%left ADD MINUS
%left IDIV
%left MUL DIV
%nonassoc LPAR RPAR
%right UMINUS         

%%


lines   :       lines expr ';' { printf("%d\n", $2); }
        |       lines DEF_expr ';'  
        |       lines ';'
        |                       
        ; 

// TODO: get value of variable and calculate.
// TODO: 完善表达式的规则
DEF_expr:       
        // variable definition
                BType { curType = $1;} ids_val  
                { printf("DEBUG INFO: %s type variable detected\n", $1);}
        ;
expr    :       expr ADD expr   {
        $$ = $1 + $3;}
        |       expr MINUS expr { 
                    $$ = $1 - $3;}
        |       MINUS expr %prec UMINUS   {
                    $$ = -$2;}
        |       NUMBER          {$$ = $1;}
        |       ID              {
                    struct INFO res = find($1);
                    if (res.place == -1){
                        yyerror("Variable not exist");
                    } 
                    $$ = res.block->vals[res.place];
                    }
        |       expr MUL expr   {
                    $$ = $1 * $3;}
        |       expr DIV expr   {
                    $$ = $1 / $3;}
        |       LPAR expr RPAR  {$$ = $2;} 
        |       ID EQUAL expr   {
                    struct INFO res = find($1);   // Check if ID in sign table.
                    res.block->vals[res.place] = $3;
                    }
        ;

ids_val :       ID EQUAL expr COMMA ids_val     { 
                    struct INFO res = find($1);
                    if (res.place != -1)
                        yyerror("Variable redefined");
                    addName($1, $3);}
        |       ID EQUAL expr                   { 
                    struct INFO res = find($1);
                    if (res.place != -1)
                        yyerror("Variable redefined");
                    addName($1, $3);}
        ;

BType   :       INT                 {$$ = (char*)malloc(LEN); $$ = "int";}
        |       FLOAT               {$$ = (char*)malloc(LEN); $$ = "int";};


%%

// programs section

void    insert(struct LINK* newChild){
        CUR->child[CUR->numChild] = newChild;
        newChild->father = CUR;
        CUR->numChild++;
        newChild->numChild++;
    if (CUR->numChild == 10)
            printf("Too many scopes\n");
    }

void    addName(char* name, int val){
        CUR->name[CUR->numName] = name;
        CUR->type[CUR->numName] = curType;
        CUR->vals[CUR->numName] = val;
        CUR->numName++;
        if (CUR->numName == 10)
            yyerror("Too many variables!");
    }

void    addType(char* type){
        CUR->type[CUR->numChild - 1] = type;
    }

// Identify NUMBER
void number(int t)
{
    yylval.num = 0;
    while (isdigit(t)){
        yylval.num = yylval.num * 10 + t - (int)'0';
        t = getchar();
    }
    ungetc(t, stdin);
    return;
}

// Identify ID
int id(int t)
{
    yylval.str = (char*)malloc(LEN);
    while (isalpha(t) || isdigit(t) || t == (int)'_'){
        char temp[2];
        temp[0] = (char)t;
        temp[1] = '\0';
        strcat(yylval.str, temp);
        t = getchar();
    }
    ungetc(t, stdin);
    if (!strcmp(yylval.str, "int")){
        return INT;}
    else if (!strcmp(yylval.str, "float"))
        return FLOAT;
    else
        return ID;
}

struct INFO find(char* name){
        struct INFO res;
        struct LINK* state = CUR;
        while (state != NULL){
            for (int i = 0; i < state->numName; i++){
                if (!strcmp(name, state->name[i])){
                    res.place = i;
                    res.val = state->vals[i];
                    res.block = state;
                    return res;
                }
            }
            state = state->father;
        }
        // error
        res.place = -1;
        return res;
}

/* ------------------------------------------------------------*/
// lexical analysis
int yylex()
{
    int t;
    while(1){
        t=getchar();
        if (t == EOF){
            printf("at fucking end\n");
            break;
        }
        if(t==' '||t=='\t'||t=='\n'){
            //do noting
        }else if(isdigit(t)){
          number(t);
          return NUMBER;
        }else if(t=='+'){
            return ADD;
        }else if(t=='-'){
            return MINUS;
        }else if(t == '*'){
            return MUL;
        }else if(t == '/'){
            return DIV;
        }else if(t == '\\'){
            return IDIV;
        }else if(t == '('){
            return LPAR;
        }else if(t == ')'){
            return RPAR;
        }else if(isalpha(t) || t == (int)'_'){
            return id(t);
        }else if(t == '='){
            return EQUAL;
        }else if (t == '{'){
        // Strictly this should be realized in Grammar Analysis.
        // But this version is easy and convient.
            struct LINK *newBlock = (struct LINK*)malloc(sizeof(struct LINK));
            newBlock->numChild = 0;
            newBlock->numName = 0;
            insert(newBlock);
            CUR = newBlock;
            continue;
        }else if (t == '}'){
            CUR = CUR->father;
            continue;
        }else if (t == ','){
            return COMMA;
        }else if (t == ';'){
            return ';';
        }
        else{
            return t;
        }
    }
}

int main(void)
{
    yyin=stdin;
    do{
        yyparse();
    }while(!feof(yyin));
    return 0;
}
void yyerror(const char* s){
    fprintf(stderr,"Parse error: %s\n",s);
    exit(1);
}
