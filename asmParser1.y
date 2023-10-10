/* 
Fatal vulnerability: registers collision in complex expression.
    Example: When meet expr "a = 1*2 + 5*2 + 3*4;"
        Multiplications in right side will conflict on register r3
*/
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
#define ASM_LEN (4000)
#define ASM_TOTAL_LEN (40000)
#define NUM_NAME (10)
#define ERROR_LEN (200)
int yylex();
extern int yyparse();
FILE* yyin;
void yyerror(const char* s);

struct LINK{
    char    *name[NUM_NAME];   // Store names
    char    *type[NUM_NAME];   // Store types
    int     isConst[NUM_NAME];
    int     places[NUM_NAME];
    int     offset;
    int     numName;
    int     numChild;
    struct LINK    *father;
    struct LINK    *child[10];
    } root;
char *curType;
struct LINK *CUR = &root;
int curOffset   = 0;    // Used to record variables' relative offset.
int curIsConst  = 0;    // 0-Not const; 1-const
int curInDef    = 0;    // 0-Not in def; 1-In def
int isFunCall   = 0;    // 0-Not function call; 1-Function call
int paramNum    = 0;    // Number of parameters in function. Used in function call
int paramNumDef = 0;    // Number of parameters in function. Used in function definition
int constRel    = 0;    // Record relative place of consts
char* asmRes; 
char* tmpRes;
char* constAssign;      // Assignment instructions for const

char* commInst;
struct INFO{
    int place;
    struct LINK* block;
};

// Function declarations
void insert(struct LINK* newChild);
void addName(char* name);
void addType(char* type);
struct INFO  find(char* name, int isBigDomain);
int id(int t);
%}

// Define YYSTYPE with union
%union 
{
    char*   assem;
    char*   type;
    int     num;
    double  dou;
}

// Define types for tokens
%type<assem> ID expr ids_val BType f_expr fun_def 
%type<assem> param_list lines expr_inner call_param_list
%type<num> NUMBER

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
%token '{'
%token '}'
%token VOID
%token PARAM_LIST
%token CONST

// Set precedence
%right EQUAL
%left ADD MINUS
%left IDIV
%left MUL DIV
%right UMINUS         

%%


lines   :       lines expr ';'      {/*do nothing*/ }
        |       lines f_expr        {
                    /* char* assem = (char*)malloc (ASM_LEN);
                    strcat(assem, $2);
                    $$ = assem; */
                    strcat(tmpRes, $2);
                    }
        |       lines fun_def       {strcat(tmpRes, $2);}
        |                           {/* do nothing*/}
        ; 
fun_def :       BType ID LPAR 
        { // add a new block with new frame
                    // record old offset
                    paramNumDef = 0;
                    CUR->offset = curOffset;
                    // update curOffset to zero
                    curOffset = 0;
                    // create new block
                    struct LINK *newBlock = (struct LINK*)malloc(sizeof(struct LINK));
                    newBlock->numChild = 0;
                    newBlock->numName = 0;
                    insert(newBlock);
                    // switch to new block
                    CUR = newBlock;
                    }
                param_list RPAR '{' expr_inner '}'
                    {
                    char* assem = (char*) malloc (ASM_LEN);
                    char* fetchInst = (char*) malloc (ASM_LEN);
                    strcat(assem, ".global ");
                    strcat(assem, $2);
                    strcat(assem, "\n");
                    strcat(assem, $2);      // Add tag to assembly language
                    strcat(assem, ":\n");
                    // Append fixed instructions to assembly code
                    if (!strcmp($2, "main")){
                        strcat(assem, "push {fp, lr}\n");
                        strcat(assem, "mov fp, sp\n");
                        // Append assignments of const
                        if (strlen(constAssign) > 0){
                            strcat(assem, constAssign);
                            constAssign = "";
                        } 
                    }
                    else{
                        strcat(assem, "str fp, [sp, #-4]!\n");
                        strcat(assem, "mov fp, sp\n");
                    }
                    strcat(assem, $5);
                    // Prepare stack for parameters
                    for (int i = 0; i < paramNumDef; i++){
                        sprintf(fetchInst, "str r%d, [fp, #-%d]\n", i + 1, (i + 1) * 4);
                        strcat(assem, fetchInst);
                    }
                    strcat(assem, $8);
                    // Append fixed instructions to assembly code
                    if (!strcmp($2, "main")){
                        strcat(assem, "mov r0, #0\n");
                        strcat(assem, "mov sp, fp\n");
                        strcat(assem, "pop {fp, pc}\n");
                    }else{
                        strcat(assem, "mov sp, fp\n");
                        strcat(assem, "ldr fp, [sp], #4\n");
                        strcat(assem, "bx lr\n");
                    }
                    $$ = assem;
                    // switch to old block
                    CUR = CUR->father;
                    // recover curOffset to old value
                    curOffset = CUR->offset;
                    }
        |       VOID ID LPAR
                    { // add a new block with new frame
                    // record old offset
                    paramNumDef = 0;
                    CUR->offset = curOffset;
                    // update curOffset to zero
                    curOffset = 0;
                    // create new block
                    struct LINK *newBlock = (struct LINK*)malloc(sizeof(struct LINK));
                    newBlock->numChild = 0;
                    newBlock->numName = 0;
                    insert(newBlock);
                    // switch to new block
                    CUR = newBlock;
                    }
                param_list RPAR '{' expr_inner '}'
                    {
                    char* fetchInst = (char*) malloc (ASM_LEN);
                    char* assem = (char*) malloc (ASM_LEN);
                    strcat(assem, $2);      // Add tag to assembly language
                    strcat(assem, ":");
                    strcat(assem, "\n");
                    // Append fixed instructions to assembly code
                    if (!strcmp($2, "main")){
                        strcat(assem, "push {fp, lr}\n");
                        strcat(assem, "mov fp, sp\n");
                        // Append assignments of const
                        if (strlen(constAssign) > 0){
                            strcat(assem, constAssign);
                            constAssign = "";
                        } 
                    }
                    else{
                        strcat(assem, "str fp, [sp, #-4]!\n");
                        strcat(assem, "mov fp, sp\n");
                    }
                    strcat(assem, $5);
                    // Prepare stack for parameters
                    for (int i = 0; i < paramNumDef; i++){
                        sprintf(fetchInst, "str r%d, [fp, #-%d]\n", i + 1, (i + 1) * 4);
                        strcat(assem, fetchInst);
                    }
                    strcat(assem, $8);
                    // Append fixed instructions to assembly code
                    if (!strcmp($2, "main")){
                        strcat(assem, "mov r0, #0\n");
                        strcat(assem, "mov sp, fp\n");
                        strcat(assem, "pop {fp, pc}\n");
                    }else{
                        strcat(assem, "mov sp, fp\n");
                        strcat(assem, "ldr fp, [sp], #4\n");
                        strcat(assem, "bx lr\n");
                    }
                    $$ = assem;
                    // switch to old block
                    CUR = CUR->father;
                    // recover curOffset to old value
                    curOffset = CUR->offset;}
        ;

expr_inner  :   expr_inner f_expr   
            {
                    char* assem  = (char*) malloc (ASM_LEN);
                    strcat(assem, $1);
                    strcat(assem, $2);
                    $$ = assem;
                    }
            |   f_expr
                    {
                    $$ = $1;
                    }
            ;

f_expr  :       ID EQUAL expr ';'   {   
        struct INFO res = find($1, 1);   // Check if ID in sign table.
                    if (res.place == -1){
                        char* errorMsg = (char*) malloc (ERROR_LEN);
                        sprintf(errorMsg, "Define your variable before use it.");
                        yyerror(errorMsg);
                    }
                    if (res.block->isConst[res.place] == 1){
                        char* errorMsg = (char*) malloc (ERROR_LEN);
                        sprintf(errorMsg, "Do not redefine your const value");
                        yyerror(errorMsg);
                    }
                    char* assem = (char*) malloc (ASM_LEN);
                    char* ldInst = (char*)malloc(ASM_LEN);
                    strcat(assem, $3);
                    sprintf(ldInst, "str r0, [sp, #-%d]\n", 
                        (res.block->places[res.place] + 1)*4);
                    strcat(assem, ldInst);
                    $$ = assem;
                    }
        // TODO: Fucking funciton use.
        // Const declaration
        |       CONST  BType {curIsConst = 1; curInDef = 1;} ids_val ';'
                    { 
                    curIsConst = 0;
                    curInDef = 0; 
                    curType = $2; 
                    $$ = $4;
                    }
        // Declaration
        |       BType ids_val ';' 
                    { curType = $1; $$ = $2;}
        // Call Function
        |       ID LPAR {isFunCall = 1; paramNum = 0;} call_param_list RPAR ';'
                    {
                    isFunCall = 0;
                    char* assem = (char*) malloc (ASM_LEN);
                    char* callInst = (char*) malloc (ASM_LEN);
                    strcat(assem, $4);
                    sprintf(callInst, "bl %s\n", $1); // bl ID
                    strcat(assem, callInst);
                    $$ = assem;
                    }
        // epsilon
        |       ';'             {$$ = "";} 
        ;
expr    :       expr ADD expr   {
        char* assm = (char*) malloc (ASM_LEN);
                    strcat(assm, $1);
                    strcat(assm, "mov r3, r0\n");
                    strcat(assm, $3);
                    strcat(assm, "mov r1, r3\n");
                    strcat(assm, "mov r2, r0\n");
                    strcat(assm, "add r0, r1, r2\n");
                    $$ = assm;}
        |       expr MINUS expr { 
                    char* assem = (char*) malloc (ASM_LEN);
                    strcat(assem, $1);
                    strcat(assem, "mov r3, r0\n");
                    strcat(assem, $3);
                    strcat(assem, "mov r1, r3\n");
                    strcat(assem, "mov r2, r0\n");
                    strcat(assem, "sub r0, r1, r2\n");
                    $$ = assem;
                    }
        |       MINUS expr %prec UMINUS   {
                    char* assem = (char*) malloc (ASM_LEN);
                    strcat(assem, $2);
                    strcat(assem, "mul r0, #-1\n");
                    $$ = assem;
                    }
        |       NUMBER          {
                    char* assem = (char*) malloc (ASM_LEN);
                    sprintf(assem, "mov r0, #%d\n", $1);
                    $$ = assem;
                    }
        |       ID              {
                    struct INFO res = find($1, 1);
                    if (res.place == -1){
                        char* errorMsg = (char*) malloc (20);
                        sprintf(errorMsg, "fuck you with undefined variable \"%s\".", $1);
                        yyerror(errorMsg);
                    }
                    char* ldInst = (char*) malloc (ASM_LEN);
                    if (curInDef == 0){
                        if (res.block->isConst[res.place] == 0){ 
                            sprintf(ldInst, "ldr r0, [fp, #-%d]\n",             // TODO: Shouldn't find in definition.
                                (res.block->places[res.place] + 1) * 4);
                        }
                        else {
                            sprintf(ldInst, "ldr r1, _bridge + %d\n", 
                                res.block->places[res.place] * 4);
                            strcat(ldInst, "ldr r0, [r1]\n");
                        }
                    }
                    else 
                        ldInst = "";
                    $$ = ldInst;
                    }
        |       expr MUL expr   {
                    char* assem = (char*) malloc (ASM_LEN);
                    strcat(assem, $1);
                    strcat(assem, "mov r3, r0\n");
                    strcat(assem, $3);
                    strcat(assem, "mov r1, r3\n");
                    strcat(assem, "mov r2, r0\n");
                    strcat(assem, "mul r0, r1, r2\n");
                    $$ = assem;
                    }
        |       expr DIV expr   {
                    // TODO: complete this.
                    }
        |       expr IDIV expr  {
                    char* assem = (char*) malloc (ASM_LEN);
                    strcat(assem, $1);
                    strcat(assem, "mov r3, r0\n");
                    strcat(assem, $3);
                    strcat(assem, "mov r2, r0\n");
                    strcat(assem, "mov r1, r3\n");
                    // Perform division by loop
                    strcat(assem, "ARMDIV:\n");
                    strcat(assem, "sub r1, r2\n");
                    strcat(assem, "add r0, #1\n");
                    strcat(assem, "cmp r1, #0\n");
                    strcat(assem, "bgt ARMDIV\n");
                    strcat(assem, "sub r0, #1\n");
                    $$ = assem;
                    } 
        |       LPAR expr RPAR  {$$ = (char*)malloc(ASM_LEN); strcat($$, $2);}
        ;

param_list  :   BType ID COMMA param_list   {
            char* assem = (char*) malloc (ASM_LEN);
                    strcat(assem, "sub sp, #4\n");   // Prepare space for params
                    strcat(assem, $4);
                    curType = $1;
                    addName($2);
                    paramNumDef++;
                    $$ = assem;
                }
            |   BType ID                    {
                    char* assem = (char*) malloc (ASM_LEN);
                    strcat(assem, "sub sp, #4\n");
                    curType = $1;
                    addName($2);
                    paramNumDef++;
                    $$ = assem;
                }
            |   VOID                        {/* do nothing */}
            ;

call_param_list :   call_param_list COMMA expr 
                {
                        char* assem = (char*) malloc (ASM_LEN);
                        char* movInst = (char*) malloc (ASM_LEN);
                        strcat(assem, $1);
                        strcat(assem, $3);
                        sprintf(movInst, "mov r%d, r0\n", paramNum + 1);
                        strcat(assem, movInst);
                        paramNum++;
                        $$ = assem;
                        }
                |   expr
                        {
                        char* assem = (char*) malloc (ASM_LEN);
                        char* movInst = (char*) malloc (ASM_LEN);
                        strcat(assem, $1);
                        sprintf(movInst, "mov r%d, r0\n", paramNum + 1);
                        strcat(assem, movInst);
                        paramNum++;
                        $$ = assem;
                        }
                |   {}
                ;
ids_val :       ID EQUAL expr COMMA ids_val     { 
        struct INFO res = find($1, 0);
                    if (res.place != -1){
                        char* errorMsg = (char*) malloc (ERROR_LEN);
                        sprintf(errorMsg, "Do not redefine your variable.");
                        yyerror(errorMsg);
                    }
                    char* assem = (char*)malloc(ASM_LEN);
                    if (curIsConst == 0){
                        strcat(assem, $3);
                        strcat(assem, "push {r0}\n");
                        strcat(assem, $5);
                        $$ = assem;
                    } else {
                        char* comm = (char*) malloc (ASM_LEN);
                        sprintf(comm, ".comm %s, 4\n", $1);
                        strcat(commInst, comm);
                        sprintf(assem, ".word %s\n", $1);
                        strcat(tmpRes, assem);
                        assem = "";
                        // Store assignments in constAssign
                        strcat(constAssign, $3);
                        char* inst = (char*) malloc (ASM_LEN);
                        sprintf(inst, "ldr r1, _bridge + %d\n", constRel * 4);
                        strcat(constAssign, inst);
                        strcat(constAssign, "str r0, [r1]\n");
                        constRel++;
                    }
                    $$ = assem;
                    addName($1);
                    }
        |       ID EQUAL expr                   {
                    struct INFO res = find($1, 0);
                    if (res.place != -1){
                        char* errorMsg = (char*) malloc (ERROR_LEN);
                        sprintf(errorMsg, "Do not redefine your variable.");
                        yyerror(errorMsg);
                    }
                    char* assem = (char*) malloc (ASM_LEN);
                    if (curIsConst == 0){
                        strcat(assem, $3);
                        strcat(assem, "push {r0}\n");
                    } else {
                        sprintf(commInst, ".comm %s, 4\n", $1);
                        sprintf(assem, ".word %s\n", $1);
                        strcat(tmpRes, assem);
                        assem = "";
                        // Store assignments in constAssign
                        strcat(constAssign, $3);
                        char* inst = (char*) malloc (ASM_LEN);
                        sprintf(inst, "ldr r1, _bridge + %d\n", constRel * 4);
                        strcat(constAssign, inst);
                        strcat(constAssign, "str r0, [r1]\n");
                        constRel++;
                    }
                    $$ = assem;
                    addName($1);
                    }
        ;


BType   :       INT                 {$$ = (char*)malloc(ASM_LEN); $$ = "int";}
        |       FLOAT               {$$ = (char*)malloc(ASM_LEN); $$ = "float";}
        ;


%%

// programs section

void    insert(struct LINK* newChild){
        CUR->child[CUR->numChild] = newChild;
        newChild->father = CUR;
        CUR->numChild++;
        newChild->numChild++;
        if (CUR->numChild == 10)
            printf("ERROR: Too many scopes in your shit code!\n");
}

void    addName(char* name){
        CUR->name[CUR->numName]  = name;
        CUR->type[CUR->numName] = curType;
        CUR->places[CUR->numName] = curOffset;
        CUR->isConst[CUR->numName] = curIsConst;
        curOffset++;
        CUR->numName++;
        if (CUR->numName == 10)
            printf("ERROR: You use too many variables!\n");
    }

void    addType(char* type){
        CUR->type[CUR->numChild - 1] = type;
    }

struct INFO find(char* name, int isBigDomain){
        struct INFO res;
        struct LINK *state = CUR;
        while (state != NULL){
            for (int i = 0; i < state->numName; i++){
                if (!strcmp(name, state->name[i])){
                    res.place = i;
                    res.block = state;
                    return res;
                }
            }
            if (isBigDomain == 1)
                state = state->father;
            else
                state = NULL;
        }
        // error
        res.place = -1;
        return res;
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
    yylval.assem = (char*)malloc(ASM_LEN);
    while (isalpha(t) || isdigit(t) || t == (int)'_'){
        char temp[2];
        temp[0] = (char)t;
        temp[1] = '\0';
        strcat(yylval.assem, temp);
        t = getchar();
    }
    ungetc(t, stdin);
    if (!strcmp(yylval.assem, "int")){
        return INT;}
    else if (!strcmp(yylval.assem, "float"))
        return FLOAT;
    else if (!strcmp(yylval.assem, "void")){
        yylval.assem = "";
        return VOID;
    }
    else if (!strcmp(yylval.assem, "const")){
        return CONST;
    }
    else
        return ID;
}

/* ------------------------------------------------------------*/
// lexical analysis
int yylex()
{
    int t;
    while(1){
        t=getchar();
        if (t == EOF){
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
        }else if(isalpha(t)){
            return id(t);
        }else if(t == '='){
            return EQUAL;
        }else if (t == '{'){
        // Strictly this should be realized in Grammar Analysis.
        // But this version is easy and convient.
            return '{';
        }else if (t == '}'){
            return '}';
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
    /*
    .arch armv7-a
    .comm 
    -bridge
        .word a
        .word b
        ......
    */
    tmpRes = (char*) malloc (ASM_TOTAL_LEN);
    commInst = (char*) malloc (ASM_TOTAL_LEN);
    asmRes = (char*) malloc (ASM_TOTAL_LEN + 100);
    constAssign = (char*) malloc (ASM_TOTAL_LEN);
    strcat(asmRes, ".arch armv7-a\n");
    yyin=stdin;
    do{
        yyparse();
    }while(!feof(yyin));
    strcat(asmRes, commInst);
    strcat(asmRes, "_bridge:\n");
    strcat(asmRes, tmpRes);
printf("%s", asmRes);
return 0;
}
void yyerror(const char* s){
    fprintf(stderr,"Parse error: %s\n",s);
    exit(1);
}
