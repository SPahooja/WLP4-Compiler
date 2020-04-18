#include <iostream>
#include <sstream>
#include <map>
#include <vector>
#include <utility>
using namespace std;

typedef pair<string, int> ValTable;
typedef map<string,ValTable> SymbTable;
typedef pair<vector<string>, SymbTable> SymbTablewithSig;
typedef map<string,SymbTablewithSig> SymbTablewithSigwithProc;
SymbTablewithSigwithProc st;
int ct = 0;
string terminals[]  = {"BOF", "BECOMES", "COMMA", "ELSE", "EOF", "EQ", "GE", "GT", "ID", "IF", "INT", "LBRACE", "LE", "LPAREN", "LT", "MINUS", "NE", "NUM", "PCT", "PLUS", "PRINTLN", "RBRACE", "RETURN", "RPAREN", "SEMI", "SLASH", "STAR", "WAIN", "WHILE", "AMP", "LBRACK", "RBRACK", "NEW", "DELETE", "NULL"};
string procedure = "empty";
string SigProcedure = "empty";
bool error = false;
int offset = -4;
int loopcounter = 0;
int ifcounter = 0;
int delcounter = 0;

class Tree{
    public:
    string rule;
    vector<string> tokens;
    vector<Tree> children;
};
void PrintSymbolTable(){
    for (map<string,SymbTablewithSig>::const_iterator jt = st.begin(); jt != st.end(); ++jt){
        cerr << jt->first;
        procedure = jt->first;
        for(vector<string>::const_iterator kt = st[procedure].first.begin(); kt != st[procedure].first.end(); ++kt){
            cerr << " " << *kt ;
        }
        cerr << endl;
        for(map<string,ValTable>::const_iterator it = st[procedure].second.begin(); it != st[procedure].second.end(); ++it){
            cerr << it->first << " ";
            string var = it->first;
            cerr << st[procedure].second[var].first << " ";
            cerr << st[procedure].second[var].second << endl;
           // for(pair<string,int>::const_iterator lt = st[procedure].second[var].first;)
        }
        cerr << endl;
    }
}

Tree BuildNode(string rule){
    Tree ptree;
    bool state = false;
    ptree.rule = rule;

    istringstream ss(rule);
    string token;

    while(ss >> token){
        ptree.tokens.push_back( token );
    }

    for(int i = 0; i < 35; i++){
        if( ptree.tokens[0] == terminals[i] ){
        state = true;
        }
    } 
  

    if(state == false){
        for(int i = 1; i < ptree.tokens.size(); i++) {
            string line;
            getline(cin,line);
            ptree.children.push_back(BuildNode(line));
        }
    }

    return ptree;
}

void push(int regis);

void pop(int regis);

void load(int regis, int value);

void load(int regis, string value);

void add(int dest, int reg1, int reg2);

void BuildSymbolTable(Tree ptree){
    for(vector<Tree>::const_iterator it = ptree.children.begin(); it != ptree.children.end(); ++it){
        if(it->rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
            if(st.find("wain") == st.end()){
                procedure = "wain";
                st.insert(make_pair(procedure,SymbTablewithSig()));

                if(it->children[3].children[0].rule == "type INT STAR"){
                    st[procedure].first.push_back( it->children[3].children[0].children[0].tokens[1] + it->children[3].children[0].children[1].tokens[1] );
                } else if (it->children[3].children[0].rule == "type INT")
                {
                    st[procedure].first.push_back( it->children[3].children[0].children[0].tokens[1] );
                }
                push(1);
                if(it->children[5].children[0].rule == "type INT STAR"){
                    st[procedure].first.push_back( it->children[5].children[0].children[0].tokens[1] + it->children[5].children[0].children[1].tokens[1] );
                } else if (it->children[5].children[0].rule == "type INT")
                {
                    st[procedure].first.push_back( it->children[5].children[0].children[0].tokens[1] );
                }
                push(2);
            } else{
                //cerr << "ERROR: WAIN has already been declared" << endl;
                error = true;
            }
        } else if(it->rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
            if(st.find(it->children[1].tokens[1]) == st.end()){
                procedure = it->children[1].tokens[1];
                st.insert(make_pair(procedure,SymbTablewithSig()));
            } else {
                cerr << "ERROR: Function" << it->children[1].tokens[1] << "has already been declared"<< endl;
                error = true;
            }
        } else if (it->rule == "paramlist dcl COMMA paramlist" || it->rule == "paramlist dcl"){
            if(it->children[0].children[0].rule == "type INT"){
                st[procedure].first.push_back(it->children[0].children[0].children[0].tokens[1]);
            } else if(it->children[0].children[0].rule == "type INT STAR"){
                st[procedure].first.push_back( it->children[0].children[0].children[0].tokens[1] + it->children[0].children[0].children[1].tokens[1] );
            }
        } else if (it->rule == "factor ID LPAREN arglist RPAREN" || it->rule == "factor ID LPAREN RPAREN"){
          if (st.find(it->children[0].tokens[1]) == st.end())
          {
           cerr << "ERROR: function " << it->children[0].tokens[1] << " has not been declared before calling" << endl;
           error = true;   
          }  
        } else if (it->rule == "dcl type ID"){
            if(st[procedure].second.find(it->children[1].tokens[1]) == st[procedure].second.end()){
                if(it->children[0].rule == "type INT"){
                    st[procedure].second.insert(make_pair(it->children[1].tokens[1] , make_pair(it->children[0].children[0].tokens[1],offset)));
                } else if(it->children[0].rule == "type INT STAR"){
                    st[procedure].second.insert(make_pair(it->children[1].tokens[1] , make_pair(it->children[0].children[0].tokens[1] + it->children[0].children[1].tokens[1], offset)));
                }
                offset = offset - 4;
            } else {
            //cerr << "ERROR: " << it->children[1].tokens[1] << " has already been declared" << endl;
            error = true;
            }
        } else if(it->rule == "dcls dcls dcl BECOMES NUM SEMI"){
            load(3,it->children[3].tokens[1]);
            push(3);
        }else if(it->rule == "dcls dcls dcl BECOMES NULL SEMI"){
            add(3,11,0);
            push(3);
        }
        BuildSymbolTable(*it); 
    }
}


void  CheckUndeclaredUsed(Tree ptree){
    for(vector<Tree>::const_iterator it = ptree.children.begin(); it != ptree.children.end(); ++it){
        if(it->rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
            procedure = "wain";
        } else if(it->rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
            procedure = it->children[1].tokens[1];
        } else if(it->rule == "factor ID"){
            for(map<string,SymbTablewithSig>::const_iterator jt = st.begin(); jt != st.end(); ++jt){
                if(st[procedure].second.find(it->children[0].tokens[1]) == st[procedure].second.end()) {
                    //cerr << "ERROR: Variable "<< it->children[0].tokens[1] <<" hasn't been decelared before in " << procedure << endl;
                    error = true;
                    break; //*********************************** MAY BREAK STUFF***********************************
                }
            }
        }
        CheckUndeclaredUsed(*it);
    }
}

string typeOf(Tree ptree);

void Get_Type(Tree ptree){
    for(vector<Tree>::const_iterator it = ptree.children.begin(); it != ptree.children.end(); ++it){
        if(it->rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
            procedure = "wain";
        }else if (it->rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
            procedure = it->children[1].tokens[1];
        }
        size_t found= typeOf(*it).find("ERROR");
         if ( found != string::npos)         // ***************<<<<CHECK>>>>>**************
         {
             cerr << typeOf(*it) << endl;
             error = true;
         }
        
        Get_Type(*it);
    }
}

string typeOf(Tree pt){
    if(pt.tokens[0] == "NUM"){
        return "int";
    }else if(pt.rule == "lvalue STAR factor" || pt.rule == "factor STAR factor"){
        if(typeOf(pt.children[1]) == "int*"){
            return "int";
        } else {
            return "ERROR: Invalid pointer declaration";
        }
    }else if(pt.rule == "type INT"){
        return "int";
    }else if(pt.rule == "type INT STAR"){
        return "int*";
    }else if(pt.rule == "dcl type ID"){
        return typeOf(pt.children[0]);
    }else if(pt.rule == "term factor" || pt.rule == "expr term") {
        return typeOf(pt.children[0]);
    }else if (pt.rule == "factor NUM" || pt.rule == "factor ID" || pt.rule == "lvalue ID"){
        return typeOf(pt.children[0]);
    }else if (pt.rule == "factor AMP lvalue"){
        if(typeOf(pt.children[1]) == "int"){
            return "int*";
        }else{
            return "ERROR: Amp value";
        }
    }else if (pt.rule == "test expr NE expr" || pt.rule == "test expr EQ expr"){
        if(typeOf(pt.children[2]) == typeOf(pt.children[0])){
            return typeOf(pt.children[2]);
        }else {
            return "ERROR: Can't compare different types";
        }
    }else if(pt.rule == "test expr LE expr" || pt.rule == "test expr GT expr" || pt.rule == "test expr GE expr" || pt.rule == "test expr LT expr"){
        if(typeOf(pt.children[2]) == typeOf(pt.children[0])){
            return typeOf(pt.children[2]);
            /*if(typeOf(pt.children[2]) == "*int"){
                return "Comapring an array of integers";
            }else{
                return "Valid Comparison";
            }*/
        } else {
            return "ERROR : Can't compare values of different types";
        }
    }else if(pt.rule == "lvalue LPAREN lvalue RPAREN" || pt.rule == "factor LPAREN expr RPAREN"){ 
        return typeOf(pt.children[1]);
    }else if(pt.rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        if(typeOf(pt.children[9]) == "int"){
            return typeOf(pt.children[9]);
        }else {
            return "ERROR: Function has to be of type 'int'";
        }
    }else if(pt.rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        if(typeOf(pt.children[11]) == "int" && typeOf(pt.children[5]) == "int"){
            return typeOf(pt.children[11]);
        }else {
            return "ERROR: WAIN has to be of type 'int'";
        }
    }else if(pt.rule == "statement DELETE LBRACK RBRACK expr SEMI"){
        if(typeOf(pt.children[3]) != "int*"){
            return "ERROR: Can't delete non-int*";
        }
    }else if(pt.rule == "statement lvalue BECOMES expr SEMI"){
        if(typeOf(pt.children[2]) == typeOf(pt.children[0])){
            return typeOf(pt.children[0]);
        } else {
            return "";
            return "ERROR: Can't assign values of different types";
        }
    }else if(pt.rule == "PRINTLN LPAREN expr RPAREN SEMI"){
        if(typeOf(pt.children[1]) != "int"){
            return "PRINTLN can't be used without an INT";
        }
    }else if(pt.rule == "factor NEW INT LBRACK expr RBRACK"){
        if(typeOf(pt.children[3]) == "int"){
            return "int*";
        }else {
            return "ERROR: Can' use new with non-int";
        }
    }else if(pt.rule == "dcls dcls dcl BECOMES NUM SEMI"){
        if(typeOf(pt.children[1].children[1]) == "int"){
            return typeOf(pt.children[1]);
        }else {
            return "ERROR: Need to have a NULL value to initialise int*";
        }
    }else if(pt.rule == "dcls dcls dcl BECOMES NULL SEMI"){
        if(typeOf(pt.children[1].children[1]) == "int*"){
            return typeOf(pt.children[1]);
        }else {
            return "ERROR: Need to have a non-NULL value to initialise int";
        }
    }else if(pt.rule == "factor ID LPAREN arglist RPAREN"){
        ct = 0;
        SigProcedure = pt.children[0].tokens[1];
        return typeOf(pt.children[2]);
    }else if(pt.rule == "arglist expr"){
        if(ct == st[SigProcedure].first.size() - 1 && ct < st[SigProcedure].first.size() && st[SigProcedure].first[ct] == typeOf(pt.children[0])) {
            return typeOf(pt.children[0]);
        }else{
            return "ERROR: Insufficient or mismatched signatures";
        }
    }else if(pt.rule == "arglist expr COMMA arglist"){
        if(ct<st[SigProcedure].first.size() && st[SigProcedure].first[ct] == typeOf(pt.children[0])){
            ct++;
            return typeOf(pt.children[2]);
        } else{
            return "ERROR: Insufficient or mismatched signatures";
        }
    }else if(pt.rule == "term term SLASH factor" || pt.rule == "term term STAR factor" || pt.rule == "term term PCT factor"){
        if(typeOf(pt.children[0]) == "int" && typeOf(pt.children[2]) == "int"){
            return "int";
        } else{
            return "ERROR: Can't multiply non-ints";
        }
    }else if(pt.rule == "expr expr MINUS term"){
        if(typeOf(pt.children[0]) == "int" && typeOf(pt.children[2]) == "int*"){
            return "ERROR: Can't compute int - int*";
        }else if(typeOf(pt.children[0]) == "int*" && typeOf(pt.children[2]) == "int"){
            return "int*";
        } else{
            return "int";
        }
    }else if(pt.rule == "expr expr PLUS term"){
        if(typeOf(pt.children[0]) == "int*" && typeOf(pt.children[2]) == "int*"){
            return "ERROR: Can't compute int* + int*";
        } else if(typeOf(pt.children[0]) == "int" && typeOf(pt.children[2]) == "int"){
            return "int";
        } else{
            return "int*";
        }
    }else if(pt.tokens[0] == "ID"){
        return st[procedure].second[pt.tokens[1]].first;
    }else if(pt.rule == "factor ID LPAREN RPAREN"){
        return "int";
    }
    
    else
    {
        return "Unknown type";
    }
}

void push(int regis){
    cout << "sub $30, $30, $4" << endl;
    cout << "sw $" << regis << ", 0($30)" << endl;
}

void pop(int regis){
    cout << "lw $" << regis << ", 0($30)" << endl;
    cout << "add $30, $30, $4" << endl;
}

void load(int regis, int value){
    cout << "lis $" << regis << endl;
    cout << ".word " << value << endl ;
}

void load(int regis, string value){
    cout << "lis $" << regis << endl;
    cout << ".word " << value << endl;
}

void add(int dest, int reg1, int reg2){
    cout << "add $" << dest << ", $" << reg1 << ", $" << reg2 << endl;
}

void sub(int dest, int reg1, int reg2){
    cout << "sub $" << dest << ", $" << reg1 << ", $" << reg2 << endl;
}

void multl(int dest, int reg1, int reg2){
    cout << "mult $" << reg1 << ", $" << reg2 << endl;
    cout << "mflo $" << dest << endl;
}

void multh(int dest, int reg1, int reg2){
    cout << "mult $" << reg1 << ", $" << reg2 << endl;
    cout << "mfhi $" << dest << endl;
}

void div(int dest, int reg1, int reg2){
    cout << "div $" << reg1 << ", $" << reg2 << endl;
    cout << "mflo $" << dest << endl;
}

void mod(int dest, int reg1, int reg2){
    cout << "div $" << reg1 << ", $" << reg2 << endl;
    cout << "mfhi $" << dest << endl;
}

void jr (int regs){
    cout << "jr $" << regs << endl;
}

void jalr (int regs){
    cout << "jalr $" << regs << endl;
}

void imports(string s){
    cout << ".import " << s << endl;
}

void prologue(){
    cout << ";SAHIL" << endl;
    cout << ";Prologue" << endl;
    imports("print");
    imports("init");
    imports("new");
    imports("delete");
    load(4,4);
    //load(10,"print");
    load(11,1);
    add(29,30,0);
}

void epilogue(){
    cout << ";Epilogue" << endl;
    add(30,29,0);
    jr(31);
}

void init(){
    cout << ";Init" << endl;    
    if(st["wain"].first.front() == "int*"){
        cout<< ";using init" << endl;;
        add(2,2,0);
    }else{
        add(2,0,0);
    }
    load(10,"init");
    push(29);
    push(31);
    jalr(10);
    pop(31);
    pop(29);
}

void lw29(int regis, int offset){
    cout << "lw $" << regis << ", " << offset << "($29)" << endl;
}

void lw(int regis, int offset, int addr){
    cout << "lw $" << regis << ", " << offset << "($" << addr << ")" << endl;
}

void sw29(int regis, int offset){
    cout << "sw $" << regis << ", " << offset << "($29)" << endl;
}

void sw(int regis, int offset, int addr){
    cout << "sw $" << regis << ", " << offset << "($" << addr << ")" << endl;
}

void beq(int reg1, int reg2, string branch){
    cout <<"beq $" << reg1 << ", $" << reg2 << ", " << branch << endl;
}

void bne(int reg1, int reg2, string branch){
    cout <<"bne $" << reg1 << ", $" << reg2 << ", " << branch << endl;
}

void slt(int dest, int reg1, int reg2){
    cout << "slt $" << dest << ", $" << reg1 << ", $" << reg2 <<endl; 
}

void sltu(int dest, int reg1, int reg2){
    cout << "sltu $" << dest << ", $" << reg1 << ", $" << reg2 <<endl; 
}

string s(int n){
    stringstream ss;
    ss << n;
    string s;
    ss >> s;
    return s;
}

void call(string s){
    push(29);
    push(31);
    load(10,s);
    jalr(10);
    pop(31);
    pop(29);
}

void comment(string c){
    cout << ";" << c << endl;
}

void debug(int reg){
    comment("DEBUG");
    sw(1,-4,30);
    sw(31,-8,30);
    load(1,8);
    sub(30,30,1);
    add(1,reg,0);
    load(31,"print");
    jalr(31);
    load(1,8);
    add(30,30,1);
    lw(31,-8,30);
    lw(1,-4,30); 
    comment("DEBUG OVER"); 
}

void print(string c){
    cout << c << endl;
}

int offset_of(string v){
    return st[procedure].second[v].second;
}

void code(Tree t){
    if(t.tokens[0] == "start"){                                           // root node
        comment("got start");
        code(t.children[1]);
    }else if (t.rule == "procedures main"){                             // next procedure is main
        code(t.children[0]);
    }else if (t.tokens[0] == "main"){                                    // got main(wain) function
        procedure = "wain";
        print("WAIN:");
        comment("got WAIN");
        code(t.children[8]);
        comment("done dcls for WAIN");
        code(t.children[9]);
        comment("done statements for WAIN");
        code(t.children[11]);
        comment("done return for WAIN");
    }else if (t.tokens[0] == "ID"){                                      // got a variable, load it from the offest from symbol table into $3
        lw29(3,offset_of(t.tokens[1]));
    }else if (t.rule == "expr term" || t.rule == "term factor"){        // rules leading to either a number or a variable or NULL
        //comment("got expr");
        code(t.children[0]);
    }else if(t.rule == "factor ID" || t.rule == "factor NUM"){          // rule leading to loading of a num or variable
        comment("Load " + t.children[0].tokens[1] + " in $3");
        code(t.children[0]);
    }else if (t.rule == "factor LPAREN expr RPAREN"){                   // support not only a but (a) too
        code(t.children[1]);        
    }else if (t.rule == "expr expr PLUS term" ){                        // a + b
        comment("ADD");
        if(typeOf(t.children[0]) == "int" && typeOf(t.children[2]) == "int"){
            comment("Add ints");
            code(t.children[0]);
            push(3);
            code(t.children[2]);
            pop(5);
            add(3,5,3);
        } else if(typeOf(t.children[0]) == "int*" && typeOf(t.children[2]) == "int"){
            comment("Add int* + int");
            code(t.children[0]);
            push(3);
            code(t.children[2]);
            multl(3,3,4);
            pop(5);
            add(3,5,3);
        }else if(typeOf(t.children[0]) == "int" && typeOf(t.children[2]) == "int*"){
            comment("Add int + int*");
            code(t.children[0]);
            multl(3,3,4);
            push(3);
            code(t.children[2]);
            pop(5);
            add(3,5,3);
        }else {comment("ADD FAILED");
        }
    }else if (t.rule == "expr expr MINUS term"){                          // a - b
        if(typeOf(t.children[0]) == "int" && typeOf(t.children[2]) == "int"){
            comment("Subtract");
            code(t.children[0]);
            push(3);
            code(t.children[2]);
            pop(5);
            sub(3,5,3);
        } else if(typeOf(t.children[0]) == "int*" && typeOf(t.children[2]) == "int"){
            code(t.children[0]);
            push(3);
            code(t.children[2]);
            multl(3,3,4);
            pop(5);
            sub(3,5,3);
        }else if(typeOf(t.children[0]) == "int*" && typeOf(t.children[2]) == "int*"){
            code(t.children[0]);
            push(3);
            code(t.children[0]);
            pop(5);
            sub(3,5,3);
            div(3,3,2);
        }
    }else if (t.rule == "term term STAR factor"){                       // a * b
        comment("Multiply");
        code(t.children[0]);
        push(3);
        code(t.children[2]);
        pop(5);
        multl(3,5,3);
    }else if (t.rule == "term term SLASH factor"){                      // a / b
        comment("Divide");
        code(t.children[0]);
        push(3);
        code(t.children[2]);
        pop(5);
        div(3,5,3);        
    }else if(t.rule == "term term PCT factor"){                       // a % b
        comment("Mod");
        code(t.children[0]);
        push(3);
        code(t.children[2]);
        pop(5);
        mod(3,5,3);
    }else if (t.tokens[0] == "NUM"){                                    // got a numerical value, store and return in $3
        //comment("load" + t.tokens[1] + "into $3");
        load(3,t.tokens[1]);
    }else if(t.rule == "statements statements statement"){              // support statemetns
        code(t.children[0]);
        code(t.children[1]);
    }else if(t.rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){  // write to std out
        comment("Printing");
        code(t.children[2]);
        push(1);
        add(1,3,0);
        push(29);
        push(31);
        load(10,"print");
        jalr(10);
        pop(31);
        pop(29);
        pop(1);
        comment("End Printing");
    }else if(t.rule == "dcls dcls dcl BECOMES NUM SEMI"){           // Assignment of variables to NUM
        code(t.children[0]);
        comment("set " + t.children[1].children[1].tokens[1] + " to " + t.children[3].tokens[1]);
        load(3,t.children[3].tokens[1]);
        sw29(3,offset_of(t.children[1].children[1].tokens[1]));
    }else if(t.rule == "statement lvalue BECOMES expr SEMI"){       // Assignment of variables to NUM or other variables
        comment("BOOM");
        if(t.children[0].rule == "lvalue STAR factor"){     // in case of dereferencing
            //comment("set " + t.children[0].children[1].tokens[1] + " to **value at following address**");
            code(t.children[0].children[1]);        
            //add(5,3,0);
            push(3);
            code(t.children[2]);
            pop(5);
            sw(3,0,5);
        }else if(t.children[0].rule == "lvalue ID"){
            //comment("set " + t.children[0].children[1].tokens[1] + " to **following**"); // when lvalue is just ID
            code(t.children[2]);
            comment("set " + t.children[0].children[0].tokens[1]);
            sw29(3,offset_of(t.children[0].children[0].tokens[1]));
        }else{
            comment("&&&&&&&&&&&&&&&&&BROKE&&&&&&&&&&&&&&&&&&&&");
        }
    }else if(t.rule == "lvalue LPAREN lvalue RPAREN"){              // support (a) = b
        code(t.children[1]);
    }else if(t.tokens[0] == "test"){                                // comapre a & b (a<b,a>b,a==b,a!=b,a<=b,a>=b)
        comment("test start");
        comment("expr1");
        code(t.children[0]); //exprA
        push(3);
        comment("expr2");
        code(t.children[2]); //exprB
        pop(5);

        if(t.tokens[2] == "LT"){                // a < b
            comment("test expr1 < expr2");
            if(typeOf(t.children[0]) == "int"){            
                slt(3,5,3);
            }else {
                sltu(3,5,3);
            }
        }if(t.tokens[2] == "EQ"){               // a == b
            comment("test expr1 == expr2");
            push(6);                //**************************************************************can remove push,pop of 6,7 if i dont use them*****************************************
            push(7);                //**************************************************************still no harm in doing it************************************************************
            if(typeOf(t.children[0]) == "int"){
                slt(6,3,5);
                slt(7,5,3);
            }else {
                sltu(6,3,5);
                sltu(7,5,3);
            }
            add(3,6,7);
            sub(3,11,3);
            pop(7);
            pop(6);
        }if(t.tokens[2] == "NE"){               // a != b
            comment("test expr1 != expr2");
            push(6);
            push(7);
            if(typeOf(t.children[0]) == "int"){
                slt(6,3,5);
                slt(7,5,3);
            }else{
                sltu(6,3,5);
                sltu(7,5,3);
            }
            add(3,6,7);
            pop(7);
            pop(6);
        }if(t.tokens[2] == "LE"){               // a <= b
            comment("test expr1 <= expr2");
            push(6);
            push(7);
            if(typeOf(t.children[0]) == "int"){            
                slt(6,3,5);
                slt(7,5,3);
            }else{
                sltu(6,3,5);
                sltu(7,5,3);
            }
            add(3,7,6);
            sub(3,11,3); // same as a == b
            add(3,3,7);  // add 1 if a<b
            pop(7);
            pop(6);
        }if(t.tokens[2] == "GE"){               // a >= b
            comment("test expr1 >= expr2");
            push(6);
            push(7);
            if(typeOf(t.children[0]) == "int"){                        
                slt(6,5,3);
                slt(7,3,5);
            }else{
                sltu(6,5,3);
                sltu(7,3,5);
            }
            add(3,6,7);
            sub(3,11,3);    // same as a==b
            add(3,3,7);     // add 1 is a>b
            pop(7);
            pop(6);
        }if(t.tokens[2] == "GT"){
            comment("test expr1 > expr2");
            if(typeOf(t.children[0]) == "int"){
                slt(3,3,5);
            } else {
                sltu(3,3,5);
            }
        }
        comment("test end");
    }else if(t.rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"){      // While Loop
        int local_count = loopcounter;
        print("Loop" + s(local_count) + ":");
        code(t.children[2]);
        beq(3,0,"Done" +s(local_count));
        loopcounter++;
        comment("Loop_Statements" + s(local_count));
        code(t.children[5]);
        loopcounter--;
        beq(0,0,"Loop" +s(local_count));
        print("Done" + s(local_count) + ":");
        loopcounter++;
    }else if(t.rule == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"){       //If condition
        int local_count = ifcounter;
        comment("If" + s(local_count));
        code(t.children[2]);
        beq(3,0,"Else" + s(local_count));
        ifcounter++;
        comment("If_Statements" + s(local_count));
        code(t.children[5]);
        ifcounter--;
        beq(0,0,"EndIf" + s(local_count));
        print("Else" + s(local_count) + ":");
        ifcounter++;
        code(t.children[9]);
        ifcounter--;
        print("EndIf" + s(local_count) + ":");
        ifcounter++;
    }else if(t.rule == "dcls dcls dcl BECOMES NULL SEMI"){                  // Initialise pointer to NULL
        code(t.children[0]);
        comment("Initialise " + t.children[1].children[1].tokens[1] + " to NULL");
        add(3,11,0);
        sw29(3,offset_of(t.children[1].children[1].tokens[1]));
    }else if(t.rule == "factor NULL"){
        comment("Assign NULL");
        add(3,11,0);
    }else if(t.rule == "factor STAR factor"){
        comment("Get address of " + t.children[1].children[0].tokens[1]);
        code(t.children[1]);
        lw(3,0,3);
        comment("Set to address of " + t.children[1].children[0].tokens[1]);
    }else if(t.rule == "factor AMP lvalue"){
        if(t.children[1].rule == "lvalue ID"){
            comment("Set to " + t.children[1].children[0].tokens[1]);
            load(3,offset_of(t.children[1].children[0].tokens[1]));
            add(3,3,29);
        }else if(t.children[1].rule == "lvalue STAR factor"){
            comment("set to *something* to *somthing*");
            code(t.children[1].children[1]);
        }
        code(t.children[1]); 
    }else if(t.rule == "lvalue STAR factor"){
        code(t.children[1]);
    }else if(t.rule == "lvalue ID"){
        load(3,offset_of(t.children[0].tokens[1]));
        add(3,3,29);
    }else if(t.rule == "factor NEW INT LBRACK expr RBRACK"){                // Memory allocation by NEW
        comment("NEW Memory Alloc");
        //init();
        code(t.children[3]);
        add(1,3,0);
        call("new");
        bne(3,0, s(1));
        add(3,11,0);
    }else if(t.rule == "statement DELETE LBRACK RBRACK expr SEMI"){
        comment("DELETE Memory DeAlloc");
        code(t.children[3]);
        beq(3, 11, "skipDelete" + s(delcounter));
        add(1,3,0);
        call("delete");
        print("skipDelete" + s(delcounter) + ":");
        delcounter++;
    }else if(t.rule == "procedures procedure procedures"){
        comment("got procedures");
        code(t.children[0]);
        code(t.children[1]);
    }else if(t.rule == "factor ID LPAREN RPAREN"){
        comment("Calling Function: " + t.children[0].tokens[1]);
        push(29);
        push(31);
        load(10, "F" + t.children[0].tokens[1]);
        jalr(10);
        pop(31);
        pop(29);
        comment("Returned from Function: " + t.children[0].tokens[1]);
    }else if(t.rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
        procedure = t.children[1].tokens[1];
        comment("Defining Function: " + t.children[1].tokens[1]);
        print("F" + t.children[1].tokens[1] + ":");
        //add(29,30,0);      
        code(t.children[6]);
        comment("done dcls for " + t.children[1].tokens[1]);
        code(t.children[7]);
        comment("done statements for " + t.children[1].tokens[1]);
        code(t.children[9]);
        comment("done return for " + t.children[1].tokens[1]);
        jr(31);
    }
}


int main(){
    Tree ptree;
    string first_line;
    getline(cin,first_line);
    ptree = BuildNode(first_line);
    
    // Code Generation Begins
    
    prologue();                 // Beginning setup with standard registersconventions, imports  XXX and init XXX
    cout << ";Decalre local variables" << endl;
    BuildSymbolTable(ptree);    // Stores input parameters to wain in the stack and assigns an offset from $29 to each varaible in SymbolTable
    init();                     // Initliases the memory allocation data structure and stores either 0 or lenght of array in $2, depending on type of first paramet to wain
    CheckUndeclaredUsed(ptree); // Checks if a variable was used without declaration
    beq(0,0,"WAIN");
    cout << ";Code" << endl;;
    code(ptree);                // Main cooe genration function taking the top of parse tree and recursively generating code for selective children trees
    epilogue();                 // Restoring registers and jumping out of the program
    
    Get_Type(ptree);            // Type-checker to ensure consistency in data types
    if(!error){
        //PrintSymbolTable();
    }
}