{
open Parser
exception Eof
}
rule token = parse
    |   "COUNT"
    |   "ROWCOUNT"
    |   "COLCOUNT"
    |   "SUM"
    |   "ROWSUM"
    |   "COLSUM"
    |   "AVG"
    |   "ROWAVG"
    |   "COLAVG"
    |   "MIN"
    |   "ROWMIN"
    |   "COLMIN"
    |   "MAX"
    |   "ROWMAX"
    |   "COLMAX"                                                          as lxm { UNARY lxm }
    |   "ADD"
    |   "SUBT"
    |   "MULT"
    |   "DIV"                                                             as lxm { BINARY lxm }
    |   ['-''+']?(['0'-'9']+['.']?['0'-'9']*|['0'-'9']*['.']?['0'-'9']+)  as lxm { FLOAT (float_of_string lxm) }
    |   '+'                                                                      { PLUS }
    |   '-'                                                                      { MINUS }
    |   '*'                                                                      { STAR }
    |   '/'                                                                      { SLASH }
    |   '('                                                                      { LPAREN }
    |   ')'                                                                      { RPAREN }
    |   '['                                                                      { LBRACK }
    |   ']'                                                                      { RBRACK }
    |   ','                                                                      { COMMA }
    |   ':'                                                                      { COLON }
    |   ":="                                                                     { ASSIGN }
    |   ';'                                                                      { SEMI }
    |   [' ''\t''\n']+                                                           { token lexbuf }
    |   [^' ''\t''\n''+''-''*''/''('')''['']'','':'';']+                         { INVALID }
    |   eof                                                                      { raise Eof }
