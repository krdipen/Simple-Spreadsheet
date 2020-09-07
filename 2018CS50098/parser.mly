%{
open Func ;;
type exp =  A of string * (int * int * int * int)
        |   B of string * (int * int * int * int) * (int * int * int * int)
        |   C of string * (int * int * int * int) * float ;;
let m = (int_of_string Sys.argv.(2)) ;;
let n = (int_of_string Sys.argv.(3)) ;;
let s0 = Array.make_matrix m n NA  ;;
let rec print_sheet (s:sheet) :unit =   for i=0 to ((Array.length s) - 1) do
                                            for j=0 to ((Array.length s.(i)) -1) do
                                                ( match s.(i).(j) with
                                                      NUM(v) -> print_float v
                                                  |   NA     -> print_string "N/A" ) ;
                                                print_string " " ;
                                            done ;
                                            print_newline() ;
                                        done ;
                                        print_newline() ;
                                        print_newline() ;;
let rec make_row (v:string list) (j:int) (k:int) :unit = match v with
        [] -> ()
    |   x :: xs -> if k == n then () else (if (String.length x) > 0 then s0.(j).(k) <- NUM(float_of_string x) else () ; make_row xs j (k+1)) ;;
let _ =
    try
        let in_stream = open_in Sys.argv.(1) in
        for i=0 to (m-1) do
            let line = input_line in_stream in
            let split = Str.split_delim (Str.regexp ",") in
            let values = split line in
            make_row values i 0 ;
        done;
        close_in in_stream;
        print_newline() ;
        print_newline() ;
        print_sheet s0;
    with e ->
        Printf.printf "File not found!";
        raise e ;;
%}
%token <float> FLOAT
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN LBRACK RBRACK
%token COMMA COLON ASSIGN SEMI
%token <string> UNARY
%token <string> BINARY
%token INVALID
%start main
%type <unit> main
%%
main: T {print_sheet s0;} ;
T : I ASSIGN E SEMI {match $3 with
                    A("COUNT",r)      -> full_count s0 r $1
                |   A("ROWCOUNT",r)   -> row_count s0 r $1
                |   A("COLCOUNT",r)   -> col_count s0 r $1
                |   A("SUM",r)        -> full_sum s0 r $1
                |   A("ROWSUM",r)     -> row_sum s0 r $1
                |   A("COLSUM",r)     -> col_sum s0 r $1
                |   A("AVG",r)        -> full_avg s0 r $1
                |   A("ROWAVG",r)     -> row_avg s0 r $1
                |   A("COLAVG",r)     -> col_avg s0 r $1
                |   A("MIN",r)        -> full_min s0 r $1
                |   A("ROWMIN",r)     -> row_min s0 r $1
                |   A("COLMIN",r)     -> col_min s0 r $1
                |   A("MAX",r)        -> full_max s0 r $1
                |   A("ROWMAX",r)     -> row_max s0 r $1
                |   A("COLMAX",r)     -> col_max s0 r $1
                |   B("ADD",r1,r2)    -> add_range s0 r1 r2 $1
                |   B("SUBT",r1,r2)   -> subt_range s0 r1 r2 $1
                |   B("MULT",r1,r2)   -> mult_range s0 r1 r2 $1
                |   B("DIV",r1,r2)    -> div_range s0 r1 r2 $1
                |   C("ADD",r,c)      -> add_const s0 r c $1
                |   C("SUBT",r,c)     -> subt_const s0 r c $1
                |   C("MULT",r,c)     -> mult_const s0 r c $1
                |   C("DIV",r,c)      -> div_const s0 r c $1
                |   _                 -> s0 ;} ;
E : UNARY R {A($1,$2);}
  | BINARY R R {B($1,$2,$3);}
  | BINARY N R {C($1,$3,$2);}
  | BINARY R N {C($1,$2,$3);}
  | BINARY I R {match $2 with (x0,y0) -> ( match s0.(x0).(y0) with NUM(v) -> C($1,$3,v) | NA -> C($1,$3,1.0); );}
  | BINARY R I {match $3 with (x0,y0) -> ( match s0.(x0).(y0) with NUM(v) -> C($1,$2,v) | NA -> C($1,$2,1.0); );} ;
R : LPAREN I COLON I RPAREN {match $2 with (x1,y1) -> match $4 with (x2,y2) -> (x1,y1,x2,y2);} ;
I : LBRACK N COMMA N RBRACK {(int_of_float $2,int_of_float $4);} ;
N : N PLUS N {$1+.$3;}
  | N MINUS N {$1-.$3;}
  | N STAR N {$1*.$3;}
  | N SLASH N {$1/.$3;}
  | FLOAT {$1;} ;
%%
