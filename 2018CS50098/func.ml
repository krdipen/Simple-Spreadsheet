type data = NUM of float | NA
type sheet = data array array
type range = int * int * int * int
type index = int * int
type constant = float

(* count the cells in the range with valid entries i.e. with float rather than NA *)
let rec full_count (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) then (
                let count = ref 0.0 in
                for j = x1 to x2 do
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> count := (!count +. 1.0)
                        |   NA     -> count := !count;
                    done;
                done;
                s.(x0).(y0) <- NUM(!count); ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* count of valid entries per row in the given range into the column starting from the specified cell *)
let rec row_count (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let count = ref 0.0 in
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> count := (!count +. 1.0)
                        |   NA     -> count := !count;
                    done;
                    s.(x0+(!x)).(y0) <- NUM(!count);
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* count of valid entries per column in the given range into the row starting from the specified cell *)
let rec col_count (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let y = ref 0 in
                for k = y1 to y2 do
                    let count = ref 0.0 in
                    for j = x1 to x2 do
                        match s.(j).(k) with
                            NUM(v) -> count := (!count +. 1.0)
                        |   NA     -> count := !count;
                    done;
                    s.(x0).(y0+(!y)) <- NUM(!count);
                    y := (!y + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* sum of entries of cells in the given range into the specified cell *)
let rec full_sum (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) then (
                let sum = ref 0.0 in
                for j = x1 to x2 do
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> sum := (!sum +. v)
                        |   NA     -> sum := !sum;
                    done;
                done;
                s.(x0).(y0) <- NUM(!sum); ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* sum of entries of cells per row in the given range into the column starting from the specified cell *)
let rec row_sum (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let sum = ref 0.0 in
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> sum := (!sum +. v)
                        |   NA     -> sum := !sum;
                    done;
                    s.(x0+(!x)).(y0) <- NUM(!sum);
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* sum of entries of cells per column in the given range into the row starting from the specified cell *)
let rec col_sum (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let y = ref 0 in
                for k = y1 to y2 do
                    let sum = ref 0.0 in
                    for j = x1 to x2 do
                        match s.(j).(k) with
                            NUM(v) -> sum := (!sum +. v)
                        |   NA     -> sum := !sum;
                    done;
                    s.(x0).(y0+(!y)) <- NUM(!sum);
                    y := (!y + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* average of entries of cells in the given range into the specified cell *)
let rec full_avg (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) then (
                let count = ref 0.0 in
                let sum = ref 0.0 in
                for j = x1 to x2 do
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> ( sum := (!sum +. v) ; count := (!count +. 1.0) )
                        |   NA     -> ( sum := !sum ; count := !count ) ;
                    done;
                done;
                s.(x0).(y0) <- NUM(!sum /. !count); ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* average of entries of cells per row in the given range into the column starting from the specified cell *)
let rec row_avg (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let count = ref 0.0 in
                    let sum = ref 0.0 in
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> ( sum := (!sum +. v) ; count := (!count +. 1.0) )
                        |   NA     -> ( sum := !sum ; count := !count ) ;
                    done;
                    s.(x0+(!x)).(y0) <- NUM(!sum /. !count);
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* sum of entries of cells per column in the given range into the row starting from the specified cell *)
let rec col_avg (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let y = ref 0 in
                for k = y1 to y2 do
                    let count = ref 0.0 in
                    let sum = ref 0.0 in
                    for j = x1 to x2 do
                        match s.(j).(k) with
                            NUM(v) -> ( sum := (!sum +. v) ; count := (!count +. 1.0) )
                        |   NA     -> ( sum := !sum ; count := !count ) ;
                    done;
                    s.(x0).(y0+(!y)) <- NUM(!sum /. !count);
                    y := (!y + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* min of entries of cells in the given range into the specified cell *)
let rec full_min (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) then (
                let min = ref max_float in
                for j = x1 to x2 do
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> if v < !min then min := v else min := !min
                        |   NA     -> min := !min ;
                    done;
                done;
                s.(x0).(y0) <- NUM(!min); ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* min of entries of cells per row in the given range into the column starting from the specified cell *)
let rec row_min (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let min = ref max_float in
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> if v < !min then min := v else min := !min
                        |   NA     -> min := !min ;
                    done;
                    s.(x0+(!x)).(y0) <- NUM(!min);
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* min of entries of cells per column in the given range into the row starting from the specified cell *)
let rec col_min (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let y = ref 0 in
                for k = y1 to y2 do
                    let min = ref max_float in
                    for j = x1 to x2 do
                        match s.(j).(k) with
                            NUM(v) -> if v < !min then min := v else min := !min
                        |   NA     -> min := !min ;
                    done;
                    s.(x0).(y0+(!y)) <- NUM(!min);
                    y := (!y + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* max of entries of cells in the given range into the specified cell *)
let rec full_max (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) then (
                let max = ref (-1.0 *. max_float) in
                for j = x1 to x2 do
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> if v > !max then max := v else max := !max
                        |   NA     -> max := !max ;
                    done;
                done;
                s.(x0).(y0) <- NUM(!max); ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* max of entries of cells per row in the given range into the column starting from the specified cell *)
let rec row_max (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let max = ref (-1.0 *. max_float) in
                    for k = y1 to y2 do
                        match s.(j).(k) with
                            NUM(v) -> if v > !max then max := v else max := !max
                        |   NA     -> max := !max ;
                    done;
                    s.(x0+(!x)).(y0) <- NUM(!max);
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* max of entries of cells per column in the given range into the row starting from the specified cell *)
let rec col_max (s:sheet) (r:range) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let y = ref 0 in
                for k = y1 to y2 do
                    let max = ref (-1.0 *. max_float) in
                    for j = x1 to x2 do
                        match s.(j).(k) with
                            NUM(v) -> if v > !max then max := v else max := !max
                        |   NA     -> max := !max ;
                    done;
                    s.(x0).(y0+(!y)) <- NUM(!max);
                    y := (!y + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* adds a constant to the contents of each cell in the selected cell range *)
let rec add_const (s:sheet) (r:range) (c:constant) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v +. c)
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* subtracts a constant from the contents of each cell in the selected cell range *)
let rec subt_const (s:sheet) (r:range) (c:constant) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v -. c)
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* multiplies the contents of each cell in the selected cell range by a constant *)
let rec mult_const (s:sheet) (r:range) (c:constant) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v *. c)
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range and index\n") ; (* handeled baised input *)
                s;;

(* divides the contents of each cell in the selected cell range by a constant *)
let rec div_const (s:sheet) (r:range) (c:constant) (i:index) :sheet = match r with
    (x1,y1,x2,y2) -> match i with
    (x0,y0) ->  if c!=0.0 && x2>=x1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v /. c)
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct range, constant and index\n") ; (* handeled baised input *)
                s;;

(* adds the cell contents for each corresponding pair of cells in two selected cell ranges *)
let rec add_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = match r1 with
    (x1,y1,x2,y2) -> match r2 with
    (a1,b1,a2,b2) -> match i with
    (x0,y0) ->  if x2-x1 == a2-a1 && y2-y1 == b2-b1 && x2>=x1 && a2>=a1 && b2>=b1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && a1<(Array.length s) && a2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && b1<(Array.length s.(0)) && b2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v1) -> ( match s.(a1+(!x)).(b1+(!y)) with
                                               NUM(v2) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v1 +. v2)
                                           |   NA      -> () ; )
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct ranges and index\n") ; (* handeled baised input *)
                s;;

(* performs a subtraction on the cell contents for each corresponding pair if cells in two selected cell ranges *)
let rec subt_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = match r1 with
    (x1,y1,x2,y2) -> match r2 with
    (a1,b1,a2,b2) -> match i with
    (x0,y0) ->  if x2-x1 == a2-a1 && y2-y1 == b2-b1 && x2>=x1 && a2>=a1 && b2>=b1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && a1<(Array.length s) && a2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && b1<(Array.length s.(0)) && b2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v1) -> ( match s.(a1+(!x)).(b1+(!y)) with
                                               NUM(v2) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v1 -. v2)
                                           |   NA      -> () ; )
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct ranges and index\n") ; (* handeled baised input *)
                s;;

(* multiplies the cell contents for each corresponding pair of cells in two selected cell ranges *)
let rec mult_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = match r1 with
    (x1,y1,x2,y2) -> match r2 with
    (a1,b1,a2,b2) -> match i with
    (x0,y0) ->  if x2-x1 == a2-a1 && y2-y1 == b2-b1 && x2>=x1 && a2>=a1 && b2>=b1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && a1<(Array.length s) && a2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && b1<(Array.length s.(0)) && b2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v1) -> ( match s.(a1+(!x)).(b1+(!y)) with
                                               NUM(v2) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v1 *. v2)
                                           |   NA      -> () ; )
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct ranges and index\n") ; (* handeled baised input *)
                s;;

(* performs a division on the cell contents for each corresponding pair  of cells in two selected cell ranges *)
let rec div_range (s:sheet) (r1:range) (r2:range) (i:index) :sheet = match r1 with
    (x1,y1,x2,y2) -> match r2 with
    (a1,b1,a2,b2) -> match i with
    (x0,y0) ->  if x2-x1 == a2-a1 && y2-y1 == b2-b1 && x2>=x1 && a2>=a1 && b2>=b1 && y2>=y1 && x1<(Array.length s) && x2<(Array.length s) && a1<(Array.length s) && a2<(Array.length s) && x0<(Array.length s) && x0+x2-x1<(Array.length s) && y1<(Array.length s.(0)) && y2<(Array.length s.(0)) && b1<(Array.length s.(0)) && b2<(Array.length s.(0)) && y0<(Array.length s.(0)) && y0+y2-y1<(Array.length s.(0)) then (
                let x = ref 0 in
                for j = x1 to x2 do
                    let y = ref 0 in
                    for k = y1 to y2 do
                        ( match s.(j).(k) with
                              NUM(v1) -> ( match s.(a1+(!x)).(b1+(!y)) with
                                               NUM(v2) -> s.(x0+(!x)).(y0+(!y)) <- NUM(v1 /. v2)
                                           |   NA      -> () ; )
                          |   NA     -> () ) ;
                        y := (!y + 1);
                    done;
                    x := (!x + 1);
                done; ) else (print_string "Please give correct ranges and index\n") ; (* handeled baised input *)
                s;;
