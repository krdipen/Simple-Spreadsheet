let _ =
    try
        let filename = Sys.argv.(4) in
        let file_handle = open_in filename in
        let lexbuf = Lexing.from_channel file_handle in
        while true do
            Parser.main Lexer.token lexbuf;
            flush stdout
        done
    with Lexer.Eof ->
    exit 0
