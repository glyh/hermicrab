include Nice_parser.Make(struct
  type result = Ast.expr
  type token = Menhir_parser.token
  exception ParseError = Menhir_parser.Error
  let parse = Menhir_parser.program_eof
  include Lexer
end)

(*===========================================================================*)
(* TESTS                                                                     *)
(* https://dune.readthedocs.io/en/stable/tests.html#inline-expectation-tests *)
(* https://github.com/smolkaj/nice-parser/blob/master/example/parser.ml      *)
(*===========================================================================*)

let%test_module _ = (module struct
  
  let () = Printexc.record_backtrace false

  let%expect_test "single-line-comment" =

    parse_string {|
      echo 1 #hello I'm here
      echo 2 #wow
    |}
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{| (MBlock ((Command echo ((Just 1)) ()) (Command echo ((Just 2)) ()))) |}]

  let%expect_test "nestable-comments" =
    parse_string {|
      #| hello I'm here with #|nestable|# comments |#
      echo 1
    |}
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{| (MBlock ((Command echo ((Just 1)) ()))) |}]

  let%expect_test "cmd-simple" =
    parse_string "pwd"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{| (MBlock ((Command pwd () ()))) |}]

  let%expect_test "exp-int" =
    parse_string "= 1"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{| (MBlock ((Val (Int 1)))) |}]

  let%expect_test "exp-unit-nested" =
    parse_string "= ((((((()))))))"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{| (MBlock ((Val Unit))) |}]

  let%expect_test "exp-string" =
    parse_string "= 'This ACTUALLY works!'"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|  (MBlock ((Val (Str "This ACTUALLY works!"))))  |}]

  let%expect_test "exp-simple" =
    parse_string "= 1 + 1"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock ((Binary Add (Val (Int 1)) (Val (Int 1)))))
    |}]

  let%expect_test "exp-complex" =
    parse_string "(1 + 1 * 3 shl 3 bor 9 bxor 100 and '1' ++ '2' == '12')"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock
       ((Binary LAnd
         (Binary BOr
          (Binary BShiftL
           (Binary Add (Val (Int 1)) (Binary Mul (Val (Int 1)) (Val (Int 3))))
           (Val (Int 3)))
          (Binary BXor (Val (Int 9)) (Val (Int 100))))
         (Binary Eq (Binary SConcat (Val (Str 1)) (Val (Str 2))) (Val (Str 12))))))
    |}]

  let%expect_test "simple cmd" =
    parse_string "test -d my.zip && 7z x my.zip || echo no zip file"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
        (MBlock
         ((Binary OrElse
           (Binary AndThen (Command test ((Just -d) (Just my.zip)) ()) 
            (Command 7z ((Just x) (Just my.zip)) ()))
           (Command echo ((Just no) (Just zip) (Just file)) ()))))
    |}]

  let%expect_test "different word types" =
    parse_string "ls 1 2 3 'some string' -999 -la"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock
       ((Command ls
         ((Just 1) (Just 2) (Just 3) (Just "some string") (Just -999) (Just -la))
         ())))
    |}]


  let%expect_test "weird cmds" =
    parse_string "ls && 7z && mkfs.ntfs && x86_64-pc-linux-gnu-c++-11"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
        (MBlock
         ((Binary AndThen
           (Binary AndThen (Binary AndThen (Command ls () ()) (Command 7z () ()))
            (Command mkfs.ntfs () ()))
           (Command x86_64-pc-linux-gnu-c++-11 () ()))))
    |}]

  let%expect_test "if" =
    parse_string "if (1 + 1 == 2) { echo yes } else { echo world ends. }"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
        (MBlock
         ((If (Binary Eq (Binary Add (Val (Int 1)) (Val (Int 1))) (Val (Int 2)))
           (Block ((Command echo ((Just yes)) ())))
           (Block ((Command echo ((Just world) (Just ends.)) ()))))))
    |}]

  let%expect_test "assign" =
    parse_string "a = 1 + 1"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock ((Assign a (Binary Add (Val (Int 1)) (Val (Int 1))) false)))
    |}]

  let%expect_test "expr as word arg" =
    parse_string "echo [1 + 1]"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock ((Command echo ((Exp (Binary Add (Val (Int 1)) (Val (Int 1))))) ())))
    |}]

  let%expect_test "semicol as line delimiter" =
    parse_string "ls; pwd; echo 1"
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock
       ((Command ls () ()) (Command pwd () ()) (Command echo ((Just 1)) ())))
    |}]

  let%expect_test "templated string" =
    parse_string {|hey (""{1 + 1}"")|}
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock
       ((Command hey ()
         ((Binary SConcat (Val (Str ""))
           (Binary SConcat
            (Command stringify () ((Binary Add (Val (Int 1)) (Val (Int 1)))))
            (Val (Str ""))))))))
    |}]

  let%expect_test "multi string" =
    parse_string {|
      hey (
        \\\ hello
        \\\{ 1 + 1 }
        \\\ world
      )
    |}
    |> Printf.printf !"%{sexp:Ast.expr}";
    [%expect{|
      (MBlock
       ((Command hey ()
         ((Binary SConcat
           (Binary SConcat (Val (Str " hello"))
            (Binary Add (Val (Int 1)) (Val (Int 1))))
           (Val (Str " world")))))))
    |}]

end)
