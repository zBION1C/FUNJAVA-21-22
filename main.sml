use "syntax.sml";
use "parser/parser.sml";
use "type-checker/type-checker.sml";
use "interpreter/interpreter.sml";

Control.Print.printLength := 500;
Control.Print.printDepth := 500;

fun readProgram (infile : string) =  
  let
      val ins = TextIO.openIn infile 
      fun loop indata = 
          case TextIO.inputLine indata of 
              SOME line => line ^ loop indata 
            | NONE      => ""
      val result = loop ins
  in 
     TextIO.closeIn ins;
     result
  end;

val file = String.translate (fn c => if c = #"\n" then "" else str c) ("esempi/" ^ valOf(TextIO.inputLine TextIO.stdIn));
val P = readProgram file;
val tknzd_prog = tokenize (String.tokens delimitator (String.translate replace P));
val prog = parse(tknzd_prog);
val tipo = type_check(prog);
val valore = eval(prog);