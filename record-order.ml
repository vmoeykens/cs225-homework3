let se = ref 0;;

type rt = { a : int; b : int };;

let inc x = (se := (x + !se); !se);; 

{ a = inc 1; b = inc 2; };;
