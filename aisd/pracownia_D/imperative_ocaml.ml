type avl = {
	mutable height : int;
	mutable v : int;
	mutable lt : avl option;
	mutable rt : avl option;
}
type cmp  = Less | Equal | More

let avl_empty () = None
let rec find_max t = match t.rt with
	| None 		-> t.v
	| Some t' 	-> find_max t'
let depth t = match t with
	| None 		-> 0
	| Some t'	-> t'.height
let icomp x y =
	if x < y then Less
 	else if x == y then Equal
 	else More
let max1 x y = (max x y) + 1
let fromSome (Some y) = y

let opt_min x y = match y with
	| None 		-> Some x
	| Some v 	-> Some (min x v)
let rec _upper t' x m = match t' with
	| None 			-> m
	| Some t 		-> (match (icomp x t.v) with
		| Less  -> _upper t.lt x (opt_min t.v m)
		| Equal -> Some x
		| More  -> _upper t.rt x m)
let upper t x = _upper t x None

let opt_max x y = match y with
	| None 		-> Some x
	| Some v 	-> Some (max x v)
let rec _lower t' x m = match t' with
	| None 			-> m
	| Some t -> (match (icomp x t.v) with
		| Less  -> _lower t.lt x m
		| Equal -> Some x
		| More  -> _lower t.rt x (opt_max t.v m))
let lower t x = _lower t x None

let rotate_right p =
	let pv = p.v
	and child = fromSome p.lt
	and gamma = p.rt in
	let alfa = child.lt
	and beta = child.rt
	in begin
		p.v <- child.v;
		child.v <- pv;

		child.lt <- beta;
		child.rt <- gamma;
		child.height <- max1 (depth beta) (depth gamma);

		p.lt <- alfa;
		p.rt <- Some child;
		p.height <- max1 (depth alfa) (depth (Some child))
	end

let rotate_left p =
	let pv = p.v
	and alfa = p.lt
	and child = fromSome p.rt in
	let beta = child.lt
	and gamma = child.rt
	in begin
		p.v <- child.v;
		child.v <- pv;

		child.lt <- alfa;
		child.rt <- beta;
		child.height <- max1 (depth alfa) (depth beta);

		p.lt <- Some child;
		p.rt <- gamma;
		p.height <- max1 (depth (Some child)) (depth gamma)
	end

let balanceL t = match ((depth (fromSome t.lt).lt) - (depth (fromSome t.lt).rt)) with
	| -1 		-> begin
		rotate_left (fromSome t.lt);
		t.height <- max1 (depth t.lt) (depth t.rt);
		rotate_right t
	end
	| 1 | 0 	-> rotate_right t

let balanceR t = match ((depth (fromSome t.rt).lt) - (depth (fromSome t.rt).rt)) with
	| 1 		-> begin
		rotate_right (fromSome t.rt);
		t.height <- max1 (depth t.lt) (depth t.rt);
		rotate_left t
	end
	| -1 | 0 	-> rotate_left t

let balanceP t' = match t' with
	| None 		-> ()
	| Some t 	-> (match ((depth t.lt) - (depth t.rt)) with
			| -2 			-> balanceR t
			| -1 | 0 | 1 	-> ()
			| 2				-> balanceL t
	)
(* let balanceP t = t *)

let rec insert t' x = match t' with
	| None 		-> Some { height = 1; v = x; lt = None; rt = None }
	| Some t 	-> (match (icomp x t.v) with
		| Less 	->
			let new_lt = insert t.lt x
			in begin
				t.height <- max1 (depth new_lt) (depth t.rt);
				t.lt <- new_lt;
				balanceP (Some t);
				Some t
			end
		| Equal -> t'
		| More  ->
			let new_rt = insert t.rt x
			in begin
				t.height <- max1 (depth t.lt) (depth new_rt);
				t.rt <- new_rt;
				balanceP (Some t);
				Some t
			end
	)

let rec delete t' x = match t' with
	| None		-> None
	| Some t	-> (match (icomp x t.v) with
		| Less 		-> (match (delete t.lt x) with
				| None 			-> None
				| Some new_lt 	-> begin
					t.lt <- new_lt;
					t.height <- max1 (depth new_lt) (depth t.rt);
					balanceP (Some t);
					Some (Some t)
				end)
		| Equal 	-> Some (delete_ t)
		| More 		-> (match (delete t.rt x) with
				| None 			-> None
				| Some new_rt 	-> begin
					t.rt <- new_rt;
					t.height <- max1 (depth t.lt) (depth new_rt);
					balanceP (Some t);
					Some (Some t)
				end)
	)
and delete_ t = match (t.lt,t.rt) with
	| (None, None)	-> None
	| (None, _)		-> t.rt
	| (_, None)		-> t.lt
	| (_, _)		->
		let lt_max = find_max (fromSome t.lt)
		in let new_lt = fromSome (delete t.lt lt_max)
		in begin
			t.lt <- new_lt;
			t.v <- lt_max;
			t.height <- max1 (depth new_lt) (depth t.rt);
			balanceP (Some t);
			Some t
		end

(* Wczytywanie *)

let asd = ref 100 and pos = ref 100;;
let buf = String.create 100 ;;
let getbuf () = input stdin buf 0 100;;
let gettok () = if !pos >= !asd then
        (if !asd = 0 then char_of_int 0
        else (asd := getbuf (); if !asd = 0 then char_of_int 0 else (pos := 1; buf.[0] )))
        else ( incr pos; buf.[!pos-1] );;

let mread_int () =
    let v = ref 0 and tok = ref '#' and negative = ref false in
	begin
        while !tok < '0' || !tok == '-'
        do
        		if !tok == '-' then negative := true;
                tok := gettok ();

        done;
        v := int_of_char(!tok) - 48;
        tok := gettok ();
        while !tok >= '0'
        do
                v := !v * 10;
                v := !v + int_of_char !tok - 48;
                tok := gettok ()
        done;
        if !negative then -(!v) else !v
	end

let mread_cmd () = let x = gettok () in (gettok(); x)


let cmd_upper t x = match (upper t x) with
	| None 		-> begin
		print_string "BRAK";
		print_newline ();
		t
	end
	| Some y 	-> begin
		print_int y;
		print_newline ();
		t
	end

let cmd_lower t x = match (lower t x) with
	| None 		-> begin
		print_string "BRAK";
		print_newline ();
		t
	end
	| Some y 	-> begin
		print_int y;
		print_newline ();
		t
	end

let cmd_delete t x = (match (delete t x) with
	| None 		-> begin
		print_string "BRAK";
		print_newline ();
		t
	end
	| Some t2	-> begin
		print_string "OK";
		print_newline ();
		balanceP t2;
		t2
	end)

let execute_command t c x = match c with
			| 'I'	-> insert t x
			| 'D'	-> cmd_delete t x
			| 'U'	-> cmd_upper t x
			| 'L'	-> cmd_lower t x

(* let read_command t =
	let line = read_line ()
	in let cmd = line.[0]
	in let x = int_of_string (String.sub line 2 ((String.length line) - 2))
	in execute_command t cmd x *)

let read_command t =
	let cmd = mread_cmd ()
	in let x = mread_int ()
	in (execute_command t cmd x)
	(* in (Printf.printf "Wczytałem [%c, %d]" cmd x;execute_command t cmd x) *)

let rec read_commands n t =
	if n == 0 then ()
	else let new_t = read_command t
			in read_commands (n-1) new_t

let main () =
	let n = mread_int ()
	in read_commands n (avl_empty ())

let _ = main ()