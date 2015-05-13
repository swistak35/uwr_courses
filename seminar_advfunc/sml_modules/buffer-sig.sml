  (* The LastNBuffer is an efficient fixed-size (N elements) buffer that
   stores (only) the last N elements pushed into it. Pushing is
   constant time and accessing any element by index is also constant
   time. (It is implemented as a circular array of size N.) *)
signature LASTNBUFFER =
sig

    type 'a buffer

    (* lastnbuffer n elt
       Creates a buffer of size n with n copies of elt in it. *)
    val buffer : int * 'a -> 'a buffer

    (* Element zero. An element falls off the back. *)
    val push_front : 'a buffer * 'a -> unit

    val sub : 'a buffer * int -> 'a
    val update : 'a buffer * int * 'a -> unit
end
