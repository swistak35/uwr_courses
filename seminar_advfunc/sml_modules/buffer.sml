structure LastNBuffer : LASTNBUFFER =
struct

    (* zero (0 <= zero < size data) is the zero element. *)
    type 'a buffer = { zero : int, data : 'a Array.array } ref

    fun buffer (n, x) = ref { zero = 0, data = Array.array(n, x) }

    fun rotate_right (r as ref { zero, data }) = 
        r := { zero = (zero - 1) mod (Array.length data), data = data }

    fun push_front (r, e) =
        let in
            rotate_right r;
            Array.update(#data (!r), #zero (!r), e)
        end

    fun sub (ref {zero, data}, x) = Array.sub(data, (zero + x) mod Array.length data)
    fun update (ref {zero, data}, x, e) = Array.update(data, (zero + x) mod Array.length data, e)
end
