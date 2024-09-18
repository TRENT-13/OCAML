module type Printable = sig
  type t
  val print : t -> unit
  val length : t -> int
  end


module PrintableString = struct
  type t = string
  let print x = print_string x 
  let length  = String.length 
end



module PrintableInt = struct
  type t= int
  let print  = print_int 
  let length x = String.length (string_of_int x)
end

module PrintableList (X : Printable) = struct
  type t = X.t list
  let print = List.iter (fun t -> X.print t;
  print_string "\n")

  let length = List.fold_left (fun a x -> a+ X.length x + 1) 0 
end


module PrintablePair(A:Printable) (B: Printable) = struct
  type t = A.t * B.t
  let print (a,b) = A.print a;
  print_string " ";
  B.print b
  let length (a,b) = A.length a + B.length b
end


module PrintableWarnings (Warning:Printable) (Location:Printable) = struct
module P = PrintablePair(Location)(Warning)
include PrintableList(P)
end



module StringWarnings =
PrintableWarnings(StringPrintable)(IntPrintable)
let warn () = StringWarnings.print [
(0, "Error!");
(42, "No error!")
]