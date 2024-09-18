module type Ring  = sig
  type t
  val zero : t
  val one : t
  val add : t-> t-> t
  val mul: t-> t-> t
  val compare : t-> t-> int 
  val to_string : t -> string
end


module type Matrix =  sig
  type elem 
  type t
  val create : int-> int -> t
  val identity : int -> t
  val from_rows: elem list list ->t
  val set : int -> int -> elem-> t-> t
  val get : int -> int -> t -> elem 
  val transpose : t-> t
  val add : t-> t-> t
  val mul : t-> t-> t
  val to_string : t-> string
end


module IntRing : Ring with type t = int = struct
  type t = int
  let zero = 0
  let one = 1
  let add = ( + )
  let mul = ( * )
  let compare = compare
  let to_string = string_of_int
end


module FloatRing : Ring with type t = float = struct
  type t = float 
  let zero = 0.0
  let one = 1.0
  let add = (+.)
  let mul = ( *. )
  let compare = compare
  let to_string = string_of_float
end


module type FIniteRing = sig
include Ring
val elem : t list
end


module BoolRIng : FIniteRing with type t = bool = struct
  type t = bool
  let zero = false
  let one = true
  let compare = Pervasives.compare
  let to_string = string_of_bool
  let add = (||)
  let mul = (&&)
  let elem = [false;true]
end


module SetRing (D : FIniteRing) : Ring with type t = D.t list = 
struct
  type t = D.t list
  let zero = []
  let one = D.elem
  let compare a b =
    let a = List.sort D.compare a in
    let b = List.sort D.compare b in
    let rec impl l1 l2 = match l1, l2 with
      | [],_ | _,[] -> (List.length l1) - (List.length l2)
      | x::xs, y::ys -> let c = D.compare x y in
        if c <> 0 then c else impl xs ys
    in
    impl a b

    
  let to_string l = "{" ^ (String.concat ", " (List.map D.to_string l)) ^ "}"
  let add a b = List.sort_uniq D.compare (a @ b)
  let mul a b = List.filter (fun x -> List.find_opt (fun y -> D.compare x y = 0) b <> None) a
end







module DenseMatrix (T : Ring) : Matrix with type elem = T.t = struct
  type elem = T.t
  type t = T.t list list

  let create rows cols =
   List.init rows (fun _ -> List.init cols (fun _ -> T.zero))  
  
  let identity size = 
    List.init size (fun r -> List.init size (fun c -> if r = c then T.one 
    else T.zero))



  let from_rows l = l


  let to_string m =
    List.map (fun row -> List.map T.to_string row |> String.concat " ") m |> String.concat "\n"



  let set row col value m = List.mapi (fun r x -> if r <> row then x else List.mapi (fun c x -> if c <> col then x else value) x) m


  let get row col m =
    if row < 0 || row >= List.length m || col < 0 || col >= (List.length (List.hd m)) then raise Invalid_operation;
    List.nth (List.nth m row) col


  let transpose m = 
    let cols = List.length (List.hd m ) in 
    let new_m =  List.init cols (fun x -> []) in 
    List.fold_left (fun acc row -> 
      List.fold_left (fun acc col -> 
        match acc with 
        |[] -> failwith "unreachable"
        | x :: xs -> xs @ [col :: x])
        acc row ) new_m m |> List.map List.rev 


        let add a b = List.map2 (fun r1 r2 -> List.map2 T.add r1 r2) a b




    let mul a b =
    let arows = List.length a in
    let brows = List.length b in
    let acols = List.length (List.hd a) in
    let bcols = List.length (List.hd b) in
    if acols <> brows then raise Invalid_operation;
    let b = transpose b in
    let compute row col =
      List.fold_left2 (fun acc r c -> T.add acc (T.mul r c)) 
       T.zero (List.nth a row) (List.nth b col)
    in
    List.init arows (fun r -> List.init bcols (fun c -> compute r c))
  
end


module SparseMatrix (X : Ring) : Matrix with type elem = X.t struct
  type elem = X.t
  type t = { cells : (int * int * X.t) list; nrows : int; ncols : int }

  let create rows cols =  { cells=[]; nrows=rows; ncols=cols }

  let identity size = { cells=List.init size (fun s -> s,s,T.one); nrows=size; ncols=size }


end