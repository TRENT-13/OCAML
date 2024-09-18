type  'a tlist = T of ('a * 'a tlist) list

let rec extract_min = function
 | T []-> None, T[]
 | T ((a,t)::list)-> (match extract_min t with
 | None, _-> Some a, T list
 | Some a',t'-> Some a, T((a',t')::list)
 )



 let verify t = let rec check a = function
 | T []-> Some a
 | T ((a',t)::list)-> if a < a' then
 (match check a' t with
 | Some a''-> check a'' (T list)
 | None-> None
 ) else None in
 match t with
 | T[]-> true
 | T ((a,t)::list)-> (match check a t with
 | None-> false
 | Some a-> (match check a (T list) with
 | None-> false
 | Some _-> true)
 )



let rec insert t a = match t with 
| T [] -> T[(a,T[])]
| T [a',t'] -> (match compare a a' with
  | -1 -> T [a, insert t' a']
  |0 -> t
  | 1 -> T[a', insert t' a]
)
| T((a1,t1)::(a2,t2)::list) -> match compare a a1 with
| -1 -> T ((a, insert t1 a1) ::(a2,t2)::list)
| 0 -> t
| 1 -> if a < a2 then  T ((a1, insert t1 a):: (a2,t2):: list)
else let T res = insert (T ((a2,t2)::list)) a
in T ((a1,t1)::res)

let from_list l = List.fold_left insert (T[]) l


let list_of t = let rec doit acc = function
 | T[]-> acc
 | T((a,t)::list)-> a :: doit (
 doit acc (T list)) t
 in doit [] t



 let rec remove t a = match t with
 | T [] -> true, t
 | T [a',t'] -> (match compare a a' with
  | -1 -> false, t 
  | 0 ->  (match extract_min t' with
    | None, _ -> true, T[]
    | Some a'',t'' -> true, T [a'',t''] 
  )
  | 1 -> (match remove t' a with
    |false, _ -> false, t
    | true, t' -> true, T [a',t']
  )
  | T ((a1,t1)::(a2,t2)::list) ->  match compare a a1 with
    | -1 -> false, t
    | 0 -> (match extract_min t1 with 
      |None, _ -> true , T((a2,t2)::list)
      |Some a1, t1 -> true,  T((a1,t1) :: (a2,t2)::list) 
    )
    | 1 -> if a < a2 then remove t1 a 
    else (match remove ((a2,t2)::list) a  with  
      | false, _ -> false, T []
      | true, T res -> true ((a1,t1) :: res)
    )  



 )
 