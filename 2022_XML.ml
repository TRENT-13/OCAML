type tag = A | B | C
type 't xml = Text of string | Comment of 't xml
| XML of 't * 't xml list
type 't query = Simple of 't | NotSimple of 't * string

let doc = XML (A, [
Comment (XML (C,[Text "stalin"]));
XML (B, [
Text "Hello!";
XML (A, [])
])
])
let q = NotSimple (A, "Hello!")


let rec contains  a = function
| Text s -> a=s 
| Comment b -> false 
| XML (h,rest) -> List.exists (contains a) rest



let rec contains s = function
| Text s' -> s=s'
| XML (_,content) -> List.exists (contains s) content
| Comment _ -> false


let rec search query xml = match query with
| NotSimple (a,s) -> let list = search (Simple a) xml in
List.filter (contains s) list
| Simple a -> (match xml with
| Text _ -> []
| Comment _ -> []
| XML (b,rest) ->
let list = List.concat
(List.map (search (Simple a)) rest)
in if a=b then xml :: list
else list
)

let open_tag = function
| A -> "<A>"
| B -> "<B>"
| C -> "<C>"
let closing_tag = function
| A -> "</A>"
| B -> "</B>"
| C -> "</C>"


let rec string_of = function
| Comment _ -> ""
| Text str -> str
| XML (t,list) -> open_tag t ^ (
String.concat "" (List.map string_of list)) ^
closing_tag t

