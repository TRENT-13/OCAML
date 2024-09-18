type game_info = string * string list * string * string list

let calculate_score (team1, scorers1, team2, scorers2) =
  let count_goals scorers =
    List.fold_left (fun acc scorer ->
      if scorer = "OG" then acc+1
      else acc + 1
    ) 0 scorers
  in
  let team1_goals = count_goals scorers1 in
  let team2_goals = count_goals scorers2 in
  ((team1, team1_goals), (team2, team2_goals))

let compare calculate_score = List.fold_left (fun ("x",a,i) ("y",b,_j) -> if a > b then ("x",a,i+3) if else a < b then ("y",b,j+4) else ("x",a,i+1) ("y",b,j+1)) 

let gf_ga calculate_score = List.fold_left (fun ("x",a,gf_a,ga_a) ("y",b,gf_b,ga_b) -> ("x",a,gf+a,ga_a+b) ("y",b,gf_b+b,ga_a+))

let game = ("Fra", ["OG"; "Griezmann"; "Pogba"; "Mbappe"], "Cro", ["Perisic"; "Mandzukic"])