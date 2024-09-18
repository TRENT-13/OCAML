type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}

type database = student list

let student1 = {
  first_name = "Alice";
  last_name = "Johnson";
  id = 1;
  semester = 3;
  grades = [(1, 3.5); (2, 4.0)];
}

let student2 = {
  first_name = "Bob";
  last_name = "Smith";
  id = 2;
  semester = 2;
  grades = [(1, 3.0); (3, 3.7)];
}

let student3 = {
  first_name = "Alice";
  last_name = "Johnson";
  id = 3;
  semester = 2;
  grades = [(1, 3.5); (2, 4.0)];
}

let database : database = [student1; student2; student3] 


let get_grades student = student.grades



(*             remove student by id          *)
let rec remove_by_id db id = match db with
  | [] -> []
  | h :: t -> if h.id=id then remove_by_id t id else h :: remove_by_id t id


  (*     USING FILTER *)

  (* Remove student by id *)
let remove_by_id db id =
  List.filter (fun student -> student.id <> id) 


(*     student   number    *)



let rec studen_num db acc = match db with
  | [] -> acc
  | h :: t -> 1 + studen_num t acc


  (* Insert a student into a database *)


  let first_names = ["Charlie"; "Dana"; "Eve"; "Frank"; "Grace"]
  let last_names = ["Miller"; "Davis"; "Martinez"; "Garcia"; "Wilson"]
  
  (* Function to generate a random float between two values *)
  let random_float min max =
    min +. (Random.float (max -. min))
  
  (* Function to generate random grades *)
  let generate_random_grades num_courses =
    let rec helper n acc =
      if n = 0 then acc
      else
        let course_id = Random.int 10 + 1 in  (* Random course ID between 1 and 10 *)
        let grade = random_float 2.0 4.0 in   (* Random grade between 2.0 and 4.0 *)
        helper (n-1) ((course_id, grade) :: acc)
    in
    helper num_courses []
  
  (* Function to create a new random student *)
  let create_random_student id =
    let first_name = List.nth first_names (Random.int (List.length first_names)) in
    let last_name = List.nth last_names (Random.int (List.length last_names)) in
    let semester = Random.int 8 + 1 in  (* Random semester between 1 and 8 *)
    let grades = generate_random_grades (Random.int 5 + 1) in  (* Random number of courses between 1 and 5 *)
    {
      first_name;
      last_name;
      id;
      semester;
      grades;
    }
  
  (* Generate a new student with a unique ID *)
  let new_student_id = List.length database + 1
  let new_student = create_random_student new_student_id
  
  (* Insert the new student into the database *)
  let database = new_student :: database

  
  
(*        student_average_grade      *)

let calculate_average grades =
  let total_grades = List.fold_left (fun acc (num_subjects, grade) -> acc +. (grade *. float_of_int num_subjects)) 0.0 grades in
  let total_subjects = List.fold_left (fun acc (num_subjects, _) -> acc + num_subjects) 0 grades in
  total_grades /. float_of_int total_subjects;;

(*   course average grade *)

let calculate_course_average db =
  let all_student_grades = List.concat (List.map get_grades db) in
  let total_grades = List.fold_left (fun acc (num_subjects, grade) -> acc +. (grade *. float_of_int num_subjects)) 0.0 all_student_grades in
  let total_subjects = List.fold_left (fun acc (num_subjects, _) -> acc + num_subjects) 0 all_student_grades in
  total_grades /. float_of_int total_subjects;;