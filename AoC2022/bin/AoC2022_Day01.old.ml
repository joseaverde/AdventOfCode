(* --- Day 1: Calorie Counting ---
 * Santa's reindeer typically eat regular reindeer food, but they need a lot of
 * magical energy to deliver presents on Christmas. For that, their favorite
 * snack is a special type of star fruit that only grows deep in the jungle.
 * The Elves have brought you on their annual expedition to the grove where the
 * fruit grows.
 *
 * To supply enough magical energy, the expedition needs to retrieve a minimum
 * of fifty stars by December 25th. Although the Elves assure you that the
 * grove has plenty of fruit, you decide to grab any fruit you see along the
 * way, just in case.
 *
 * Collect stars by solving puzzles. Two puzzles will be made available on each
 * day in the Advent calendar; the second puzzle is unlocked when you complete
 * the first. Each puzzle grants one star. Good luck!
 *
 * The jungle must be too overgrown and difficult to navigate in vehicles or
 * access from the air; the Elves' expedition traditionally goes on foot. As
 * your boats approach land, the Elves begin taking inventory of their
 * supplies. One important consideration is food - in particular, the number of
 * Calories each Elf is carrying (your puzzle input).
 *
 * The Elves take turns writing down the number of Calories contained by the
 * various meals, snacks, rations, etc. that they've brought with them, one
 * item per line. Each Elf separates their own inventory from the previous
 * Elf's inventory (if any) by a blank line.
 *
 * For example, suppose the Elves finish writing their items' Calories and end
 * up with the following list:
 *
 *    1000
 *    2000
 *    3000
 *
 *    4000
 *
 *    5000
 *    6000
 *
 *    7000
 *    8000
 *    9000
 *
 *    10000
 *
 * This list represents the Calories of the food carried by five Elves:
 *
 *  - The first Elf is carrying food with 1000, 2000, and 3000 Calories, a
 *    total of 6000 Calories.
 *  - The second Elf is carrying one food item with 4000 Calories.
 *  - The third Elf is carrying food with 5000 and 6000 Calories, a total of
 *    11000 Calories.
 *  - The fourth Elf is carrying food with 7000, 8000, and 9000 Calories, a
 *    total of 24000 Calories.
 *  - The fifth Elf is carrying one food item with 10000 Calories.
 * In case the Elves get hungry and need extra snacks, they need to know which
 * Elf to ask: they'd like to know how many Calories are being carried by the
 * Elf carrying the most Calories. In the example above, this is 24000 (carried
 * by the fourth Elf).
 *
 * Find the Elf carrying the most Calories. How many total Calories is that Elf
 * carrying?
 *
 * --- Part Two ---
 *
 * By the time you calculate the answer to the Elves' question, they've already
 * realized that the Elf carrying the most Calories of food might eventually
 * run out of snacks.
 *
 * To avoid this unacceptable situation, the Elves would instead like to know
 * the total Calories carried by the top three Elves carrying the most
 * Calories total way, even if one of those Elves runs out of snacks, they
 * still have two backups.
 *
 * In the example above, the top three Elves are the fourth Elf (with 24000
 * Calories), then the third Elf (with 11000 Calories), then the fifth Elf
 * (with 10000 Calories). The sum of the Calories carried by these three elves
 * is 45000.
 *
 * Find the top three Elves carrying the most Calories. How many Calories are
 * those Elves carrying in total?
 *
 * AUTHOR : José Antonio Verde Jiménez  <joseaverde@protonmail.com>
 * DATE   : 2022-12-01
 *)

let get_calories () =
   let rec _get_calories r =
   try begin
      let s = input_line stdin in
      if String.length s = 0 then r else _get_calories ((int_of_string s) :: r)
   end
   with End_of_file -> r
   in _get_calories []

let total_calories l =
   let rec _sum l r =
      match l with
      |   []   -> r
      | h :: t -> _sum t (h + r)
   in _sum l 0

let reverse l =
   let rec _reverse l r =
      match l with
      |   []   -> r
      | h :: t -> _reverse t (h :: r)
   in _reverse l []

let map l f =
   let rec _map l f r =
      match l with
      |   []   -> reverse r
      | h :: t -> _map t f ((f h) :: r)
   in _map l f []

let get_all_calories () =
   let rec _get_all_calories r =
      let cal = get_calories () in
      if cal = [] then r else _get_all_calories (cal :: r)
   in _get_all_calories []

let max_calories l =
   let rec _max_calories l m =
      match l with
      |   []   -> m
      | h :: t -> (_max_calories t) (if h > m then h else m)
   in match l with
      |   []   -> raise Not_found
      | h :: t -> _max_calories t h

let max3_calories r =
   let b1 = ref 0
   and b2 = ref 0
   and b3 = ref 0 in
   let rec _best r =
      match r with
      |   []   -> ()
      | h :: t ->
      begin
         if      h > !b1 then (b3 := !b2 ; b2 := !b1 ; b1 := h)
         else if h > !b2 then (b3 := !b2 ; b2 := h)
         else if h > !b3 then (b3 := h)
            else ()
      end ; _best t
   in _best r ; [!b1; !b2; !b3]

let all_calories = get_all_calories ()
let calories = map all_calories total_calories

let () =
   Printf.printf "%d\n" (max_calories calories) ;
   Printf.printf "%d\n" (total_calories (max3_calories calories))
