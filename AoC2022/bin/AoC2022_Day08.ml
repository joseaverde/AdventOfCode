(* --- Day 8: Treetop Tree House ---
 *
 * The expedition comes across a peculiar patch of tall trees all planted
 * carefully in a grid. The Elves explain that a previous expedition planted
 * these trees as a reforestation effort. Now, they're curious if this would be a
 * good location for a tree house.
 *
 * First, determine whether there is enough tree cover here to keep a tree house
 * hidden. To do this, you need to count the number of trees that are visible
 * from outside the grid when looking directly along a row or column.
 *
 * The Elves have already launched a quadcopter to generate a map with the height
 * of each tree (your puzzle input). For example:
 *
 * 30373
 * 25512
 * 65332
 * 33549
 * 35390
 *
 * Each tree is represented as a single digit whose value is its height, where 0
 * is the shortest and 9 is the tallest.
 *
 * A tree is visible if all of the other trees between it and an edge of the grid
 * are shorter than it. Only consider trees in the same row or column; that is,
 * only look up, down, left, or right from any given tree.
 *
 * All of the trees around the edge of the grid are visible - since they are
 * already on the edge, there are no trees to block the view. In this example,
 * that only leaves the interior nine trees to consider:
 *
 *  - The top-left 5 is visible from the left and top. (It isn't visible from
 *    the right or bottom since other trees of height 5 are in the way.)
 *  - The top-middle 5 is visible from the top and right.
 *  - The top-right 1 is not visible from any direction; for it to be visible,
 *    there would need to only be trees of height 0 between it and an edge.
 *  - The left-middle 5 is visible, but only from the right.
 *  - The center 3 is not visible from any direction; for it to be visible,
 *    there would need to be only trees of at most height 2 between it and an
 *    edge.
 *  - The right-middle 3 is visible from the right. In the bottom row, the
 *    middle 5 is visible, but the 3 and 4 are not.
 *
 *  With 16 trees visible on the edge and another 5 visible in the interior, a
 *  total of 21 trees are visible in this arrangement.
 *
 *  Consider your map; how many trees are visible from outside the grid?
 *
 * --- Part Two ---
 *
 * Content with the amount of tree cover available, the Elves just need to know
 * the best spot to build their tree house: they would like to be able to see a
 * lot of trees.
 *
 * To measure the viewing distance from a given tree, look up, down, left, and
 * right from that tree; stop if you reach an edge or at the first tree that is
 * the same height or taller than the tree under consideration. (If a tree is
 * right on the edge, at least one of its viewing distances will be zero.)
 *
 * The Elves don't care about distant trees taller than those found by the
 * rules above; the proposed tree house has large eaves to keep it dry, so they
 * wouldn't be able to see higher than the tree house anyway.
 *
 * In the example above, consider the middle 5 in the second row:
 *
 *       30373
 *       25512
 *       65332
 *       33549
 *       35390
 *
 *  - Looking up, its view is not blocked; it can see 1 tree (of height 3).
 *  - Looking left, its view is blocked immediately; it can see only 1 tree
 *    (of height 5, right next to it).
 *  - Looking right, its view is not blocked; it can see 2 trees.
 *  - Looking down, its view is blocked eventually; it can see 2 trees (one of
 *    height 3, then the tree of height 5 that blocks its view).
 *
 * A tree's scenic score is found by multiplying together its viewing distance
 * in each of the four directions. For this tree, this is 4 (found by
 * multiplying 1 * 1 * 2 * 2).
 *
 * However, you can do even better: consider the tree of height 5 in the middle
 * of the fourth row:
 *
 *       30373
 *       25512
 *       65332
 *       33549
 *       35390
 *
 *  - Looking up, its view is blocked at 2 trees (by another tree with a height
 *    of 5).
 *  - Looking left, its view is not blocked; it can see 2 trees.
 *  - Looking down, its view is also not blocked; it can see 1 tree.
 *  - Looking right, its view is blocked at 2 trees (by a massive tree of
 *    height 9).
 *
 * This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for
 * the tree house.
 *
 * Consider each tree on your map. What is the highest scenic score possible
 * for any tree?
 *
 * AUTHOR : José Antonio Verde Jiménez
 * DATE   : 2022-12-08
 *)

let () =
   let rec reverse l r =
      match l with
      |   []   -> r
      | h :: t -> reverse t (h :: r)
   in let rec get_all_input r =
      try
         let s = input_line stdin in
         get_all_input (s :: r)
      with End_of_file -> r
   in
   let lines = reverse (get_all_input []) [] in
   let cols = (match lines with h :: _ -> String.length h | [] -> assert false) in
   let rows = List.length lines in
   let grid = Array.make_matrix rows cols 0 in
   let rec fill_grid l r =
      match l with
      |   []   -> ()
      | h :: t ->
         String.iteri (fun c i -> grid.(r).(c) <- Char.code i - Char.code '0') h ;
         fill_grid t (r + 1)
   in
   let visible = ref 0 in
   let count_visible () =
      (* let dirs = [| 0; 1; 0; -1; 0 |] in *)
      let is_it = ref false in
      let left = ref false in
      let right = ref false in
      let top = ref false in
      let bottom = ref false in
      for r = 0 to rows - 1 do
         for c = 0 to cols - 1 do
            is_it := true ;
            if r = 0 || r = rows - 1 || c = 0 || c = cols - 1 then is_it := true
               else (
                  left := true ;
                  right := true ;
                  bottom := true ;
                  top := true ;
                  for r' = 0 to r - 1 do
                     if grid.(r').(c) >= grid.(r).(c) then top := false
                  done ;
                  for r' = r + 1 to rows - 1 do
                     if grid.(r').(c) >= grid.(r).(c) then bottom := false
                  done ;
                  for c' = 0 to c - 1 do
                     if grid.(r).(c') >= grid.(r).(c) then left := false
                  done ;
                  for c' = c + 1 to cols - 1 do
                     if grid.(r).(c') >= grid.(r).(c) then right := false
                  done ;
                  (* Printf.printf "(%d, %d) -> %b %b %b %b\n"
                     r c !top !bottom !left !right ; *)
                  is_it := !top || !bottom || !left || !right
               ) ;
            (if !is_it then visible := !visible + 1)
         done
      done
   in let score = ref 0 in
   let scenic_scores () =
      let stop = ref false in
      let top = ref 0 in
      let bottom = ref 0 in
      let left = ref 0 in
      let right = ref 0 in
      for r = 0 to rows - 1 do
         for c = 0 to cols - 1 do
            (* Top *)
            stop := false ;
            top := 0 ;
            for r' = r - 1 downto 0 do
               if !stop = false then (
                  top := !top + 1 ;
                  if grid.(r').(c) >= grid.(r).(c) then stop := true)
            done ;
            (* Bottom *)
            stop := false ;
            bottom := 0 ;
            for r' = r + 1 to rows - 1 do
               if !stop = false then (
                  bottom := !bottom + 1 ;
                  if grid.(r').(c) >= grid.(r).(c) then stop := true)
            done ;
            (* Left *)
            stop := false ;
            left := 0 ;
            for c' = c - 1 downto 0 do
               if !stop = false then (
                  left := !left + 1 ;
                  if grid.(r).(c') >= grid.(r).(c) then stop := true)
            done ;
            (* Right *)
            stop := false ;
            right := 0 ;
            for c' = c + 1 to cols - 1 do
               if !stop = false then (
                  right := !right + 1 ;
                  if grid.(r).(c') >= grid.(r).(c) then stop := true)
            done ;
            (* Printf.printf "(%d %d) -> (%d %d %d %d)\n"
               r c !top !bottom !left !right ; *)
            let s = !left * !right * !top * !bottom in
               if s > !score then score := s
         done
      done
   in
   fill_grid lines 0 ;
   count_visible () ;
   scenic_scores () ;
   Printf.printf "AoC 2022 Day 08 Part 1 %d\n" !visible ;
   Printf.printf "AoC 2022 Day 08 Part 2 %d\n" !score

(* End of file *)
