(* The expedition can depart as soon as the final supplies have been unloaded
 * from the ships. Supplies are stored in stacks of marked crates, but because
 * the needed supplies are buried under many other crates, the crates need to
 * be rearranged.
 * 
 * The ship has a giant cargo crane capable of moving crates between stacks. To
 * ensure none of the crates get crushed or fall over, the crane operator will
 * rearrange them in a series of carefully-planned steps. After the crates are
 * rearranged, the desired crates will be at the top of each stack.
 * 
 * The Elves don't want to interrupt the crane operator during this delicate
 * procedure, but they forgot to ask her which crate will end up where, and
 * they want to be ready to unload them as soon as possible so they can embark.
 * 
 * They do, however, have a drawing of the starting stacks of crates and the
 * rearrangement procedure (your puzzle input). For example:
 *
 *       [D]    
 *       [N] [C]    
 *       [Z] [M] [P]
 *        1   2   3 
 *
 *        move 1 from 2 to 1
 *        move 3 from 1 to 3
 *        move 2 from 2 to 1
 *        move 1 from 1 to 2
 *
 * In this example, there are three stacks of crates. Stack 1 contains two
 * crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains
 * three crates; from bottom to top, they are crates M, C, and D. Finally,
 * stack 3 contains a single crate, P.
 * Then, the rearrangement procedure is given. In each step of the procedure, a
 * quantity of crates is moved from one stack to a different stack. In the
 * first step of the above rearrangement procedure, one crate is moved from
 * stack 2 to stack 1, resulting in this configuration:
 *
 *       [D]        
 *       [N] [C]    
 *       [Z] [M] [P]
 *        1   2   3
 *
 * In the second step, three crates are moved from stack 1 to stack 3. Crates
 * are moved one at a time, so the first crate to be moved (D) ends up below
 * the second and third crates:
 *
 *               [Z]
 *               [N]
 *           [C] [D]
 *           [M] [P]
 *        1   2   3
 *
 * Then, both crates are moved from stack 2 to stack 1. Again, because crates
 * are moved one at a time, crate C ends up below crate M:
 *
 *               [Z]
 *               [N]
 *       [M]     [D]
 *       [C]     [P]
 *        1   2   3
 *
 * Finally, one crate is moved from stack 1 to stack 2:
 *
 *               [Z]
 *               [N]
 *               [D]
 *       [C] [M] [P]
 *        1   2   3
 * The Elves just need to know which crate will end up on top of each stack; in
 * this example, the top crates are C in stack 1, M in stack 2, and Z in stack
 * 3, so you should combine these together and give the Elves the message CMZ.
 *
 * After the rearrangement procedure completes, what crate ends up on top of
 * each stack?
 *
 * --- Part Two ---
 *
 * As you watch the crane operator expertly rearrange the crates, you notice
 * the process isn't following your prediction.
 *
 * Some mud was covering the writing on the side of the crane, and you quickly
 * wipe it away. The crane isn't a CrateMover 9000 - it's a CrateMover 9001.
 * *
 * The CrateMover 9001 is notable for many new and exciting features: air
 * conditioning, leather seats, an extra cup holder, and the ability to pick up
 * and move multiple crates at once.
 *
 * Again considering the example above, the crates begin in the same
 * configuration:
 *
 *           [D]    
 *       [N] [C]    
 *       [Z] [M] [P]
 *        1   2   3
 *
 * Moving a single crate from stack 2 to stack 1 behaves the same as before:
 *
 * AUTHOR : José Antonio Verde Jiménez
 * DATE   : 2022-12-05
 *)

let size = 9

let reverse_stack s =
   let rec _reverse_stack s r =
      match s with
      |     []   -> r
      | ' ' :: t -> _reverse_stack t r
      |  h  :: t -> _reverse_stack t (h :: r)
   in _reverse_stack s []

let pop_stack b s c =     (* c = count *)
   let rec _pop s c r =
      if c = 0 then (s, r) else
      match s with
      |   []   -> raise Not_found
      | h :: t -> _pop t (c - 1) (h :: r)
   in let (s', r') = _pop s c [] in ((if b then reverse_stack r' else r'), s')

let () =
   let crates = Array.make size [' '] in
   let rec _load_crates () =
      let s = input_line stdin in
      if s.[1] = '1' then begin let _ = input_line stdin in () end else begin
         for i = 0 to size - 1 do
            let i' = i * 4 + 1 in
            let c = s.[i'] in
            Array.set crates i (
               if c = ' ' then Array.get crates i
                  else c :: (Array.get crates i))
         done;
         _load_crates ()
         end
   in let rec _actions cr cr' =
      try
         let s = input_line stdin in
         let (c, f, t) = match String.split_on_char ' ' s with
            | _ :: c :: _ :: f :: _ :: t :: [] -> (
               int_of_string c,
               (int_of_string f) - 1,
               (int_of_string t) - 1)
            | _ -> assert false
         in
         let (a, b) = pop_stack false (Array.get cr f) c in
         let x = a @ (Array.get cr t) in
         let (a', b') = pop_stack true (Array.get cr' f) c in
         let x' = a' @ (Array.get cr' t) in
            (* Printf.printf "%s\n" (String.init size (
               fun i -> match Array.get cr' i with h::_ -> h | _ -> ' '));
            Printf.printf "move %d from %d to %d\n" c (f + 1) (t + 1) ; *)
            Array.set cr  f b  ;
            Array.set cr  t x  ;
            Array.set cr' f b' ;
            Array.set cr' t x' ;
            (* Printf.printf "%s\n\n" (String.init size (
               fun i -> match Array.get cr' i with h::_ -> h | _ -> ' ')); *)
            _actions cr cr'
      with End_of_file -> ()
   in
   _load_crates () ;
   for i = 0 to size - 1 do
      Array.set crates i (reverse_stack (reverse_stack (Array.get crates i)))
   done ;
   begin
      let copy_crates c =
         let r = Array.make size [' '] in
         for i = 0 to size - 1 do
            Array.set r i (reverse_stack (Array.get c i))
         done ; r
      in let crates' = copy_crates crates in
      _actions crates crates' ;
      begin
         let top = String.init size (
            fun i -> match Array.get crates i with h::_ -> h | _ -> ' ') in
         let top' = String.init size (
            fun i -> match Array.get crates' i with h::_ -> h | _ -> ' ') in
         Printf.printf "AoC 2022 Day 05 Part 1 -> %s\n" top  ;
         Printf.printf "AoC 2022 Day 05 Part 2 -> %s\n" top' ;
      end
   end
