(* --- Day 7: No Space Left On Device ---
 *
 * You can hear birds chirping and raindrops hitting leaves as the expedition
 * proceeds. Occasionally, you can even hear much louder sounds in the distance;
 * how big do the animals get out here, anyway?
 *
 * The device the Elves gave you has problems with more than just its
 * communication system. You try to run a system update:
 *       $ system-update --please --pretty-please-with-sugar-on-top
 *       Error: No space left on device
 *
 * Perhaps you can delete some files to make space for the update?
 *
 * You browse around the filesystem to assess the situation and save the
 * resulting terminal output (your puzzle input). For example:
 *
 *       $ cd /
 *       $ ls
 *       dir a
 *       14848514 b.txt
 *       8504156 c.dat
 *       dir d
 *       $ cd a
 *       $ ls
 *       dir e
 *       29116 f
 *       2557 g
 *       62596 h.lst
 *       $ cd e
 *       $ ls
 *       584 i
 *       $ cd ..
 *       $ cd ..
 *       $ cd d
 *       $ ls
 *       4060174 j
 *       8033020 d.log
 *       5626152 d.ext
 *       7214296 k
 *
 * The filesystem consists of a tree of files (plain data) and directories
 * (which can contain other directories or files). The outermost directory is
 * called /. You can navigate around the filesystem, moving into or out of
 * directories and listing the contents of the directory you're currently in.
 *
 * Within the terminal output, lines that begin with $ are commands you
 * executed, very much like some modern computers:
 *
 *  - cd means change directory. This changes which directory is the current
 *    directory, but the specific result depends on the argument: 
 *     - cd x moves in one level: it looks in the current directory for the
 *       directory named x and makes it the current directory.
 *     - cd .. moves out one level: it finds the directory that contains the
 *       current directory, then makes that directory the current directory.
 *     - cd / switches the current directory to the outermost directory, /.
 *  - ls means list. It prints out all of the files and directories immediately
 *    contained by the current directory: 
 *     - 123 abc means that the current directory contains a file named abc
 *       with size 123.
 *     - dir xyz means that the current directory contains a directory named
 *       xyz.G
 *  - Given the commands and output in the example above, you can determine
 *    that the filesystem looks visually like this:
 *
 *       - / (dir)
 *          - a (dir)
 *             - e (dir)
 *                - i (file, size=584)
 *             - f (file, size=29116)
 *             - g (file, size=2557)
 *             - h.lst (file, size=62596)
 *          - b.txt (file, size=14848514)
 *          - c.dat (file, size=8504156)
 *          - d (dir)
 *             - j (file, size=4060174)
 *             - d.log (file, size=8033020)
 *             - d.ext (file, size=5626152)
 *             - k (file, size=7214296)
 *
 * Here, there are four directories: / (the outermost directory), a and d
 * (which are in /), and e (which is in a). These directories also contain
 * files of various sizes.
 *
 * Since the disk is full, your first step should probably be to find
 * directories that are good candidates for deletion. To do this, you need to
 * determine the total size of each directory. The total size of a directory is
 * the sum of the sizes of the files it contains, directly or indirectly.
 * (Directories themselves do not count as having any intrinsic size.)
 *
 * The total sizes of the directories above can be found as follows:
 *
 *  - The total size of directory e is 584 because it contains a single file i
 *    of size 584 and no other directories.
 *  - The directory a has total size 94853 because it contains files f (size
 *    29116), g (size 2557), and h.lst (size 62596), plus file i indirectly (a
 *    contains e which contains i).
 *  - Directory d has total size 24933642.
 *  - As the outermost directory, / contains every file. Its total size is
 *    48381165, the sum of the size of every file.
 *
 * To begin, find all of the directories with a total size of at most 100000,
 * then calculate the sum of their total sizes. In the example above, these
 * directories are a and e; the sum of their total sizes is 95437 (94853 +
 * 584). (As in this example, this process can count files more than once!)
 *
 * Find all of the directories with a total size of at most 100000. What is the
 * sum of the total sizes of those directories?
 *
 * --- Part Two ---
 *
 * Now, you're ready to choose a directory to delete.
 *
 * The total disk space available to the filesystem is 70000000. To run the
 * update, you need unused space of at least 30000000. You need to find a
 * directory you can delete that will free up enough space to run the update.
 *
 * In the example above, the total size of the outermost directory (and thus
 * the total amount of used space) is 48381165; this means that the size of the
 * unused space must currently be 21618835, which isn't quite the 30000000
 * required by the update. Therefore, the update still requires a directory
 * with total size of at least 8381165 to be deleted before it can run.
 *
 * To achieve this, you have the following options:
 *
 *  - Delete directory e, which would increase unused space by 584.
 *  - Delete directory a, which would increase unused space by 94853.
 *  - Delete directory d, which would increase unused space by 24933642.
 *  - Delete directory /, which would increase unused space by 48381165.
 *
 * Directories e and a are both too small; deleting them would not free up
 * enough space. However, directories d and / are both big enough! Between
 * these, choose the smallest: d, increasing unused space by 24933642.
 *
 * Find the smallest directory that, if deleted, would free up enough space on
 * the filesystem to run the update. What is the total size of that directory?
 *
 * AUTHOR : José Antonio Verde Jiménez
 * DATE   : 2022-12-07
 *)

type tree =
   | File of {parent : tree ref ; name : string ; size  : int}
   | Dir  of {parent : tree ref ; name : string ; items : tree list ref}
   | Null

let root = Dir {parent = ref Null ; name = "" ; items = ref []}

let get_parent (t : tree) =
   match t with
   | File {parent = p ; name = _ ; size = _}  -> !p
   | Dir  {parent = p ; name = _ ; items = _} -> !p
   | _ -> raise Not_found

let search (t : tree) s =
   let rec _search l =
      match l with
      |   []   -> raise Not_found
      | h :: t -> match h with
         | Dir  {parent = _ ; name = n ; items = _} ->
            if n = s then h else _search t
         | _ -> _search t
   in match t with
   | Dir  {parent = _ ; name = _ ; items = i} -> _search !i
   | _ -> raise Not_found

let insert t (f : tree) =
   match t with
   | Dir d -> begin
      match f with
      | File f' -> f'.parent := t
      | Dir  f' -> f'.parent := t
      | _ -> raise Not_found end ;
      d.items := f :: !(d.items)
   | _ -> raise Not_found

let print_tree (t : tree) =
   let rec _print tab (f : tree) =
      for _ = 0 to tab do
         Printf.printf "   "
      done ;
      match f with
      | File f -> Printf.printf "%s %d\n" f.name f.size
      | Dir  d -> Printf.printf "%s/\n" d.name ;
         List.iter (_print (tab + 1)) !(d.items)
      | Null   -> Printf.printf "null\n"
   in _print 0 t

let () =
   let current = ref root in
   let rec command s =
      let s' = String.split_on_char ' ' s in
      let rec _next_file () =
         try
            let t  = input_line stdin in
            let t' = String.split_on_char ' ' t in
            match t' with
            | "$"   :: _   -> command t
            | "dir" :: [n] -> begin
               insert !current (Dir {
                  parent = ref !current ; name = n ; items = ref []}) ;
               _next_file ()
            end
            | i     :: [n] -> begin
               insert !current (File {
                  parent = ref !current ; name = n ; size = int_of_string i}) ;
               _next_file ()
            end
            | _ -> raise Not_found
         with End_of_file -> ()
      in begin
      match s' with
      | "$" :: "cd" :: ["/"]  -> current := root
      | "$" :: "cd" :: [".."] -> current := get_parent !current
      | "$" :: "cd" :: [d]    -> current := search !current d
      | "$" :: ["ls"]         -> _next_file ()
      | _ -> raise Not_found
      end ; command (input_line stdin)
   in try
      command (input_line stdin)
   with End_of_file -> ()

(* Part 1 *)
let () =
   let sum = ref 0 in
   let rec get_size (t : tree) =
      let rec sum_list l t' =
         match l with
         |   []   -> t'
         | h :: t -> sum_list t (t' + (get_size h))
      in match t with
      | File f -> f.size
      | Dir  d -> let s = sum_list !(d.items) 0 in
         (if s <= 100_000 then sum := !sum + s) ; s
      | Null   -> 0
   in
   let max    = 70000000
   and needed = 30000000
   in let s = print_tree root; get_size root in
   let sum = !sum in
   let need = needed - (max - s) in
   let rec min_size (l : tree) b =
      let rec _min l b =
         match l with
         |   []   -> b
         | h :: t -> _min t (
            let k = min_size h b in
            let i = get_size h in
            if i > need && i < k then i else k)
      in match l with
      | File _ -> b
      | Dir  d -> _min !(d.items) b
      | Null   -> b
   in let min = min_size root s in
      Printf.printf "AoC 2022 Day 07 Part 1 %d\n" sum ;
      Printf.printf "AoC 2022 Day 07 Part 2 %d\n" min

(* End of file *)
