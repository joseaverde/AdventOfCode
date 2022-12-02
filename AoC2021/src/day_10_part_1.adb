-- --- Day 10: Syntax Scoring ---
--
-- You ask the submarine to determine the best route out of the deep-sea cave,
-- but it only replies:
--
-- Syntax error in navigation subsystem on line: all of them
--
-- All of them?! The damage is worse than you thought. You bring up a copy of
-- the navigation subsystem (your puzzle input).
--
-- The navigation subsystem syntax is made of several lines containing chunks.
-- There are one or more chunks on each line, and chunks contain zero or more
-- other chunks. Adjacent chunks are not separated by any delimiter; if one
-- chunk stops, the next chunk (if any) can immediately start. Every chunk must
-- open and close with one of four legal pairs of matching characters:
--
--    - If a chunk opens with (, it must close with ).
--    - If a chunk opens with [, it must close with ].
--    - If a chunk opens with {, it must close with }.
--    - If a chunk opens with <, it must close with >.
--
-- So, () is a legal chunk that contains no other chunks, as is []. More
-- complex but valid chunks include ([]), {()()()}, <([{}])>,
-- [<>({}){}[([])<>]], and even (((((((((()))))))))).
--
-- Some lines are incomplete, but others are corrupted. Find and discard the
-- corrupted lines first.
--
-- A corrupted line is one where a chunk closes with the wrong character - that
-- is, where the characters it opens and closes with do not form one of the
-- four legal pairs listed above.
--
-- Examples of corrupted chunks include (], {()()()>, (((()))}, and
-- <([]){()}[{}]). Such a chunk can appear anywhere within a line, and its
-- presence causes the whole line to be considered corrupted.
--
-- For example, consider the following navigation subsystem:
--
--    [({(<(())[]>[[{[]{<()<>>
--    [(()[<>])]({[<{<<[]>>(
--    {([(<{}[<>[]}>{[]{[(<()>
--    (((({<>}<{<{<>}{[]{[]{}
--    [[<[([]))<([[{}[[()]]]
--    [{[{({}]{}}([{[{{{}}([]
--    {<[[]]>}<{[{[{[]{()[[[]
--    [<(<(<(<{}))><([]([]()
--    <{([([[(<>()){}]>(<<{{
--    <{([{{}}[<[[[<>{}]]]>[]]
--
-- Some of the lines aren't corrupted, just incomplete; you can ignore these
-- lines for now. The remaining five lines are corrupted:
--
--    - {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
--    - [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
--    - [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
--    - [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
--    - <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
--
-- Stop at the first incorrect closing character on each corrupted line.
--
-- Did you know that syntax checkers actually have contests to see who can get
-- the high score for syntax errors in a file? It's true! To calculate the
-- syntax error score for a line, take the first illegal character on the line
-- and look it up in the following table:
--
--    - ): 3 points.
--    - ]: 57 points.
--    - }: 1197 points.
--    - >: 25137 points.
--
-- In the above example, an illegal ) was found twice (2*3 = 6 points), an
-- illegal ] was found once (57 points), an illegal } was found once (1197
-- points), and an illegal > was found once (25137 points). So, the total
-- syntax error score for this file is 6+57+1197+25137 = 26397 points!
--
-- Find the first illegal character in each corrupted line of the navigation
-- subsystem. What is the total syntax error score for those errors?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Day_10; use Day_10;

procedure Day_10_Part_1 is
   function Parse_Line (Line : String) return Integer is
      Stack : Stack_Type;
   begin
      for Char of Line loop
         case Char is
            when '(' | '[' | '{' | '<' =>
               Stack.Push(Char);
            when ')' =>
               if Stack.Pop /= '(' then
                  return 3;
               end if;
            when ']' =>
               if Stack.Pop /= '[' then
                  return 57;
               end if;
            when '}' =>
               if Stack.Pop /= '{' then
                  return 1197;
               end if;
            when '>' =>
               if Stack.Pop /= '<' then
                  return 25137;
               end if;
            when others => raise Constraint_Error;
         end case;
      end loop;
      return 0;
   end Parse_Line;

   File   : Ada.Text_IO.File_Type;
   Points : Natural := 0;
begin

   Ada.Text_IO.Open(
      File => File,
      Mode => Ada.Text_Io.In_File,
      Name => "inputs/day_10.input"
   );

   while not Ada.Text_IO.End_Of_File(File) loop
      Points := Points + Parse_Line(Ada.Text_Io.Get_Line(File));
   end loop;

   Ada.Integer_Text_IO.Put(Points, Width => 1);
   Ada.Text_IO.New_Line;

end Day_10_Part_1;
