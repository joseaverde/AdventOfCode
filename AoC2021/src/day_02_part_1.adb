-- --- Day 2: Dive! ---
--
-- Now, you need to figure out how to pilot this thing.
--
-- It seems like the submarine can take a series of commands like forward 1,
-- down 2, or up 3:
--
--    - forward X increases the horizontal position by X units.
--    - down X increases the depth by X units.
--    - up X decreases the depth by X units.
--
-- Note that since you're on a submarine, down and up affect your depth, and so
-- they have the opposite result of what you might expect.
--
-- The submarine seems to already have a planned course (your puzzle input).
-- You should probably figure out where it's going. For example:
--
--    forward 5
--    down 5
--    forward 8
--    up 3
--    down 8
--    forward 2
--
-- Your horizontal position and depth both start at 0. The steps above would
-- then modify them as follows:
--
--    - forward 5 adds 5 to your horizontal position, a total of 5.
--    - down 5 adds 5 to your depth, resulting in a value of 5.
--    - forward 8 adds 8 to your horizontal position, a total of 13.
--    - up 3 decreases your depth by 3, resulting in a value of 2.
--    - down 8 adds 8 to your depth, resulting in a value of 10.
--    - forward 2 adds 2 to your horizontal position, a total of 15.
--
-- After following these instructions, you would have a horizontal position of
-- 15 and a depth of 10. (Multiplying these together produces 150.)
--
-- Calculate the horizontal position and depth you would have after following
-- the planned course. What do you get if you multiply your final horizontal
-- position by your final depth?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
with Ada.Strings.Bounded;
with Ada.Text_IO;

procedure Day_02_Part_1 is
   type Command_Type is (Forward, Down, Up);
   Command  : Command_Type;
   Position : Integer := 0;
   Depth    : Integer := 0;

   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (500);
   File  : Ada.Text_IO.File_Type;
   Line  : Strings.Bounded_String;
   Space : Natural;
   Input : Natural;
begin

   Ada.Text_IO.Open(
      File => File,
      Name => "inputs/day_2.input",
      Mode => Ada.Text_IO.In_File
   );

   while not Ada.Text_IO.End_Of_File(File) loop
      Line := Strings.To_Bounded_String(Ada.Text_IO.Get_Line(File));
      Space := Strings.Index(Line, " ", 1);
      Command := Command_Type'Value(Strings.Slice(Line, 1, Space-1));
      Input := Integer'Value(Strings.Slice(Line, Space, Strings.Length(Line)));

      case Command is
         when Forward =>
            Position := Position + Input;
         when Down =>
            Depth := Depth + Input;
         when Up =>
            Depth := Depth - Input;
      end case;
   end loop;

   Ada.Text_IO.Close(File);
   Ada.Integer_Text_IO.Put(Position*Depth, Width => 1);
   Ada.Text_IO.New_Line;

end Day_02_Part_1;
