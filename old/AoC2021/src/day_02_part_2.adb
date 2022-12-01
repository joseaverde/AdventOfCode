-- --- Day 02 : Part Two ---
--
-- Based on your calculations, the planned course doesn't seem to make any
-- sense. You find the submarine manual and discover that the process is
-- actually slightly more complicated.
--
-- In addition to horizontal position and depth, you'll also need to track a
-- third value, aim, which also starts at 0. The commands also mean something
-- entirely different than you first thought:
--
--    - down X increases your aim by X units.
--    - up X decreases your aim by X units.
--    - forward X does two things:
--       - It increases your horizontal position by X units.
--       - It increases your depth by your aim multiplied by X.
--
-- Again note that since you're on a submarine, down and up do the opposite of
-- what you might expect: "down" means aiming in the positive direction.
--
-- Now, the above example does something different:
--
--    - forward 5 adds 5 to your horizontal position, a total of 5. Because
--      your aim is 0, your depth does not change.
--    - down 5 adds 5 to your aim, resulting in a value of 5.
--    - forward 8 adds 8 to your horizontal position, a total of 13. Because
--      your aim is 5, your depth increases by 8*5=40.
--    - up 3 decreases your aim by 3, resulting in a value of 2.
--    - down 8 adds 8 to your aim, resulting in a value of 10.
--    - forward 2 adds 2 to your horizontal position, a total of 15. Because
--      your aim is 10, your depth increases by 2*10=20 to a total of 60.
--
-- After following these new instructions, you would have a horizontal position
-- of 15 and a depth of 60. (Multiplying these produces 900.)
--
-- Using this new interpretation of the commands, calculate the horizontal
-- position and depth you would have after following the planned course. What
-- do you get if you multiply your final horizontal position by your final
-- depth?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
with Ada.Strings.Bounded;
with Ada.Text_IO;

procedure Day_02_Part_2 is
   type Command_Type is (Forward, Up, Down);
   Command  : Command_Type;
   Position : Integer := 0;
   Aim      : Integer := 0;
   Depth    : Integer := 0;

   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(512);
   Line  : Strings.Bounded_String;
   File  : Ada.Text_IO.File_Type;
   Input : Integer;
   Space : Natural;
begin

   Ada.Text_IO.Open(
      File => File,
      Name => "inputs/day_02.input",
      Mode => Ada.Text_IO.In_File
   );

   while not Ada.Text_IO.End_Of_File(File) loop
      Line := Strings.To_Bounded_String(Ada.Text_IO.Get_Line(File));
      Space := Strings.Index(Line, " ", 1);
      Command := Command_Type'Value(Strings.Slice(Line, 1, Space - 1));
      Input := Integer'Value(Strings.Slice(Line, Space, Strings.Length(Line)));

      case Command is
         when Forward =>
            Position := Position + Input;
            Depth := Depth + Input * Aim;
         when Down =>
            Aim := Aim + Input;
         when Up =>
            Aim := Aim - Input;
      end case;
   end loop;

   Ada.Text_IO.Close(File);
   Ada.Integer_Text_IO.Put(Position*Depth, Width => 1);
   Ada.Text_IO.New_Line;

end Day_02_Part_2;
