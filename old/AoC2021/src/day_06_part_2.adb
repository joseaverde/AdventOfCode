-- --- Day 06 : Part Two ---
--
-- Suppose the lanternfish live forever and have unlimited food and space.
-- Would they take over the entire ocean?
--
-- After 256 days in the example above, there would be a total of 26984457539
-- lanternfish!
--
-- How many lanternfish would there be after 256 days?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Day_06; use Day_06;

procedure Day_06_Part_2 is
   Lanternfish : Vectors.Vector;

   -- Trying to solve this problem like in the part 1 is inviable. And thus we
   -- will have to consider all the lanternfish with the same remaining days.
   package Banks is new Ada.Containers.Vectors (
      Index_Type   => Day_Count,
      Element_Type => Long_Integer
   );
   Bank : Banks.Vector;
   Birth : Long_Integer;
   Sum : Long_Integer := 0;
begin
   for Day in Day_Count'Range loop
      Bank.Append(0);
   end loop;

   Load_Data(Lanternfish);
   for Fish of Lanternfish loop
      Bank(Fish) := Bank(Fish) + 1;
   end loop;

   for I in Integer range 1 .. 256 loop
      -- We pop the first element of the list (the lanternfish that will give
      -- birth right now).
      Birth := Bank.First_Element;
      Bank.Delete_First;
      -- The number that has given birth is the same as the new offspring.
      Bank.Append(Birth);
      -- And will give birth in 7 days.
      Bank(6) := Bank(6) + Birth;
   end loop;

   for B of Bank loop
      Sum := Sum + B;
   end loop;

   Ada.Text_IO.Put_Line(Sum'Image);
end Day_06_Part_2;
