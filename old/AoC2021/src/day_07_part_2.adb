-- --- Day 07 : Part Two ---
--
-- The crabs don't seem interested in your proposed solution. Perhaps you
-- misunderstand crab engineering?
--
-- As it turns out, crab submarine engines don't burn fuel at a constant rate.
-- Instead, each change of 1 step in horizontal position costs 1 more unit of
-- fuel than the last: the first step costs 1, the second step costs 2, the
-- third step costs 3, and so on.
--
-- As each crab moves, moving further becomes more expensive. This changes the
-- best horizontal position to align them all on; in the example above, this
-- becomes 5:
--
--    - Move from 16 to 5: 66 fuel
--    - Move from 1 to 5: 10 fuel
--    - Move from 2 to 5: 6 fuel
--    - Move from 0 to 5: 15 fuel
--    - Move from 4 to 5: 1 fuel
--    - Move from 2 to 5: 6 fuel
--    - Move from 7 to 5: 3 fuel
--    - Move from 1 to 5: 10 fuel
--    - Move from 2 to 5: 6 fuel
--    - Move from 14 to 5: 45 fuel
--
-- This costs a total of 168 fuel. This is the new cheapest possible outcome;
-- the old alignment position (2) now costs 206 fuel instead.
--
-- Determine the horizontal position that the crabs can align to using the
-- least fuel possible so they can make you an escape route! How much fuel must
-- they spend to align to that position?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Day_07; use Day_07;

procedure Day_07_Part_2 is
   use Vectors;
   Min, Max : Natural;
   Data     : Vector;
   Fuel     : Natural;
   Min_Fuel : Natural := Natural'Last;
   function Sum (N : Natural) return Natural is
   begin
      return (N*(N+1))/2;
   end Sum;
begin

   Load_Data(Data, Min, Max);
   for Position in Min .. Max loop
      Fuel := 0;
      for Original_Position of Data loop
         Fuel := Fuel + Sum(abs(Original_Position - Position));
      end loop;
      if Fuel < Min_Fuel then
         Min_Fuel := Fuel;
      end if;
   end loop;

   Ada.Integer_Text_IO.Put(Min_Fuel, Width => 1);
   Ada.Text_IO.New_Line;

end Day_07_Part_2;
