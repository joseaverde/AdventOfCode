-- --- Day 1 : Part Two ---
--
-- Considering every single measurement isn't as useful as you expected:
-- there's just too much noise in the data.
--
-- Instead, consider sums of a three-measurement sliding window. Again
-- considering the above example:
--
--    199  A
--    200  A B
--    208  A B C
--    210    B C D
--    200  E   C D
--    207  E F   D
--    240  E F G
--    269    F G H
--    260      G H
--    263        H
--
-- Start by comparing the first and second three-measurement windows. The
-- measurements in the first window are marked A (199, 200, 208); their sum is
-- 199 + 200 + 208 = 607. The second window is marked B (200, 208, 210); its
-- sum is 618. The sum of measurements in the second window is larger than the
-- sum of the first, so this first comparison increased.
--
-- Your goal now is to count the number of times the sum of measurements in
-- this sliding window increases from the previous sum. So, compare A with B,
-- then compare B with C, then C with D, and so on. Stop when there aren't
-- enough measurements left to create a new three-measurement sum.
--
-- In the above example, the sum of each three-measurement window is as
-- follows:
--
--    A: 607 (N/A - no previous sum)
--    B: 618 (increased)
--    C: 618 (no change)
--    D: 617 (decreased)
--    E: 647 (increased)
--    F: 716 (increased)
--    G: 769 (increased)
--    H: 792 (increased)
--
-- In this example, there are 5 sums that are larger than the previous sum.
--
-- Consider sums of a three-measurement sliding window. How many sums are
-- larger than the previous sum?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Day_01_Part_2 is
   File         : Ada.Text_IO.File_Type;
   Count        : Natural := 0;
   Last_Sum     : Integer;
   Sum          : Integer;
   Measurements : array (1 .. 3) of Integer;
   Next         : Integer;
begin

   Ada.Text_IO.Open(
      File => File,
      Name => "inputs/day_1.input",
      Mode => Ada.Text_IO.In_File
   );

   Last_Sum := 0;
   for I in Measurements'Range loop
      Ada.Integer_Text_IO.Get(File, Measurements(I));
      Last_Sum := Last_Sum + Measurements(I);
   end loop;
   Sum := Last_Sum;

   while not Ada.Text_IO.End_Of_File(File) loop
      Ada.Integer_Text_IO.Get(File, Next);
      Sum := Last_Sum + Next - Measurements(1);
      if Sum > Last_Sum then
         Count := Count + 1;
      end if;
      Measurements(1) := Measurements(2);
      Measurements(2) := Measurements(3);
      Measurements(3) := Next;
   end loop;

   Ada.Text_IO.Close(File);
   Ada.Integer_Text_IO.Put(Count, Width => 1);
   Ada.Text_IO.New_Line;

end Day_01_Part_2;
