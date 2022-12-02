-- --- Day 05 : Part Two ---
--
-- Unfortunately, considering only horizontal and vertical lines doesn't give
-- you the full picture; you need to also consider diagonal lines.
--
-- Because of the limits of the hydrothermal vent mapping system, the lines in
-- your list will only ever be horizontal, vertical, or a diagonal line at
-- exactly 45 degrees. In other words:
--
--    - An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
--    - An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
--
-- Considering all lines from the above example would now produce the following
-- diagram:
--
--    1.1....11.
--    .111...2..
--    ..2.1.111.
--    ...1.2.2..
--    .112313211
--    ...1.2....
--    ..1...1...
--    .1.....1..
--    1.......1.
--    222111....
--
-- You still need to determine the number of points where at least two lines
-- overlap. In the above example, this is still anywhere in the diagram with a
-- 2 or larger - now a total of 12 points.
--
-- Consider all of the lines. At how many points do at least two lines overlap?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Day_05; use Day_05;

procedure Day_05_Part_2 is
   Data : Vectors.Vector;
   Map : array (Number_Type'Range, Number_Type'Range) of Natural := (
      (others => (others => 0)));
   From, To : Number_Type;
   Len : Number_Type;
   Count : Natural := 0;
   Dir_X, Dir_Y : Integer;
begin

   Load_Data(Data);

   for Line of Data loop
      if Line(1)(X) = Line(2)(X) then
         From := Number_Type'Min(Line(1)(Y), Line(2)(Y));
         To := Number_Type'Max(Line(1)(Y), Line(2)(Y));
         for YY in From .. To loop
            Increase(Map(Line(1)(X), YY));
         end loop;
      elsif Line(1)(Y) = Line(2)(Y) then
         From := Number_Type'Min(Line(1)(X), Line(2)(X));
         To := Number_Type'Max(Line(1)(X), Line(2)(X));
         for XX in From .. To loop
            Increase(Map(XX, Line(1)(Y)));
         end loop;
      else
         -- Always 45º
         Len := Number_Type'Max(Line(1)(X), Line(2)(X)) -
            Number_Type'Min(Line(1)(X), Line(2)(X));
         if Line(1)(Y) < Line(2)(Y) then
            Dir_Y := +1;
         else
            Dir_Y := -1;
         end if;
         if Line(1)(X) < Line(2)(X) then
            Dir_X := +1;
         else
            Dir_X := -1;
         end if;

         for I in 0 .. Len loop
            Increase(Map(Line(1)(X) + Dir_X*I, Line(1)(Y) + Dir_Y*I));
         end loop;
      end if;
   end loop;

   for XX in Map'Range(1) loop
      for YY in Map'Range(2) loop
         if Map(XX, YY) >= 2 then
            Count := Count + 1;
         end if;
      end loop;
   end loop;

   Ada.Integer_Text_IO.Put(Count, Width => 1);
   Ada.Text_IO.New_Line;

end Day_05_Part_2;
