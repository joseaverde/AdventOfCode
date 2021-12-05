-- --- Day 5: Hydrothermal Venture ---
--
-- You come across a field of hydrothermal vents on the ocean floor! These
-- vents constantly produce large, opaque clouds, so it would be best to avoid
-- them if possible.
--
-- They tend to form in lines; the submarine helpfully produces a list of
-- nearby lines of vents (your puzzle input) for you to review. For example:
--
--    0,9 -> 5,9
--    8,0 -> 0,8
--    9,4 -> 3,4
--    2,2 -> 2,1
--    7,0 -> 7,4
--    6,4 -> 2,0
--    0,9 -> 2,9
--    3,4 -> 1,4
--    0,0 -> 8,8
--    5,5 -> 8,2
--
-- Each line of vents is given as a line segment in the format x1,y1 -> x2,y2
-- where x1,y1 are the coordinates of one end the line segment and x2,y2 are
-- the coordinates of the other end. These line segments include the points at
-- both ends. In other words:
--
--    - An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
--    - An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
--
-- For now, only consider horizontal and vertical lines: lines where either
-- x1 = x2 or y1 = y2.
--
-- So, the horizontal and vertical lines from the above list would produce the
-- following diagram:
--
--    .......1..
--    ..1....1..
--    ..1....1..
--    .......1..
--    .112111211
--    ..........
--    ..........
--    ..........
--    ..........
--    222111....
--
-- In this diagram, the top left corner is 0,0 and the bottom right corner is
-- 9,9. Each position is shown as the number of lines which cover that point
-- or . if no line covers that point. The top-left pair of 1s, for example,
-- comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping
-- lines 0,9 -> 5,9 and 0,9 -> 2,9.
--
-- To avoid the most dangerous areas, you need to determine the number of
-- points where at least two lines overlap. In the above example, this is
-- anywhere in the diagram with a 2 or larger - a total of 5 points.
--
-- Consider only horizontal and vertical lines. At how many points do at least
-- two lines overlap?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Day_05; use Day_05;

procedure Day_05_Part_1 is
   Data : Vectors.Vector;
   Map : array (Number_Type'Range, Number_Type'Range) of Natural := (
      (others => (others => 0)));
   From, To : Number_Type;
   Count : Natural := 0;
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

end Day_05_Part_1;
