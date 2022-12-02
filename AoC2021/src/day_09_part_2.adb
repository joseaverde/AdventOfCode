-- --- Part Two ---
--
-- Next, you need to find the largest basins so you know what areas are most
-- important to avoid.
--
-- A basin is all locations that eventually flow downward to a single low
-- point. Therefore, every low point has a basin, although some basins are very
-- small. Locations of height 9 do not count as being in any basin, and all
-- other locations will always be part of exactly one basin.
--
-- The size of a basin is the number of locations within the basin, including
-- the low point. The example above has four basins.
--
-- The top-left basin, size 3:
--
--    2199943210
--    3987894921
--    9856789892
--    8767896789
--    9899965678
--
-- The top-right basin, size 9:
--
--    2199943210
--    3987894921
--    9856789892
--    8767896789
--    9899965678
--
-- The middle basin, size 14:
--
--    2199943210
--    3987894921
--    9856789892
--    8767896789
--    9899965678
--
-- The bottom-right basin, size 9:
--
--    2199943210
--    3987894921
--    9856789892
--    8767896789
--    9899965678
--
-- Find the three largest basins and multiply their sizes together. In the
-- above example, this is 9 * 14 * 9 = 1134.
--
-- What do you get if you multiply together the sizes of the three largest
-- basins?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Text_IO;
with Day_09; use Day_09;

procedure Day_09_Part_2 is
   Map    : Map_Type;
   Count  : Long_Integer := 0;
   Status : Boolean := True;
   Best_3 : array (1 .. 3) of Long_Integer := (others => 0);

   function Search (Map : Map_Type; Y, X : Integer) return Long_Integer is
      Visited : array (Map'Range(1), Map'Range(2)) of Boolean := (
         others => (others => False));
      function Inside_Search (Y, X : Integer) return Long_Integer is
         Count : Long_Integer := 1;
         function Check (Y, X, Diff_Y, Diff_X : Integer) return Boolean is
         begin
            if ((Y + Diff_Y) in Visited'Range(1) and
                (X + Diff_X) in Visited'Range(2)) and then
               (Map(Y+Diff_Y, X+Diff_X) > Map(Y, X) and
                Map(Y+Diff_Y, X+Diff_X) /= 9 and
                not Visited(Y+Diff_Y, X+Diff_X))
            then
               return True;
            else
               return False;
            end if;
         end Check;
      begin
         Visited(Y, X) := True;
         if Check(Y, X,  0, +1) then
            Count := Count + Inside_Search(Y, X + 1);
         end if;
         if Check(Y, X,  0, -1) then
            Count := Count + Inside_Search(Y, X - 1);
         end if;
         if Check(Y, X, +1,  0) then
            Count := Count + Inside_Search(Y + 1, X);
         end if;
         if Check(Y, X, -1,  0) then
            Count := Count + Inside_Search(Y - 1, X);
         end if;
         return Count;
      end Inside_Search;
   begin

      return Inside_Search(Y, X);

   end Search;
begin
   Load_Data(Map);

   for Y in Map'Range(1) loop
      for X in Map'Range(2) loop
         Status := True;
         if X /=  0 and then Map(Y, X - 1) <= Map(Y, X) then
            Status := False;
         end if;
         if X /= 99 and then Map(Y, X + 1) <= Map(Y, X) then
            Status := False;
         end if;
         if Y /=  0 and then Map(Y - 1, X) <= Map(Y, X) then
            Status := False;
         end if;
         if Y /= 99 and then Map(Y + 1, X) <= Map(Y, X) then
            Status := False;
         end if;
         if Status then
            -- It's a low point, we can search from here.
            Count := Search(Map, Y, X);
            for I in reverse Best_3'Range loop
               if Count > Best_3(I) then
                  for J in Best_3'First .. I - 1 loop
                     Best_3(J) := Best_3(J + 1);
                  end loop;
                  Best_3(I) := Count;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line(Long_Integer'Image(
      Best_3(1)*Best_3(2)*Best_3(3)));

end Day_09_Part_2;
