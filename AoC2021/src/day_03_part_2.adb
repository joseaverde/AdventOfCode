-- --- Day 03 : Part Two ---
--
-- Next, you should verify the life support rating, which can be determined by
-- multiplying the oxygen generator rating by the CO2 scrubber rating.
--
-- Both the oxygen generator rating and the CO2 scrubber rating are values that
-- can be found in your diagnostic report - finding them is the tricky part.
-- Both values are located using a similar process that involves filtering out
-- values until only one remains. Before searching for either rating value,
-- start with the full list of binary numbers from your diagnostic report and
-- consider just the first bit of those numbers. Then:
--
--    - Keep only numbers selected by the bit criteria for the type of rating
--      value for which you are searching. Discard numbers which do not match
--      the bit criteria.
--    - If you only have one number left, stop; this is the rating value for
--      which you are searching.
--    - Otherwise, repeat the process, considering the next bit to the right.
--
-- The bit criteria depends on which type of rating value you want to find:
--
--    - To find oxygen generator rating, determine the most common value (0 or
--      1) in the current bit position, and keep only numbers with that bit in
--      that position. If 0 and 1 are equally common, keep values with a 1 in
--      the position being considered.
--    - To find CO2 scrubber rating, determine the least common value (0 or 1)
--      in the current bit position, and keep only numbers with that bit in
--      that position. If 0 and 1 are equally common, keep values with a 0 in
--      the position being considered.
--
-- For example, to determine the oxygen generator rating value using the same
-- example diagnostic report from above:
--
--    - Start with all 12 numbers and consider only the first bit of each
--      number. There are more 1 bits (7) than 0 bits (5), so keep only the 7
--      numbers with a 1 in the first position: 11110, 10110, 10111, 10101,
--      11100, 10000, and 11001.
--    - Then, consider the second bit of the 7 remaining numbers: there are
--      more 0 bits (4) than 1 bits (3), so keep only the 4 numbers with a 0 in
--      the second position: 10110, 10111, 10101, and 10000.
--    - In the third position, three of the four numbers have a 1, so keep
--      those three: 10110, 10111, and 10101.
--    - In the fourth position, two of the three numbers have a 1, so keep
--      those two: 10110 and 10111.
--    - In the fifth position, there are an equal number of 0 bits and 1 bits
--      (one each). So, to find the oxygen generator rating, keep the number
--      with a 1 in that position: 10111.
--    - As there is only one number left, stop; the oxygen generator rating is
--      10111, or 23 in decimal.
--
-- Then, to determine the CO2 scrubber rating value from the same example
-- above:
--
--    - Start again with all 12 numbers and consider only the first bit of each
--      number. There are fewer 0 bits (5) than 1 bits (7), so keep only the 5
--      numbers with a 0 in the first position: 00100, 01111, 00111, 00010, and
--      01010.
--    - Then, consider the second bit of the 5 remaining numbers: there are
--      fewer 1 bits (2) than 0 bits (3), so keep only the 2 numbers with a 1
--      in the second position: 01111 and 01010.
--    - In the third position, there are an equal number of 0 bits and 1 bits
--      (one each). So, to find the CO2 scrubber rating, keep the number with
--      a 0 in that position: 01010.
--    - As there is only one number left, stop; the CO2 scrubber rating is
--      01010, or 10 in decimal.
--
-- Finally, to find the life support rating, multiply the oxygen generator
-- rating (23) by the CO2 scrubber rating (10) to get 230.
--
-- Use the binary numbers in your diagnostic report to calculate the oxygen
-- generator rating and CO2 scrubber rating, then multiply them together. What
-- is the life support rating of the submarine? (Be sure to represent your
-- answer in decimal, not binary.)
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Containers.Vectors;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Day_03_Part_2 is
   subtype Char_Count is Integer range 1 .. 12;
   subtype Data_Type is String (Char_Count'Range);

   package Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Data_Type
   );

   procedure Filter (Vector : in out Vectors.Vector ; Max : Boolean) is
      use type Ada.Containers.Count_Type;
      One_Count  : Natural;
      Zero_Count : Natural;
      F          : Character;
      Cursor     : Vectors.Cursor;
      Temp       : Vectors.Cursor;
   begin
      for C in Char_Count'Range loop
         One_Count := 0;
         Zero_Count := 0;
         for Data of Vector loop
            if Data(C) = '1' then
               One_Count := One_Count + 1;
            else
               Zero_Count := Zero_Count + 1;
            end if;
         end loop;
         if Max then
            if One_Count >= Zero_Count then
               F := '0';
            else
               F := '1';
            end if;
         else
            if One_Count < Zero_Count then
               F := '0';
            else
               F := '1';
            end if;
         end if;
         Cursor := Vectors.Last(Vector);
         while Vectors.Has_Element(Cursor) loop
            if Vectors.Element(Cursor)(C) = F then
               Temp := Cursor;
               Vectors.Previous(Cursor);
               Vectors.Delete(Vector, Temp);
            else
               Vectors.Previous(Cursor);
            end if;
         end loop;
         exit when Vectors.Length(Vector) = 1;
      end loop;
   end Filter;

   function To_Integer (Data : Data_Type) return Natural is
   begin
      return Result : Natural := 0
      do
         for I in Data'Range loop
            if Data(I) = '1' then
               Result := Result + 2**(Data'Last - I);
            end if;
         end loop;
      end return;
   end To_Integer;

   O2_Vector  : Vectors.Vector;
   CO2_Vector : Vectors.Vector;

   File : Ada.Text_IO.File_Type;
   Buffer : Data_Type;
begin

   Ada.Text_IO.Open(
      File => File,
      Mode => Ada.Text_IO.In_File,
      Name => "inputs/day_03.input"
   );

   while not Ada.Text_IO.End_Of_File(File) loop
      Buffer := Ada.Text_IO.Get_Line(File);
      O2_Vector.Append(Buffer);
      CO2_Vector.Append(Buffer);
   end loop;
   Ada.Text_IO.Close(File);

   Filter(O2_Vector, True);
   Filter(CO2_Vector, False);

   Ada.Integer_Text_IO.Put(
      To_Integer(Vectors.First_Element(O2_Vector)) *
      To_Integer(Vectors.First_Element(CO2_Vector)),
      1
   );
   Ada.Text_IO.New_Line;

end Day_03_Part_2;
