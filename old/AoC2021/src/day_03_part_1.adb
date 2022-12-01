-- --- Day 3: Binary Diagnostic ---
--
-- The submarine has been making some odd creaking noises, so you ask it to
-- produce a diagnostic report just in case.
--
-- The diagnostic report (your puzzle input) consists of a list of binary
-- numbers which, when decoded properly, can tell you many useful things about
-- the conditions of the submarine. The first parameter to check is the power
-- consumption.
--
-- You need to use the binary numbers in the diagnostic report to generate two
-- new binary numbers (called the gamma rate and the epsilon rate). The power
-- consumption can then be found by multiplying the gamma rate by the epsilon
-- rate.
--
-- Each bit in the gamma rate can be determined by finding the most common bit
-- in the corresponding position of all numbers in the diagnostic report. For
-- example, given the following diagnostic report:
--
--    00100
--    11110
--    10110
--    10111
--    10101
--    01111
--    00111
--    11100
--    10000
--    11001
--    00010
--    01010
--
-- Considering only the first bit of each number, there are five 0 bits and
-- seven 1 bits. Since the most common bit is 1, the first bit of the gamma
-- rate is 1.
--
-- The most common second bit of the numbers in the diagnostic report is 0, so
-- the second bit of the gamma rate is 0.
--
-- The most common value of the third, fourth, and fifth bits are 1, 1, and 0,
-- respectively, and so the final three bits of the gamma rate are 110.
--
-- So, the gamma rate is the binary number 10110, or 22 in decimal.
--
-- The epsilon rate is calculated in a similar way; rather than use the most
-- common bit, the least common bit from each position is used. So, the epsilon
-- rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the
-- epsilon rate (9) produces the power consumption, 198.
--
-- Use the binary numbers in your diagnostic report to calculate the gamma rate
-- and epsilon rate, then multiply them together. What is the power consumption
-- of the submarine? (Be sure to represent your answer in decimal, not binary.)
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Day_03_Part_1 is
   Gamma_Rate   : Natural := 0;
   Epsilon_Rate : Natural := 0;
   subtype Char_Count is Integer range 1 .. 12;
   Bit_Count : array (Char_Count'Range, 0 .. 1) of Natural :=
      (others => (others => 0));

   File : Ada.Text_IO.File_Type;
   String_Buffer : String (Char_Count'Range);
begin

   Ada.Text_IO.Open(
      File => File,
      Name => "inputs/day_03.input",
      Mode => Ada.Text_IO.In_File
   );

   while not Ada.Text_IO.End_Of_File(File) loop
      String_Buffer := Ada.Text_IO.Get_Line(File);
      for I in Char_Count'Range loop
         if String_Buffer(I) = '0' then
            Bit_Count(I, 0) := Bit_Count(I, 0) + 1;
         else
            Bit_Count(I, 1) := Bit_Count(I, 1) + 1;
         end if;
      end loop;
   end loop;

   for I in Bit_Count'Range loop
      if Bit_Count(I, 0) > Bit_Count(I, 1) then
         Epsilon_Rate := Epsilon_Rate + 2**(Bit_Count'Last - I);
      else
         Gamma_Rate := Gamma_Rate + 2**(Bit_Count'Last - I);
      end if;
   end loop;

   Ada.Text_IO.Close(File);
   Ada.Integer_Text_IO.Put(Epsilon_Rate*Gamma_Rate, Width => 1);
   Ada.Text_IO.New_Line;

end Day_03_Part_1;
