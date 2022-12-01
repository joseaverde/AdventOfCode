-- --- Day 08 : Part Two ---
--
-- Through a little deduction, you should now be able to determine the
-- remaining digits. Consider again the first example above:
--
--    acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
--    cdfeb fcadb cdfeb cdbaf
--
-- After some careful analysis, the mapping between signal wires and segments
-- only make sense in the following configuration:
--
--     dddd
--    e    a
--    e    a
--     ffff
--    g    b
--    g    b
--     cccc
--
-- So, the unique signal patterns would correspond to the following digits:
--
--    - acedgfb: 8
--    - cdfbe: 5
--    - gcdfa: 2
--    - fbcad: 3
--    - dab: 7
--    - cefabd: 9
--    - cdfgeb: 6
--    - eafb: 4
--    - cagedb: 0
--    - ab: 1
--
-- Then, the four digits of the output value can be decoded:
--
--    - cdfeb: 5
--    - fcadb: 3
--    - cdfeb: 5
--    - cdbaf: 3
--
-- Therefore, the output value for this entry is 5353.
--
-- Following this same process for each entry in the second, larger example
-- above, the output value of each entry can be determined:
--
--    - fdgacbe cefdb cefbgd gcbe: 8394
--    - fcgedb cgb dgebacf gc: 9781
--    - cg cg fdcagb cbg: 1197
--    - efabcd cedba gadfec cb: 9361
--    - gecf egdcabf bgf bfgea: 4873
--    - gebdcfa ecba ca fadegcb: 8418
--    - cefg dcbef fcge gbcadfe: 4548
--    - ed bcgafe cdgba cbgef: 1625
--    - gbdfcae bgc cg cgb: 8717
--    - fgae cfgab fg bagce: 4315
--
-- Adding all of the output values in this larger example produces 61229.
--
-- For each entry, determine all of the wire/segment connections and decode the
-- four-digit out
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Day_08; use Day_08;

procedure Day_08_Part_2 is
   use Vectors;
   Input_Data : Vector;
   Sum : Natural := 0;

   function Image (Digit : Digit_Type) return String is
      Str : String(1 .. Digit'Length*3);
      C   : Natural := 1;
   begin
      for S in Digit'Range loop
         if Digit(S) then
            Str(C .. C+2) := S'Image;
            C := C + 3;
         end if;
      end loop;
      return Str(1 .. C-1);
   end Image;

   function Decode (Input : Input_Case) return Integer is
      Decoded : Digit_Array := (others => (others => False));
      Array_0_6_9 : array (1 .. 3) of Digit_Type; Count_6 : Natural := 1;
      Array_2_3_5 : array (1 .. 3) of Digit_Type; Count_5 : Natural := 1;
      Status : Boolean;
   begin
      -- We first find the easy ones 1, 4, 7 and 8.
      -- We also separate the other ones: (0, 6, 9) and (3, 2, 5)
      for Digit of Input.Glyphs loop
         case Get_Count(Digit) is
            when 2 => Decoded(1) := Digit;
            when 4 => Decoded(4) := Digit;
            when 3 => Decoded(7) := Digit;
            when 7 => Decoded(8) := Digit;
            when 6 => Array_0_6_9(Count_6) := Digit; Count_6 := Count_6 + 1;
            when 5 => Array_2_3_5(Count_5) := Digit; Count_5 := Count_5 + 1;
            when others => raise Constraint_Error;
         end case;
      end loop;
      -- Guessing 3 is quite simple because is the only one with 5 segments
      -- to share two segments with 1.
      for Digit of Array_2_3_5 loop
         Status := True;
         for Segment in Decoded(1)'Range loop
            if Decoded(1)(Segment) and then not Digit(Segment) then
               Status := False;
            end if;
         end loop;
         if Status then
            Decoded(3) := Digit;
            exit;
         end if;
      end loop;
      -- We can use the glyphs 4 and 1 and take the segments that are in 4
      -- and not in 1 to get glyph 5 (and by discarding we can also get 2).
      for Digit of Array_2_3_5 loop
         Status := True;
         for Segment in Digit'Range loop
            if Digit = Decoded(1) then
               Status := False;
               exit;
            end if;
            if (Decoded(4)(Segment) and not Decoded(1)(Segment)) and then
               not Digit(Segment)
            then
               Status := False;
            end if;
         end loop;
         if Status then
            Decoded(5) := Digit;
            exit;
         end if;
      end loop;
      -- Finally we can get glyph 2.
      for Digit of Array_2_3_5 loop
         if Digit /= Decoded(5) and Digit /= Decoded(3) then
            Decoded(2) := Digit;
            exit;
         end if;
      end loop;

      -- Using a similar method we can get 6, 9 and 0. Number 5 matches 6
      -- and 9. And 1 matches 0 and 9.
      for Digit of Array_0_6_9 loop
         Status := True;
         for Segment in Digit'Range loop
            if (Decoded(1)(Segment) or Decoded(5)(Segment)) and then
               not Digit(Segment)
            then
               Status := False;
               exit;
            end if;
         end loop;
         if Status then
            Decoded(9) := Digit;
            exit;
         end if;
      end loop;
      -- If the next number matches 1 and isn't 9, then it's 0.
      for Digit of Array_0_6_9 loop
         Status := True;
         for Segment in DIgit'Range loop
            if Digit = Decoded(9) then
               Status := False;
               exit;
            end if;
            if Decoded(1)(Segment) and then not Digit(Segment) then
               Status := False;
            end if;
         end loop;
         if Status then
            Decoded(0) := Digit;
            exit;
         end if;
      end loop;
      -- Finally we can get 6.
      for Digit of Array_0_6_9 loop
         if Digit /= Decoded(0) and Digit /= Decoded(9) then
            Decoded(6) := Digit;
         end if;
      end loop;

      -- Once we have all the digits decoded we can get the number.
      Decode_Number:
         declare
            Number : Integer := 0;
         begin
            for I in Input.Question'Range loop
               for G in Decoded'Range loop
                  if Decoded(G) = Input.Question(I) then
                     Number := Number + G*10**(Input.Question'Last - I);
                     exit;
                  end if;
               end loop;
            end loop;
            return Number;
         end Decode_Number;
   end Decode;
begin

   Load_Data(Input_Data);

   for Input of Input_Data loop
      Sum := Sum + Decode(Input);
   end loop;

   Ada.Integer_Text_IO.Put(Sum, Width => 1);
   Ada.Text_IO.New_Line;

end Day_08_Part_2;
