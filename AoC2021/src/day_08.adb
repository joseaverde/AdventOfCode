with Ada.Text_IO;
with Ada.Strings.Bounded;

package body Day_08 is

   type Segment_Array is array (Positive range <>) of Segment_Type;
   function Convert (Str : String) return Segment_Array is
      Next : Segment_Type;
      Seg  : Segment_Array(Str'Range);
   begin
      for C in Str'Range loop
         case Str(C) is
            when 'a' => Next := 'a';
            when 'b' => Next := 'b';
            when 'c' => Next := 'c';
            when 'd' => Next := 'd';
            when 'e' => Next := 'e';
            when 'f' => Next := 'f';
            when 'g' => Next := 'g';
            when others => raise Constraint_Error with "Invalid segment; "&Str;
         end case;
         Seg(C) := Next;
      end loop;
      return Seg;
   end Convert;


   function Get_Count (Digit : Digit_Type) return Natural is
      Count : Natural := 0;
   begin
      for Segment of Digit loop
         if Segment then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Get_Count;


   procedure Load_Data (Data : in out Vector) is
      package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(1000);
      use Strings;
      Line : Bounded_String;
      File : Ada.Text_IO.File_Type;
      Input : Input_Case;
      From : Natural;
      To   : Natural;
   begin
      Ada.Text_IO.Open(
         File => File,
         Name => "inputs/day_08.input",
         Mode => Ada.Text_IO.In_File
      );
      while not Ada.Text_IO.End_Of_File(File) loop
         Line := To_Bounded_String(Ada.Text_IO.Get_Line(File));
         Input := Input_Case'(
            Glyphs   => (others => (others => False)),
            Question => (others => (others => False))
         );
         From := 1;
         for Digit in Input.Glyphs'Range loop
            To := Index(Line, " ", From);
            for Char of Convert(Slice(Line, From, To - 1)) loop
               Input.Glyphs(Digit)(Char) := True;
            end loop;
            From := To + 1;
         end loop;
         From := Index(Line, "|", From) + 2;
         for Digit in Input.Question'Range loop
            if Digit = 4 then
               To := Length(Line);
            else
               To := Index(Line, " ", From) - 1;
            end if;
            for Char of Convert(Slice(Line, From, To)) loop
               Input.Question(Digit)(Char) := True;
            end loop;
            From := To + 2;
         end loop;
         Data.Append(Input);
      end loop;
      Ada.Text_IO.Close(File);
   end Load_Data;

end Day_08;
