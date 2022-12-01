-- ======== aoc.adb ======== --
--
-- AUTHOR : José Antonio Verde Jiménez
--

package body AoC is

   package body Integers is
      function Image (Integer : Integer_Type)
         return String
      is
         Length : Natural := 1;
         Number : constant Integer_Type := abs Integer;
      begin
         while Number / 10**Length /= 0 loop
            Length := Length + 1;
         end loop;

         Get_String:
            declare
               Str  : String (1 .. Length);
               Imgs : constant array (Integer_Type range 0 .. 9) of Character
                  := "0123456789";
            begin
               for I in Str'Range loop
                  Str(I) := Imgs((Number/Integer_Type(10**(Length-I))) mod 10);
               end loop;

               if Integer < 0 then
                  return '-' & Str;
               else
                  return Str;
               end if;
            end Get_String;
      end Image;
   end Integers;



   package body Floats is
      function Image (Float : Float_Type)
         return String is (Float'Image);
   end Floats;



   package body Strings is
      use Ada.Strings.Unbounded;

      function To_String_Type (Str : String)
         return String_Type is (
            Value => To_Unbounded_String(Str),
            Index => 1
         );

      function To_String (Str : String_Type)
         return String is
         (To_String(Str.Value));


      procedure Reset (String : in out String_Type) is
      begin
         String.Index := 1;
      end Reset;

      function Parse (Str : in out String_Type;
         Delimiter : Character := ' ')
         return String
      is
         Found : Integer;
      begin
         if Str.End_Of_String then
            raise Parsing_Error with "End of String!";
         end if;
         Try_Index_Delimiter:
            declare
            begin
               Found := Index(
                  Source  => Str.Value,
                  Pattern => Delimiter & "",
                  From    => Str.Index
               );
               if Found = 0 then
                  Found := Length(Str.Value) + 1;
               end if;
            end Try_Index_Delimiter;
         Return_String:
            declare
               Substring : constant String := Slice(
                  Str.Value, Str.Index, Found - 1);
            begin
               Str.Index := Found + 1;
               return Substring;
            end Return_String;
      end Parse;

      function Parse (String : in out String_Type;
         Delimiter : Character := ' ')
         return Integers.Integer_Type is
      begin
         return Integers.Integer_Type'Value(String.Parse(Delimiter));
      exception
         when Constraint_Error =>
            raise Parsing_Error with "Couldn't parse integer!";
      end Parse;

      function Parse (String : in out String_Type;
         Delimiter : Character := ' ')
         return String_Type is
      begin
         return Strings.To_String_Type(String.Parse);
      end Parse;

      function Parse (String : in out String_Type;
         Delimiter : Character := ' ')
         return Floats.Float_Type is
      begin
         return Floats.Float_Type'Value(String.Parse(Delimiter));
      exception
         when Constraint_Error =>
            raise Parsing_Error with "Couldn't parse float!";
      end Parse;

      function Parse_Enum (String : in out String_Type;
         Delimiter : Character := ' ')
         return Enum_Type is
      begin
         return Enum_Type'Value(String.Parse(Delimiter));
      exception
         when Constraint_Error =>
            raise Parsing_Error with "Couldn't parse enum value!";
      end Parse_Enum;


      function End_Of_String (String : in out String_Type)
         return Boolean is
         (String.Index > Length(String.Value));
   end Strings;



   package body IO is
      Escape : constant String := ASCII.ESC & '[';

      procedure Clear is
      begin
         Ada.Text_IO.Put(Escape & "0m");
      end Clear;

      procedure Bold is
      begin
         Ada.Text_IO.Put(Escape & "1m");
      end Bold;

      procedure Put (Colour : Colour_Type) is
         Imgs : constant array (Colour_Type'Range) of Character := "01234567";
      begin
         Ada.Text_IO.Put(Escape & '3' & Imgs(Colour) & 'm');
      end Put;

      procedure Put (Integer : Integers.Integer_Type) is
      begin
         Ada.Text_IO.Put(Integers.Image(Integer));
      end Put;

      procedure Put (Float : Floats.Float_Type) is
      begin
         Ada.Text_IO.Put(Floats.Image(Float));
      end Put;

      procedure Put (String : Strings.String_Type) is
      begin
         Ada.Text_IO.Put(Strings.To_String(String));
      end Put;

      procedure Put (Str : String) is
      begin
         Ada.Text_IO.Put(Str);
      end Put;

      procedure New_Line is
      begin
         Ada.Text_IO.New_Line;
      end New_Line;


      procedure Open (File : in out File_Type) is null;
      procedure Close (File : in out File_Type) is null;
      function Get_Line (File : in out File_Type)
 
      return Strings.String_Type is (Strings.To_String_Type(""));
   end IO;

end AoC;
