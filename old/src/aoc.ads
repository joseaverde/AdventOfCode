-- ======== aoc.ads ======== --
--
--    This is package contains a set of type I created for parsing strings
-- and generalising I/O for all the exercises from AdventOfCode.
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

package AoC is

   -- Integers are as big as possible.
   package Integers is
      type Integer_Type is range -2**63 .. 2**63-1 with Size => 64;
      function Image (Integer : Integer_Type)
         return String;
   end Integers;

   subtype Year_Type is Integers.Integer_Type range 2015 .. 2021;
   subtype Day_Type is Integers.Integer_Type range 1 .. 25;
   subtype Part_Type is Integers.Integer_Type range 1 .. 2;


   -- The same goes for float types.
   package Floats is
      type Float_Type is new Long_Long_Float;
      function Image (Float : Float_Type)
         return String;
   end Floats;


   -- Strings can be parsed.
   package Strings is
      Parsing_Error : exception;

      type String_Type is tagged private;

      function To_String_Type (Str : String)
         return String_Type;

      function To_String (Str : String_Type)
         return String;


      procedure Reset (String : in out String_Type);

      function Parse (Str : in out String_Type;
         Delimiter : Character := ' ')
         return String;

      function Parse (String : in out String_Type;
         Delimiter : Character := ' ')
         return Integers.Integer_Type;

      function Parse (String : in out String_Type;
         Delimiter : Character := ' ')
         return String_Type;

      function Parse (String : in out String_Type;
         Delimiter : Character := ' ')
         return Floats.Float_Type;

      generic
         type Enum_Type is (<>);
      function Parse_Enum (String : in out String_Type;
         Delimiter : Character := ' ')
         return Enum_Type;


      function End_Of_String (String : in out String_Type)
         return Boolean;

   private
      type String_Type is tagged
         record
            Value : Ada.Strings.Unbounded.Unbounded_String;
            Index : Natural := 1;
         end record;
   end Strings;


   package IO is
      type Colour_Type is (
         Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);

      procedure Clear;
      procedure Bold;
      procedure Put (Colour : Colour_Type);
      procedure Put (Integer : Integers.Integer_Type);
      procedure Put (Float : Floats.Float_Type);
      procedure Put (String : Strings.String_Type);
      procedure Put (Str : String);
      procedure New_Line;

      type File_Type (
         Year : Year_Type;
         Day  : Day_Type) is
         tagged limited private;
      procedure Open (File : in out File_Type);
      procedure Close (File : in out File_Type);
      function Get_Line (File : in out File_Type)
         return Strings.String_Type;

   private
      type File_Type (
         Year : Year_Type;
         Day  : Day_Type) is
         tagged limited record
            File : Ada.Text_IO.File_Type;
         end record;
   end IO;

end AoC;
