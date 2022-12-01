package AoC is

   Implementation_Error : exception;

   type Year_Type is range 2015 .. 2021;
   type Day_Type  is range 1 .. 25;
   type Part_Type is range 1 .. 2;

   type Number is new Long_Long_Integer;
   subtype Natural_Number  is Number range 0 .. Number'Last;
   subtype Positive_Number is Number range 1 .. Number'Last;
   subtype Integer_Number  is Number;

   function Is_Implemented (
      Year : Year_Type;
      Day  : Day_Type;
      Part : Part_Type)
      return Boolean;

   function Run_Test (
      Year : Year_Type;
      Day  : Day_Type;
      Part : Part_Type)
      return Number;

   function Run (
      Year : Year_Type;
      Day  : Day_Type;
      Part : Part_Type)
      return Number;

   type UString is tagged private;

   procedure Set (Str : in out UString; Value : String);
   function Get (Str : in UString) return String;

   function Next (Str : in out UString) return Character;
   function Prev (Str : in out UString) return Character;
   function Next (Str : in out UString; Amount : Positive) return String;
   function Prev (Str : in out UString; Amount : Positive) return String;
   procedure Skip (Str : in out UString; Amount : Positive);
   procedure Skip_To (Str : in out UString; Amount : Positive);

private

   type Challenge_Type is access function (Name : String) return Number;

   Challenges : array (Year_Type, Day_Type, Part_Type) of Challenge_Type :=
      (others => (others => (others => null)));

end AoC;
