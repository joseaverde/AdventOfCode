with Ada.Containers.Vectors;

package Day_08 is

   type Segment_Type is ('a', 'b', 'c', 'd', 'e', 'f', 'g');
   type Digit_Type is array (Segment_Type'Range) of Boolean;
   type Digit_Array is array (0 .. 9) of Digit_Type;
   type Decode_Array is array (1 .. 4) of Digit_Type;

   type Input_Case is
      record
         Glyphs   : Digit_Array;
         Question : Decode_Array;
      end record;

   function Get_Count (Digit : Digit_Type) return Natural;

   package Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Input_Case
   );
   use Vectors;

   procedure Load_Data (Data : in out Vector);

end Day_08;
