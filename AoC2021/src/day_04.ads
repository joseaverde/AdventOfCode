-- This package is shared by both parts of day 4.

with Ada.Containers.Vectors;

package Day_04 is
   subtype Bingo_Number is Integer range 0 .. 99;
   type Bingo_Field is
      record
         Number  : Bingo_Number;
         Checked : Boolean := False;
      end record;
   type Bingo_Board is array (1 .. 5, 1 .. 5) of Bingo_Field;
   package Bingo_Board_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Bingo_Board
   );
   package Number_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Bingo_Number
   );

   procedure Load_Data (
      Bingo_Boards : in out Bingo_Board_Vectors.Vector;
      Order        : in out Number_Vectors.Vector
   );

   function Check_Bingo (Board : Bingo_Board ; R, C : Integer) return Boolean;
   function Get_Score (Board : Bingo_Board ; Last : Integer) return Integer;

end Day_04;
