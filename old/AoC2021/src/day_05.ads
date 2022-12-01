with Ada.Containers.Vectors;

package Day_05 is

   subtype Number_Type is Integer range 0 .. 999;
   type Coord_Axis is (X, Y);
   type Point_Type is array (Coord_Axis'Range) of Number_Type;
   type Line_Type is array (1 .. 2) of Point_Type;

   package Vectors is new Ada.Containers.Vectors(
      Index_Type   => Positive,
      Element_Type => Line_Type
   );

   procedure Load_Data (Data : in out Vectors.Vector);
   procedure Increase (N : in out Natural);

end Day_05;
