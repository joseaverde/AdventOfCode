with Ada.Containers.Vectors;
with Ada.Strings.Bounded;

package Day_06 is

   subtype Day_Count is Natural range 0 .. 8;
   package Vectors is new Ada.Containers.Vectors(
      Index_Type   => Positive,
      Element_Type => Day_Count
   );
   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(1000);

   use Strings;
   use Vectors;

   procedure Load_Data (Lanternfish : in out Vector);

end Day_06;
