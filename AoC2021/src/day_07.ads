with Ada.Containers.Vectors;
with Ada.Strings.Bounded;

package Day_07 is

   package Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Natural
   );

   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(10_000);

   use Vectors;
   use Strings;

   procedure Load_Data (Data : in out Vector;
      Min : out Natural;
      Max : out Natural
   );

end Day_07;
