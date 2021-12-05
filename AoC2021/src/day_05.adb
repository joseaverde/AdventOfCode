with Ada.Integer_Text_IO;
with Ada.Strings.Bounded;
with Ada.Text_IO;

package body Day_05 is

   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length(500);

   procedure Load_Data (Data : in out Vectors.Vector) is
      use Strings;
      Buffer : Bounded_String;
      Last   : Natural := 1;
      Found  : Natural;
      Line   : Line_Type;
      File   : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open(
         File => File,
         Name => "inputs/day_05.input",
         Mode => Ada.Text_IO.In_File
      );

      while not Ada.Text_IO.End_Of_File(File) loop
         Buffer := To_Bounded_String(Ada.Text_IO.Get_Line(File));

         Found := Index(Buffer, ",", 1);
         Line(1)(X) := Number_Type'Value(Slice(Buffer, Last, Found - 1));
         Last := Found + 1;

         Found := Index(Buffer, " ", Last);
         Line(1)(Y) := Number_Type'Value(Slice(Buffer, Last, Found - 1));
         Last := Found + 1;

         Last := Index(Buffer, " ", Last) + 1;
         Found := Index(Buffer, ",", Last);
         Line(2)(X) := Number_Type'Value(Slice(Buffer, Last, Found - 1));
         Last := Found + 1;

         Found := Length(Buffer);
         Line(2)(Y) := Number_Type'Value(Slice(Buffer, Last, Found));

         Data.Append(Line);
         Last := 1;
      end loop;

      Ada.Text_IO.Close(File);
   end Load_Data;


   procedure Increase (N : in out Natural) is
   begin
      N := N + 1;
   end Increase;

end Day_05;
