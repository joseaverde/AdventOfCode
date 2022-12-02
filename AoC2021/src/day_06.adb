with Ada.Text_IO;

package body Day_06 is

   procedure Load_Data (Lanternfish : in out Vector) is
      File : Ada.Text_IO.File_Type;
      Input : Bounded_String;
      From : Natural := 1;
      To : Natural;
   begin
      Ada.Text_IO.Open(
         File => File,
         Mode => Ada.Text_IO.In_File,
         Name => "inputs/day_06.input"
      );
      Input := To_Bounded_String(Ada.Text_IO.Get_Line(File));
      Ada.Text_IO.Close(File);
      loop
         To := Index(Input, ",", From);
         Lanternfish.Append(Day_Count'Value(Slice(Input, From, To-1)));
         From := To + 1;
      end loop;
   exception
      when Constraint_Error =>
         Lanternfish.Append(Day_Count'Value(Slice(Input,From,Length(Input))));
   end Load_Data;

end Day_06;
