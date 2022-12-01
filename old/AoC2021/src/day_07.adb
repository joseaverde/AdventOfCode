with Ada.Text_IO;

package body Day_07 is

   procedure Load_Data (Data : in out Vector;
      Min : out Natural;
      Max : out Natural)
   is
      File : Ada.Text_IO.File_Type;
      Input : Bounded_String;
      From : Natural := 1;
      To : Natural;
   begin
      Min := Natural'Last;
      Max := Natural'First;
      Ada.Text_IO.Open(
         File => File,
         Mode => Ada.Text_IO.In_File,
         Name => "inputs/day_07.input"
      );
      Input := To_Bounded_String(Ada.Text_IO.Get_Line(File));
      Ada.Text_IO.Close(File);
      loop
         To := Index(Input, ",", From);
         Data.Append(Natural'Value(Slice(Input, From, To-1)));
         if Data.Last_Element > Max then
            Max := Data.Last_Element;
         end if;
         if Data.Last_Element < Min then
            Min := Data.Last_Element;
         end if;
         From := To + 1;
      end loop;
   exception
      when Constraint_Error =>
         Data.Append(Natural'Value(Slice(Input,From,Length(Input))));
         if Data.Last_Element > Max then
            Max := Data.Last_Element;
         end if;
         if Data.Last_Element < Min then
            Min := Data.Last_Element;
         end if;
   end Load_Data;

end Day_07;
