with Ada.Text_IO;

package body Day_09 is

   procedure Load_Data (Map : in out Map_Type) is
      File : Ada.Text_IO.File_Type;
      Char : Character;
      X, Y : Integer := 0;
   begin
      Ada.Text_IO.Open(
         File => File,
         Mode => Ada.Text_IO.In_File,
         Name => "inputs/day_09.input"
      );
      while not Ada.Text_IO.End_Of_File(File) loop
         Ada.Text_IO.Get(File, Char);
         Map(Y, X) := Character'Pos(Char) - Character'Pos('0');
         X := X + 1;
         if X >= 100 then
            X := 0;
            Y := Y + 1;
         end if;
      end loop;
      Ada.Text_IO.Close(File);
   end Load_Data;

end Day_09;
