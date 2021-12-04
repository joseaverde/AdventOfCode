with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;

package body Day_04 is

   procedure Load_Data (
      Bingo_Boards : in out Bingo_Board_Vectors.Vector;
      Order        : in out Number_Vectors.Vector)
   is
      Board : Bingo_Board;
      File  : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open(
         File => File,
         Name => "inputs/day_04.input",
         Mode => Ada.Text_IO.In_File
      );

      Get_Order:
         declare
            Data  : Unbounded_String :=
               To_Unbounded_String(Ada.Text_IO.Get_Line(File));
            Comma : Natural := 1;
            Next  : Natural := 0;
         begin
            loop
               Next := Index(
                  Source  => Data,
                  Pattern => ",",
                  From    => Comma
               );
               Order.Append(Bingo_Number'Value(Slice(Data, Comma, Next - 1)));
               Comma := Next + 1;
            end loop;
         exception
            when Constraint_Error =>
               Order.Append(Bingo_Number'Value(
                  Slice(Data, Comma, Length(Data))));
         end Get_Order;

      while not Ada.Text_IO.End_Of_File(File) loop
         Ada.Text_IO.Skip_Line(File);
         for R in Bingo_Board'Range(1) loop
            for C in Bingo_Board'Range(2) loop
               Ada.Integer_Text_IO.Get(File, Board(R, C).Number);
            end loop;
         end loop;
         Bingo_Boards.Append(Board);
      end loop;

   end Load_Data;


   function Check_Bingo (Board : Bingo_Board ; R, C : Integer) return Boolean
   is
      Line : Boolean := True;
   begin
      for I in Board'Range(1) loop
         if not Board(I, C).Checked then
            Line := False;
            exit;
         end if;
      end loop;
      if Line then
         return True;
      end if;
      for I in Board'Range(2) loop
         if not Board(R, I).Checked then
            return False;
         end if;
      end loop;
      return True;
   end Check_Bingo;

   function Get_Score (Board : Bingo_Board ; Last : Integer) return Integer is
      Count : Integer := 0;
   begin
      for R in Board'Range(1) loop
         for C in Board'Range(2) loop
            if not Board(R, C).Checked then
               Count := Count + Board(R, C).Number;
            end if;
         end loop;
      end loop;
      return Count * Last;
   end Get_Score;

end Day_04;
