with Ada.Command_Line;
with Ada.Text_IO;
with AoC;

procedure Main is
   Year : AoC.Year_Type;
   Day  : AoC.Day_Type;
   Part : AoC.Part_Type;
begin
   if Ada.Command_Line.Argument_Count /= 0 then
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Argument : constant String := Ada.Command_Line.Argument (I);
            From : Natural := Argument'First;
            To   : Natural := From;
         begin
            while To <= Argument'Length loop
               if Argument (To) = ":" then
                  To := To - 1;
                  exit;
               end if;
               To := To + 1;
            end loop;
            if From > To then
               Error ("No year given!");
               raise Continue;
            else
               Try_Year:
                  declare
                  begin
                     Year := AoC.Year_Type'Value (Argument (From .. To));

