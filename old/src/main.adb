-- ======== main.adb ======== --
--
--    This is the main program you can run any solution from here. The usage is
-- simple, if no argument is provided then all the challenges will be runned in
-- order. You can provide the years and specify days and parts:
--
--       ./aot-ada 2008 2012:1,2,3,5 2007:1.1,1.2,3.2
--
--    The input must be located in either the directory of the executable or
-- the directory immediately on top. The folder name should be "data/" and
-- contain subfolders like "aot2020/" or "aot2021/". In this subfolders the
-- input will be "day_01.input", "day_02.input" and so on.
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AoC;

procedure Main is
   type Selection_Type is array (
      AoC.Year_Type'Range,
      AoC.Day_Type'Range,
      AoC.Part_Type'Range)
      of Boolean
      with Default_Component_Value => False;

   Show : Selection_Type;
begin

   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         use type AoC.Floats.Float_Type;
         Str   : AoC.Strings.String_Type;
         Year  : AoC.Year_Type;
         Value : AoC.Floats.Float_Type;
         Diff  : AoC.Floats.Float_Type;
      begin
         Str := AoC.Strings.To_String_Type(Ada.Command_Line.Argument(I));
         Year := Str.Parse(':');
         if Str.End_Of_String then
            for Day in AoC.Day_Type'Range loop
               for Part in AoC.Part_Type'Range loop
                  Show(Year, Day, Part) := True;
               end loop;
            end loop;
         else
            while not Str.End_Of_String loop
               Value := Str.Parse(',');
               Diff := Value - AoC.Floats.Float_Type'Floor(Value);
               Diff := AoC.Floats.Float_Type'Rounding(Diff * 10.0);
               if Diff = 0.0 then
                  Show(Year, AoC.Day_Type(Value), 1) := True;
                  Show(Year, AoC.Day_Type(Value), 2) := True;
               elsif Diff = 1.0 then
                  Show(Year, AoC.Day_Type(Value), 1) := True;
               elsif Diff = 2.0 then
                  Show(Year, AoC.Day_Type(Value), 2) := True;
               else
                  raise AoC.Strings.Parsing_Error with
                     "Invalid part" & Integer(Diff)'Image & "!";
               end if;
            end loop;
         end if;
      exception
         when Constraint_Error =>
            AoC.IO.Put("Invalid value: " & Ada.Command_Line.Argument(I));
      end;
   end loop;

end Main;
