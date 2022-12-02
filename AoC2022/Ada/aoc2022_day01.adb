with Ada.Text_IO;

procedure AoC2022_Day01 is
   Best_1 : Natural := 0;
   Best_2 : Natural := 0;
   Best_3 : Natural := 0;
   Sum    : Natural;
   procedure Update is
   begin
      if Sum > Best_1 then
         Best_3 := Best_2;
         Best_2 := Best_1;
         Best_1 := Sum;
      elsif Sum > Best_2 then
         Best_3 := Best_2;
         Best_2 := Sum;
      elsif Sum > Best_3 then
         Best_3 := Sum;
      end if;
   end Update;
begin
   Sum := 0;
   loop
      declare
         S : constant String := Ada.Text_IO.Get_Line;
      begin
         if S'Length = 0 then
            Update;
            Sum := 0;
         else
            Sum := Sum + Natural'Value (S);
         end if;
      end declare;
   end loop;
exception
   when Ada.Text_IO.End_Error =>
      Sum := Best_1 + Best_2 + Best_3;
      Ada.Text_IO.Put_Line ("AoC 2022 Day 01 Part 1 ->" & Best_1'Image);
      Ada.Text_IO.Put_Line ("AoC 2022 Day 01 Part 2 ->" & Sum'Image);
end AoC2022_Day01;
