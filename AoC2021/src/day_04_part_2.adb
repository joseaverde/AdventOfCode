-- --- Part Two ---
--
-- On the other hand, it might be wise to try a different strategy: let the
-- giant squid win.
--
-- You aren't sure how many bingo boards a giant squid could play at once, so
-- rather than waste time counting its arms, the safe thing to do is to figure
-- out which board will win last and choose that one. That way, no matter which
-- boards it picks, it will win for sure.
--
-- In the above example, the second board is the last to win, which happens
-- after 13 is eventually called and its middle column is completely marked. If
-- you were to keep playing until this point, the second board would have a sum
-- of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
--
-- Figure out which board will win last. Once it wins, what would its final score be?
--
-- AUTHOR : José Antonio Verde Jiménez
--

with Ada.Integer_Text_IO;
With Ada.Text_IO;
with Day_04; use Day_04;

procedure Day_04_Part_2 is
   Last_Board : Bingo_Board;
   Last_Number : Bingo_Number;
   Bingo_Boards : Bingo_Board_Vectors.Vector;
   Order : Number_Vectors.Vector;
   Cursor : Bingo_Board_Vectors.Cursor;
   Temp   : Bingo_Board_Vectors.Cursor := Bingo_Board_Vectors.No_Element;
begin

   Load_Data(Bingo_Boards, Order);
   
   for Number of Order loop
      Cursor := Bingo_Board_Vectors.Last(Bingo_Boards);
      while Bingo_Board_Vectors.Has_Element(Cursor) loop
         Row_Loop: for R in Bingo_Board'Range(1) loop
            for C in Bingo_Board'Range(2) loop
               if Bingo_Boards(Cursor)(R, C).Number = Number then
                  Bingo_Boards(Cursor)(R, C).Checked := True;
                  if Check_Bingo(Bingo_Boards(Cursor), R, C) then
                     Last_Board := Bingo_Boards(Cursor);
                     Last_Number := Number;
                     Temp := Cursor;
                     Cursor := Bingo_Board_Vectors.Previous(Cursor);
                     exit Row_Loop;
                  end if;
               end if;
            end loop;
         end loop Row_Loop;
         if Bingo_Board_Vectors.Has_Element(Temp) then
            Bingo_Boards.Delete(Temp);
         else
            Bingo_Board_Vectors.Previous(Cursor);
         end if;
      end loop;
   end loop;


   Ada.Integer_Text_IO.Put(Get_Score(Last_Board, Last_Number), Width => 1);
   Ada.Text_IO.New_Line;

end Day_04_Part_2;
