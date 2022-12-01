package body Day_10 is

   procedure Push (Stack : in out Stack_Type;
      Item : Character) is
   begin
      Stack.Top := new Node_Type'(
         Value => Item,
         Next  => Stack.Top);
   end Push;

   function Pop (Stack : in out Stack_Type)
      return Character
   is
      Temp  : Node_Access;
      Value : Character;
   begin
      if Stack.Top = null then
         raise Underflow_Error;
      else
         Temp := Stack.Top;
         Value := Temp.Value;
         Stack.Top := Stack.Top.Next;
         Free(Temp);
         return Value;
      end if;
   end Pop;

   function Length (Stack : in Stack_Type)
      return Natural
   is
      Next  : Node_Access := Stack.Top;
      Count : Natural := 0;
   begin
      while Next /= null loop
         Count := Count + 1;
         Next := Next.Next;
      end loop;
      return Count;
   end Length;

   function Is_Empty (Stack : in Stack_Type)
      return Boolean is (Stack.Top = null);
end Day_10;
