with Ada.Unchecked_Deallocation;

package Day_10 is

   Underflow_Error : exception;

   type Stack_Type is tagged private;
   procedure Push (Stack : in out Stack_Type;
      Item : Character);
   function Pop (Stack : in out Stack_Type)
      return Character;
   function Length (Stack : in Stack_Type)
      return Natural;
   function Is_Empty (Stack : in Stack_Type)
      return Boolean;

private

   type Node_Type;
   type Node_Access is access Node_Type;
   type Node_Type is
      record
         Value : Character;
         Next  : Node_Access := null;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation(
      Object => Node_Type,
      Name   => Node_Access
   );

   type Stack_Type is tagged
      record
         Top : Node_Access := null;
      end record;

end Day_10;
