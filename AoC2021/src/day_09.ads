package Day_09 is

   subtype Depth_Type is Integer range 0 .. 9;
   type Map_Type is array (0 .. 99, 0 .. 99) of Depth_Type;

   procedure Load_Data (Map : in out Map_Type);

end Day_09;
