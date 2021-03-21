with Ada.Command_Line;
with Ada.Numerics.Long_Long_Elementary_Functions;
with Ada.Text_IO;

with Event_Device;

procedure Main is
   package LLEF renames Ada.Numerics.Long_Long_Elementary_Functions;
   package LLF_IO is new Ada.Text_IO.Float_IO (Long_Long_Float);

   EF : Event_Device.Input_Device;
   Item : Event_Device.Position;
begin
   EF.Open (Ada.Command_Line.Argument (1));

   Ada.Text_IO.Put_Line ("file name:   '" & EF.File_Name & "'");
   Ada.Text_IO.Put_Line ("device name: '" & EF.Name & "'");
   declare
      Info : Event_Device.Device_Info := EF.Device;
   begin
      Ada.Text_IO.Put_Line
        ("bus ven pro ver: " & Event_Device.Hex_Image (Info.Bus_Type) &
         " " & Event_Device.Hex_Image (Info.Vendor) &
         " " & Event_Device.Hex_Image (Info.Product) &
         " " & Event_Device.Hex_Image (Info.Version));
   end;
   loop
      EF.Read (Item);
      declare
         L : constant Long_Long_Float :=
           1.0 - LLEF.Sqrt (Long_Long_Float (Item.X * Item.X + Item.Y * Item.Y + Item.Z * Item.Z));

         L_I : String := "-0.00000000";
      begin
         LLF_IO.Put (L_I, Item => L, Aft => 8, Exp => 0);
         Ada.Text_IO.Put_Line
           (Item.Time'Image & " A " & Item.X'Image & Item.Y'Image & Item.Z'Image & L_I);
      end;
   end loop;
end Main;
