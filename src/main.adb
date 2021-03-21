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
   Ada.Text_IO.Put_Line ("location:    '" & EF.Location & "'");
   Ada.Text_IO.Put_Line ("uniq ID:     '" & EF.Unique_ID & "'");
   declare
      Info  : constant Event_Device.Device_ID := EF.ID;
      Props : constant Event_Device.Device_Properties := EF.Properties;
   begin
      Ada.Text_IO.Put_Line
        ("bus ven pro ver: " & Event_Device.Hex_Image (Info.Bus_Type) &
         " " & Event_Device.Hex_Image (Info.Vendor) &
         " " & Event_Device.Hex_Image (Info.Product) &
         " " & Event_Device.Hex_Image (Info.Version));
      Ada.Text_IO.Put_Line ("properties:");
      Ada.Text_IO.Put_Line ("  pointer:        " & Props.Pointer'Image);
      Ada.Text_IO.Put_Line ("  direct:         " & Props.Direct'Image);
      Ada.Text_IO.Put_Line ("  button pad:     " & Props.Button_Pad'Image);
      Ada.Text_IO.Put_Line ("  semi mt:        " & Props.Semi_Multi_Touch'Image);
      Ada.Text_IO.Put_Line ("  top button pad: " & Props.Top_Button_Pad'Image);
      Ada.Text_IO.Put_Line ("  pointing stick: " & Props.Pointing_Stick'Image);
      Ada.Text_IO.Put_Line ("  accelerometer:  " & Props.Accelerometer'Image);
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
