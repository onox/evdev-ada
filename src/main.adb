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
      ID     : constant Event_Device.Device_ID := EF.ID;
      Props  : constant Event_Device.Device_Properties := EF.Properties;
      Events : constant Event_Device.Device_Events := EF.Events;
   begin
      Ada.Text_IO.Put_Line
        ("bus ven pro ver: " & Event_Device.Hex_Image (ID.Bus_Type) &
         " " & Event_Device.Hex_Image (ID.Vendor) &
         " " & Event_Device.Hex_Image (ID.Product) &
         " " & Event_Device.Hex_Image (ID.Version));

      Ada.Text_IO.Put_Line ("properties:");
      Ada.Text_IO.Put_Line ("  Pointer:        " & Props.Pointer'Image);
      Ada.Text_IO.Put_Line ("  Direct:         " & Props.Direct'Image);
      Ada.Text_IO.Put_Line ("  Button pad:     " & Props.Button_Pad'Image);
      Ada.Text_IO.Put_Line ("  Semi mt:        " & Props.Semi_Multi_Touch'Image);
      Ada.Text_IO.Put_Line ("  Top button pad: " & Props.Top_Button_Pad'Image);
      Ada.Text_IO.Put_Line ("  Pointing stick: " & Props.Pointing_Stick'Image);
      Ada.Text_IO.Put_Line ("  Accelerometer:  " & Props.Accelerometer'Image);

      Ada.Text_IO.Put_Line ("events:");
      Ada.Text_IO.Put_Line ("  Synchronization:  " & Events.Synchronization'Image);
      Ada.Text_IO.Put_Line ("  Keys:             " & Events.Keys'Image);
      Ada.Text_IO.Put_Line ("  Relative axes:    " & Events.Relative_Axes'Image);
      Ada.Text_IO.Put_Line ("  Absolute axes:    " & Events.Absolute_Axes'Image);
      Ada.Text_IO.Put_Line ("  Miscellaneous:    " & Events.Miscellaneous'Image);
      Ada.Text_IO.Put_Line ("  Switches:         " & Events.Switches'Image);
      Ada.Text_IO.Put_Line ("  LEDs:             " & Events.LEDs'Image);
      Ada.Text_IO.Put_Line ("  Sound:            " & Events.Sound'Image);
      Ada.Text_IO.Put_Line ("  Repeat:           " & Events.Repeat'Image);
      Ada.Text_IO.Put_Line ("  Force-feedback:   " & Events.Force_Feedback'Image);
      Ada.Text_IO.Put_Line ("  Power:            " & Events.Power'Image);
      Ada.Text_IO.Put_Line ("  Feedback Status:  " & Events.Feedback_Status'Image);
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
