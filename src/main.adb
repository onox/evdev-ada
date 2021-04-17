with Interfaces.C;

with Ada.Command_Line;
--  with Ada.Numerics.Long_Long_Elementary_Functions;
with Ada.Text_IO;

with Event_Device.Accelerometers;
with Event_Device.Force_Feedbacks;

procedure Main is
--   package LLEF renames Ada.Numerics.Long_Long_Elementary_Functions;
--   package LLF_IO is new Ada.Text_IO.Float_IO (Long_Long_Float);

   EF : Event_Device.Input_Device;
--   EF   : Event_Device.Accelerometers.Accelerometer;
--   Item : Event_Device.Accelerometers.Measurement;
begin
   EF.Open (Ada.Command_Line.Argument (1));

   Ada.Text_IO.Put_Line ("device name: '" & EF.Name & "'");
   Ada.Text_IO.Put_Line ("location:    '" & EF.Location & "'");
   Ada.Text_IO.Put_Line ("uniq ID:     '" & EF.Unique_ID & "'");
   declare
      ID     : constant Event_Device.Device_ID := EF.ID;
      Props  : constant Event_Device.Device_Properties := EF.Properties;
      Events : constant Event_Device.Device_Events := EF.Events;

      ID_A : constant String := Event_Device.Hex_Image (ID.Bus_Type);
      ID_B : constant String := Event_Device.Hex_Image (ID.Vendor);
      ID_C : constant String := Event_Device.Hex_Image (ID.Product);
      ID_D : constant String := Event_Device.Hex_Image (ID.Version);
   begin
      Ada.Text_IO.Put_Line ("bus ven pro ver: " & ID_A & " " & ID_B & " " & ID_C & " " & ID_D);
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
      if Events.Synchronization then
         declare
            Features : constant Event_Device.Synchronization_Features := EF.Features;
         begin
            for K in Event_Device.Synchronization_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Keys:             " & Events.Keys'Image);
      if Events.Keys then
         null;
      end if;
      Ada.Text_IO.Put_Line ("  Relative axes:    " & Events.Relative_Axes'Image);
      if Events.Relative_Axes then
         declare
            Features : constant Event_Device.Relative_Axis_Features := EF.Features;
         begin
            for K in Event_Device.Relative_Axis_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Absolute axes:    " & Events.Absolute_Axes'Image);
      if Events.Absolute_Axes then
         declare
            Features : constant Event_Device.Absolute_Axis_Features := EF.Features;
         begin
            for K in Event_Device.Absolute_Axis_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Miscellaneous:    " & Events.Miscellaneous'Image);
      if Events.Miscellaneous then
         declare
            Features : constant Event_Device.Miscellaneous_Features := EF.Features;
         begin
            for K in Event_Device.Miscellaneous_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Switches:         " & Events.Switches'Image);
      if Events.Switches then
         declare
            Features : constant Event_Device.Switch_Features := EF.Features;
         begin
            for K in Event_Device.Switch_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  LEDs:             " & Events.LEDs'Image);
      if Events.LEDs then
         declare
            Features : constant Event_Device.LED_Features := EF.Features;
         begin
            for K in Event_Device.LED_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Sound:            " & Events.Sound'Image);
      if Events.Sound then
         declare
            Features : constant Event_Device.Sound_Features := EF.Features;
         begin
            for K in Event_Device.Sound_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Repeat:           " & Events.Repeat'Image);
      if Events.Repeat then
         declare
            Features : constant Event_Device.Repeat_Features := EF.Features;
         begin
            for K in Event_Device.Repeat_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Force-feedback:   " & Events.Force_Feedback'Image);
      if Events.Force_Feedback then
         declare
            Features : constant Event_Device.Force_Feedback_Features := EF.Features;
         begin
            for K in Event_Device.Force_Feedback_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
      end if;
      Ada.Text_IO.Put_Line ("  Power:            " & Events.Power'Image);
      Ada.Text_IO.Put_Line ("  Feedback Status:  " & Events.Feedback_Status'Image);

      if Events.Absolute_Axes then
         Ada.Text_IO.Put_Line ("Axes:");
         declare
            Features : constant Event_Device.Absolute_Axis_Features := EF.Features;
         begin
            for Kind in Event_Device.Absolute_Axis_Kind'Range loop
               if Features (Kind) then
                  declare
                     Axis : constant Event_Device.Axis_Info :=
                       EF.Axis (Event_Device.Absolute_Axis_Info_Kind (Kind));
                  begin
                     Ada.Text_IO.Put_Line ("  " & Kind'Image & ":");
                     Ada.Text_IO.Put_Line ("    Value:      " & Axis.Value'Image);
                     Ada.Text_IO.Put_Line ("    Minimum:    " & Axis.Minimum'Image);
                     Ada.Text_IO.Put_Line ("    Maximum:    " & Axis.Maximum'Image);
                     Ada.Text_IO.Put_Line ("    Fuzz:       " & Axis.Fuzz'Image);
                     Ada.Text_IO.Put_Line ("    Flat:       " & Axis.Flat'Image);
                     Ada.Text_IO.Put_Line ("    Resolution: " & Axis.Resolution'Image);
                  end;
               end if;
            end loop;
         end;
      end if;
   end;

   Ada.Text_IO.Put_Line ("Force feedback effects: " & EF.Force_Feedback_Effects'Image);

   declare
      package FF renames Event_Device.Force_Feedbacks;

      use all type FF.Force_Feedback_Effect_Kind;
      use all type FF.Direction_Kind;
      use type Event_Device.Force_Feedback_Effect_ID;
      use type Event_Device.Unsigned_16;
      use type Interfaces.C.short;

      Effect : aliased FF.Force_Feedback_Effect :=
        (Kind      => Rumble,
         ID        => -1,
         Direction => Down,
         Trigger   => (Button   => 0,
                       Interval => FF.From_Duration (0.0)),
         Replay    => (Length      => FF.From_Duration (8.0),
                       Start_Delay => FF.From_Duration (0.0)),
         Effect =>
           (Kind => Rumble,
--            Periodic_Effect => (Waveform  => FF.Sine,
--                                Period    => FF.From_Duration (1.0),
--                                Magnitude => Interfaces.C.short'Last,
--                                Offset    => 0,
--                                Phase     => 0,
--                                Envelope  => (Attack_Length => FF.From_Duration (4.0),
--                                              Attack_Level  => 0,
--                                              Fade_Length   => FF.From_Duration (2.0),
--                                              Fade_Level    => 0),
--                                others => <>)
            Rumble_Effect => (Strong_Magnitude => 0,
                              Weak_Magnitude   => Event_Device.Unsigned_16'Last)
         ));
   begin
      EF.Set_Force_Feedback_Gain (1.0);

      FF.Upload_Force_Feedback_Effect (EF, Effect);
      Ada.Text_IO.Put_Line ("uploaded effect " & Effect.ID'Image);

      delay 1.0;
      EF.Play_Force_Feedback_Effect (Effect.ID, Count => 1);
      Ada.Text_IO.Put_Line ("playing effect " & Effect.ID'Image &
        " for " & FF.Image (Effect.Replay.Length));

      delay 9.0;
      Ada.Text_IO.Put_Line ("stopped playing effect");

      FF.Remove_Force_Feedback_Effect (EF, Effect.ID);
      Ada.Text_IO.Put_Line ("removed effect " & Effect.ID'Image);
   end;

--   loop
--      EF.Read (Item);
--      declare
--         use Event_Device.Accelerometers;
--
--         V1 : constant Accel_Unit := Item.X * Item.X + Item.Y * Item.Y + Item.Z * Item.Z;
--         L : constant Long_Long_Float :=
--           1.0 - LLEF.Sqrt (Long_Long_Float (V1));
--
--         L_I : String := "-0.00000000";
--      begin
--         LLF_IO.Put (L_I, Item => L, Aft => 8, Exp => 0);
--         Ada.Text_IO.Put_Line
--           (Item.Time'Image & " A " & Item.X'Image & Item.Y'Image & Item.Z'Image & L_I &
--            "   " & Item.Rx'Image & Item.Ry'Image & Item.Rz'Image);
--      end;
--   end loop;
end Main;
