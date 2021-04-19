with Interfaces.C;

with Ada.Command_Line;
with Ada.Numerics.Long_Long_Elementary_Functions;
with Ada.Text_IO;

with Event_Device.Force_Feedbacks;

procedure Info is
   package ACL renames Ada.Command_Line;

   EF : Event_Device.Input_Device;

   Do_Rumble : constant Boolean :=
     ACL.Argument_Count = 2 and then ACL.Argument (2) = "--ff=rumble";
   Do_Periodic : constant Boolean :=
     ACL.Argument_Count = 2 and then ACL.Argument (2) = "--ff=periodic";
   Do_Read   : constant Boolean :=
     ACL.Argument_Count = 2 and then ACL.Argument (2) = "--read";
begin
   if ACL.Argument_Count /= 1 and not (Do_Rumble or Do_Periodic or Do_Read) then
      Ada.Text_IO.Put_Line ("Usage: <path to /dev/input/event*> [--read|--ff=rumble|periodic]");
      ACL.Set_Exit_Status (ACL.Failure);
      return;
   end if;

   EF.Open (ACL.Argument (1));

   Ada.Text_IO.Put_Line ("Device name: '" & EF.Name & "'");
   Ada.Text_IO.Put_Line ("Location:    '" & EF.Location & "'");
   Ada.Text_IO.Put_Line ("Uniq ID:     '" & EF.Unique_ID & "'");
   declare
      ID     : constant Event_Device.Device_ID := EF.ID;
      Props  : constant Event_Device.Device_Properties := EF.Properties;
      Events : constant Event_Device.Device_Events := EF.Events;

      ID_A : constant String := Event_Device.Hex_Image (ID.Bus_Type);
      ID_B : constant String := Event_Device.Hex_Image (ID.Vendor);
      ID_C : constant String := Event_Device.Hex_Image (ID.Product);
      ID_D : constant String := Event_Device.Hex_Image (ID.Version);
   begin
      Ada.Text_IO.Put_Line ("Bus vendor product version: " &
        ID_A & " " & ID_B & " " & ID_C & " " & ID_D);
      Ada.Text_IO.Put_Line ("Properties:");
      Ada.Text_IO.Put_Line ("  Pointer:        " & Props.Pointer'Image);
      Ada.Text_IO.Put_Line ("  Direct:         " & Props.Direct'Image);
      Ada.Text_IO.Put_Line ("  Button pad:     " & Props.Button_Pad'Image);
      Ada.Text_IO.Put_Line ("  Semi mt:        " & Props.Semi_Multi_Touch'Image);
      Ada.Text_IO.Put_Line ("  Top button pad: " & Props.Top_Button_Pad'Image);
      Ada.Text_IO.Put_Line ("  Pointing stick: " & Props.Pointing_Stick'Image);
      Ada.Text_IO.Put_Line ("  Accelerometer:  " & Props.Accelerometer'Image);

      Ada.Text_IO.Put_Line ("Events:");
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
         declare
            Features : constant Event_Device.Key_Features := EF.Features;
         begin
            for K in Event_Device.Key_Kind'Range loop
               if Features (K) then
                  Ada.Text_IO.Put_Line ("    " & K'Image);
               end if;
            end loop;
         end;
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
                     Axis : constant Event_Device.Axis_Info := EF.Axis (Kind);
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

   if EF.Events.Keys then
      Ada.Text_IO.Put ("Pressed keys:");
      declare
         Key_Statuses : constant Event_Device.Key_Features := EF.Key_Statuses;
         Pressed_Keys : constant Boolean := (for some Key of Key_Statuses => Key);
      begin
         if Pressed_Keys then
            for K in Event_Device.Key_Kind'Range loop
               if Key_Statuses (K) then
                  Ada.Text_IO.Put (" " & K'Image);
               end if;
            end loop;
            Ada.Text_IO.New_Line;
         else
            Ada.Text_IO.Put_Line (" none");
         end if;
      end;
   end if;

   Ada.Text_IO.Put_Line ("Force feedback effects: " & EF.Force_Feedback_Effects'Image);

   if (Do_Rumble or Do_Periodic) and
     (EF.Events.Force_Feedback and EF.Force_Feedback_Effects > 0)
   then
      declare
         package FF renames Event_Device.Force_Feedbacks;

         use all type FF.Force_Feedback_Effect_Kind;
         use all type FF.Direction_Kind;
         use type Event_Device.Force_Feedback_Effect_ID;
         use type Event_Device.Unsigned_16;
         use type Interfaces.C.short;

         Effect_Rumble : constant FF.Force_Feedback_Effect :=
           (Kind      => Rumble,
            ID        => -1,
            Direction => Down,
            Trigger   => (Button   => 0,
                          Interval => FF.From_Duration (0.0)),
            Replay    => (Length      => FF.From_Duration (8.0),
                          Start_Delay => FF.From_Duration (0.0)),
            Effect =>
              (Kind => Rumble,
               Rumble_Effect => (Strong_Magnitude => 0,
                                 Weak_Magnitude   => Event_Device.Unsigned_16'Last)
            ));

         Effect_Periodic : constant FF.Force_Feedback_Effect :=
           (Kind      => Periodic,
            ID        => -1,
            Direction => Down,
            Trigger   => (Button   => 0,
                          Interval => FF.From_Duration (0.0)),
            Replay    => (Length      => FF.From_Duration (8.0),
                          Start_Delay => FF.From_Duration (0.0)),
            Effect =>
              (Kind => Periodic,
            Periodic_Effect => (Waveform  => FF.Sine,
                                Period    => FF.From_Duration (1.0),
                                Magnitude => Interfaces.C.short'Last,
                                Offset    => 0,
                                Phase     => 0,
                                Envelope  => (Attack_Length => FF.From_Duration (3.0),
                                              Attack_Level  => 0,
                                              Fade_Length   => FF.From_Duration (2.0),
                                              Fade_Level    => 0),
                                others => <>)
            ));

         Effect : aliased FF.Force_Feedback_Effect :=
           (if Do_Rumble then
              Effect_Rumble
            elsif Do_Periodic then
              Effect_Periodic
            else
              raise Program_Error);
      begin
         EF.Set_Force_Feedback_Gain (1.0);

         FF.Upload_Force_Feedback_Effect (EF, Effect);
         Ada.Text_IO.Put_Line ("Uploaded effect " & Effect.ID'Image);

         delay 0.5;
         EF.Play_Force_Feedback_Effect (Effect.ID, Count => 1);
         Ada.Text_IO.Put_Line ("Playing effect " & Effect.ID'Image &
           " for " & FF.Image (Effect.Replay.Length));

         delay (if Do_Rumble then 2.0 elsif Do_Periodic then 8.0 else 0.0);
         Ada.Text_IO.Put_Line ("Stopped playing effect");

         FF.Remove_Force_Feedback_Effect (EF, Effect.ID);
         Ada.Text_IO.Put_Line ("Removed effect " & Effect.ID'Image);
      end;
   end if;

   if Do_Read and (EF.Events.Absolute_Axes or EF.Events.Relative_Axes or EF.Events.Keys) then
      declare
         package LLEF renames Ada.Numerics.Long_Long_Elementary_Functions;
         package LLF_IO is new Ada.Text_IO.Float_IO (Long_Long_Float);

         Item : Event_Device.State;
         Features : constant Event_Device.Absolute_Axis_Features := EF.Features;

         use all type Event_Device.Absolute_Axis_Kind;
         use all type Event_Device.Key_State;
      begin
         loop
            EF.Read (Features, Item);

            declare
               V1 : constant Event_Device.Axis_Value :=
                 Item.Absolute (X) * Item.Absolute (X) +
                 Item.Absolute (Y) * Item.Absolute (Y) +
                 Item.Absolute (Z) * Item.Absolute (Z);

               L : constant Long_Long_Float :=
                 1.0 - LLEF.Sqrt (Long_Long_Float (V1));
               L_I : String := "-0.00000000";
            begin
               LLF_IO.Put (L_I, Item => L, Aft => 8, Exp => 0);

               Ada.Text_IO.Put_Line
                 (Item.Time'Image & " " &
                  Item.Absolute (X)'Image &
                  Item.Absolute (Y)'Image &
                  Item.Absolute (Z)'Image &
                  L_I &
                  "   " &
                  Item.Absolute (Rx)'Image &
                  Item.Absolute (Ry)'Image &
                  Item.Absolute (Rz)'Image &
                  "   " &
                  Item.Absolute (Hat_0X)'Image &
                  Item.Absolute (Hat_0Y)'Image);
            end;

            declare
               Pressed_Keys : constant Boolean := (for some Key of Item.Keys => Key = Pressed);
            begin
               if Pressed_Keys then
                  for K in Event_Device.Key_Kind'Range loop
                     if Item.Keys (K) = Pressed then
                        Ada.Text_IO.Put (" " & K'Image);
                     end if;
                  end loop;
                  Ada.Text_IO.New_Line;
               end if;
            end;
         end loop;
      end;
   end if;
end Info;
