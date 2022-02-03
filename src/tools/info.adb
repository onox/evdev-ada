--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with Interfaces.C;

with Ada.Command_Line;
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

   if not EF.Open (ACL.Argument (1)) then
      Ada.Text_IO.Put_Line ("Cannot open device " & ACL.Argument (1));
      ACL.Set_Exit_Status (ACL.Failure);
      return;
   end if;

   Ada.Text_IO.Put_Line ("Device name: '" & EF.Name & "'");
   Ada.Text_IO.Put_Line ("Location:    '" & EF.Location & "'");
   Ada.Text_IO.Put_Line ("Uniq ID:     '" & EF.Unique_ID & "'");
   declare
      ID     : constant Event_Device.Device_ID := EF.ID;
      Props  : constant Event_Device.Device_Properties := EF.Properties;
      Events : constant Event_Device.Device_Events := EF.Events;

      ID_A : constant String := Event_Device.Hex_Image (ID.Bus);
      ID_B : constant String := Event_Device.Hex_Image (ID.Vendor);
      ID_C : constant String := Event_Device.Hex_Image (ID.Product);
      ID_D : constant String := Event_Device.Hex_Image (ID.Version);
   begin
      Ada.Text_IO.Put_Line ("Bus ven pro ver: " &
        ID_A & " " & ID_B & " " & ID_C & " " & ID_D);
      Ada.Text_IO.Put_Line ("GUID:            " & Event_Device.GUID (ID));
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
            for Code in Event_Device.Key_Code loop
               if Features (Event_Device.Key_Code_Index (Code)) then
                  declare
                     Kind : constant Event_Device.Key_Kind := Event_Device.To_Key (Code);

                     use all type Event_Device.Key_Kind;
                  begin
                     if Kind /= Key_Unknown then
                        Ada.Text_IO.Put_Line ("    " & Kind'Image);
                     else
                        Ada.Text_IO.Put_Line ("    Unknown key code " & Code'Image);
                     end if;
                  end;
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
               if Key_Statuses (Event_Device.Key_Code_Index (Event_Device.To_Code (K))) then
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
         if EF.Play_Force_Feedback_Effect (Effect.ID, Count => 1) then
            Ada.Text_IO.Put_Line ("Playing effect " & Effect.ID'Image &
              " for " & FF.Image (Effect.Replay.Length));
         else
            Ada.Text_IO.Put_Line ("Failed playing effect " & Effect.ID'Image);
         end if;

         delay (if Do_Rumble then 2.0 elsif Do_Periodic then 8.0 else 0.0);
         Ada.Text_IO.Put_Line ("Stopped playing effect");

         FF.Remove_Force_Feedback_Effect (EF, Effect.ID);
         Ada.Text_IO.Put_Line ("Removed effect " & Effect.ID'Image);
      end;
   end if;

   if Do_Read and (EF.Events.Absolute_Axes or EF.Events.Relative_Axes or EF.Events.Keys) then
      declare
         Item : Event_Device.State;
         Features : constant Event_Device.Absolute_Axis_Features := EF.Features;
         Accelerometer : constant Boolean := EF.Properties.Accelerometer;

         use all type Event_Device.Absolute_Axis_Kind;
         use all type Event_Device.Key_State;
         use all type Event_Device.Read_Result;

         type Axis_Value is delta 2.0 ** (-16)
           range -(2.0 ** 47) ..
                 +(2.0 ** 47 - 2.0 ** (-16));
      begin
         --  Set initial state of Item
         if EF.Events.Absolute_Axes then
            for Axis in Features'Range loop
               if Features (Axis) then
                  Item.Absolute (Axis) := EF.Axis (Axis).Value;
               end if;
            end loop;
         end if;

         loop
            exit when EF.Read (Item) /= OK;

            if EF.Events.Absolute_Axes then
               Ada.Text_IO.Put (Item.Time'Image);
               for Axis in Features'Range loop
                  if Features (Axis) then
                     declare
                        Info  : constant Event_Device.Axis_Info := EF.Axis (Axis);
                        Info_Range : constant Axis_Value :=
                          Axis_Value (Info.Maximum - Info.Minimum);
                        Value : Axis_Value := Axis_Value (Item.Absolute (Axis));
                     begin
                        Value := Value /
                          Axis_Value (if Info.Resolution > 0 then Info.Resolution else 1);
                        if not Accelerometer then
                           Value := (Value - Axis_Value (Info.Minimum)) / Info_Range;
                           Value := Value * 2.0 - 1.0;
                        end if;
                        Ada.Text_IO.Put (" " & Axis'Image & ": " & Value'Image);
                     end;
                  end if;
               end loop;
               Ada.Text_IO.New_Line;
            end if;

            declare
               Pressed_Keys : constant Boolean := (for some Key of Item.Keys => Key = Pressed);
            begin
               if Pressed_Keys then
                  for K in Event_Device.Key_Kind'Range loop
                     if
                       Item.Keys (Event_Device.Key_Code_Index (Event_Device.To_Code (K))) = Pressed
                     then
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
