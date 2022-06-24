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

with System;

with Ada.Unchecked_Conversion;

with Event_Device.Input_Dev;

package body Event_Device is

   subtype Unused_Type is Boolean range False .. False;

   type Internal_Relative_Axis_Features is record
      X                         : Boolean := False;
      Y                         : Boolean := False;
      Z                         : Boolean := False;
      Rx                        : Boolean := False;
      Ry                        : Boolean := False;
      Rz                        : Boolean := False;
      Horizontal_Wheel          : Boolean := False;
      Diagonal                  : Boolean := False;
      Wheel                     : Boolean := False;
      Misc                      : Boolean := False;
      Wheel_High_Res            : Boolean := False;
      Horizontal_Wheel_High_Res : Boolean := False;
   end record;

   for Internal_Relative_Axis_Features use record
      X                         at 0 range 0 .. 0;
      Y                         at 0 range 1 .. 1;
      Z                         at 0 range 2 .. 2;
      Rx                        at 0 range 3 .. 3;
      Ry                        at 0 range 4 .. 4;
      Rz                        at 0 range 5 .. 5;
      Horizontal_Wheel          at 0 range 6 .. 6;
      Diagonal                  at 0 range 7 .. 7;
      Wheel                     at 0 range 8 .. 8;
      Misc                      at 0 range 9 .. 9;
      Wheel_High_Res            at 0 range 11 .. 11;
      Horizontal_Wheel_High_Res at 0 range 12 .. 12;
   end record;
   for Internal_Relative_Axis_Features'Size use Relative_Axis_Info_Kind'Size;

   type Internal_Absolute_Axis_Features is record
      X              : Boolean := False;
      Y              : Boolean := False;
      Z              : Boolean := False;
      Rx             : Boolean := False;
      Ry             : Boolean := False;
      Rz             : Boolean := False;
      Throttle       : Boolean := False;
      Rudder         : Boolean := False;
      Wheel          : Boolean := False;
      Gas            : Boolean := False;
      Brake          : Boolean := False;
      Hat_0X         : Boolean := False;
      Hat_0Y         : Boolean := False;
      Hat_1X         : Boolean := False;
      Hat_1Y         : Boolean := False;
      Hat_2X         : Boolean := False;
      Hat_2Y         : Boolean := False;
      Hat_3X         : Boolean := False;
      Hat_3Y         : Boolean := False;
      Pressure       : Boolean := False;
      Distance       : Boolean := False;
      Tilt_X         : Boolean := False;
      Tilt_Y         : Boolean := False;
      Tool_Width     : Boolean := False;
      Volume         : Boolean := False;
      Misc           : Boolean := False;
      MT_Slot        : Boolean := False;
      MT_Touch_Major : Boolean := False;
      MT_Touch_Minor : Boolean := False;
      MT_Width_Major : Boolean := False;
      MT_Width_Minor : Boolean := False;
      MT_Orientation : Boolean := False;
      MT_Position_X  : Boolean := False;
      MT_Position_Y  : Boolean := False;
      MT_Tool_Type   : Boolean := False;
      MT_Blob_ID     : Boolean := False;
      MT_Tracking_ID : Boolean := False;
      MT_Pressure    : Boolean := False;
      MT_Distance    : Boolean := False;
      MT_Tool_X      : Boolean := False;
      MT_Tool_Y      : Boolean := False;
   end record;

   for Internal_Absolute_Axis_Features use record
      X              at 0 range 0 .. 0;
      Y              at 0 range 1 .. 1;
      Z              at 0 range 2 .. 2;
      Rx             at 0 range 3 .. 3;
      Ry             at 0 range 4 .. 4;
      Rz             at 0 range 5 .. 5;
      Throttle       at 0 range 6 .. 6;
      Rudder         at 0 range 7 .. 7;
      Wheel          at 0 range 8 .. 8;
      Gas            at 0 range 9 .. 9;
      Brake          at 0 range 10 .. 10;
      Hat_0X         at 0 range 16 .. 16;
      Hat_0Y         at 0 range 17 .. 17;
      Hat_1X         at 0 range 18 .. 18;
      Hat_1Y         at 0 range 19 .. 19;
      Hat_2X         at 0 range 20 .. 20;
      Hat_2Y         at 0 range 21 .. 21;
      Hat_3X         at 0 range 22 .. 22;
      Hat_3Y         at 0 range 23 .. 23;
      Pressure       at 0 range 24 .. 24;
      Distance       at 0 range 25 .. 25;
      Tilt_X         at 0 range 26 .. 26;
      Tilt_Y         at 0 range 27 .. 27;
      Tool_Width     at 0 range 28 .. 28;
      Volume         at 0 range 32 .. 32;
      Misc           at 0 range 40 .. 40;
      MT_Slot        at 0 range 47 .. 47;
      MT_Touch_Major at 0 range 48 .. 48;
      MT_Touch_Minor at 0 range 49 .. 49;
      MT_Width_Major at 0 range 50 .. 50;
      MT_Width_Minor at 0 range 51 .. 51;
      MT_Orientation at 0 range 52 .. 52;
      MT_Position_X  at 0 range 53 .. 53;
      MT_Position_Y  at 0 range 54 .. 54;
      MT_Tool_Type   at 0 range 55 .. 55;
      MT_Blob_ID     at 0 range 56 .. 56;
      MT_Tracking_ID at 0 range 57 .. 57;
      MT_Pressure    at 0 range 58 .. 58;
      MT_Distance    at 0 range 59 .. 59;
      MT_Tool_X      at 0 range 60 .. 60;
      MT_Tool_Y      at 0 range 61 .. 61;
   end record;
   for Internal_Absolute_Axis_Features'Size use Absolute_Axis_Info_Kind'Size;

   type Internal_Force_Feedback_Features is record
      Rumble      : Boolean := False;
      Periodic    : Boolean := False;
      Constant_V  : Boolean := False;
      Spring      : Boolean := False;
      Friction    : Boolean := False;
      Damper      : Boolean := False;
      Inertia     : Boolean := False;
      Ramp        : Boolean := False;

      Square      : Boolean := False;
      Triangle    : Boolean := False;
      Sine        : Boolean := False;
      Saw_Up      : Boolean := False;
      Saw_Down    : Boolean := False;
      Custom      : Boolean := False;

      Gain        : Boolean := False;
      Auto_Center : Boolean := False;

      Unused : Unused_Type := False;
   end record;

   for Internal_Force_Feedback_Features use record
      Rumble      at 0 range 80 .. 80;
      Periodic    at 0 range 81 .. 81;
      Constant_V  at 0 range 82 .. 82;
      Spring      at 0 range 83 .. 83;
      Friction    at 0 range 84 .. 84;
      Damper      at 0 range 85 .. 85;
      Inertia     at 0 range 86 .. 86;
      Ramp        at 0 range 87 .. 87;

      Square      at 0 range 88 .. 88;
      Triangle    at 0 range 89 .. 89;
      Sine        at 0 range 90 .. 90;
      Saw_Up      at 0 range 91 .. 91;
      Saw_Down    at 0 range 92 .. 92;
      Custom      at 0 range 93 .. 93;

      Gain        at 0 range 96 .. 96;
      Auto_Center at 0 range 97 .. 97;

      Unused      at 0 range 98 .. 127;
   end record;
   for Internal_Force_Feedback_Features'Size use 128;

   function To_Code (Kind : Key_Kind) return Key_Code is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Key_Kind, Target => Key_Code);
   begin
      return Convert (Kind);
   end To_Code;

   function To_Key (Code : Key_Code) return Key_Kind is
   begin
      for Kind in Key_Kind'Range loop
         if Code = To_Code (Kind) then
            return Kind;
         end if;
      end loop;

      return Key_Unknown;
   end To_Key;

   function Hex_Image (Value : Unsigned_8) return String is
      Hex : constant array (Unsigned_8 range 0 .. 15) of Character := "0123456789abcdef";
   begin
      return Hex (Value / 16) & Hex (Value mod 16);
   end Hex_Image;

   function Hex_Image (Value : Unsigned_16; Bit_Order : System.Bit_Order) return String is
      Low  : constant Unsigned_8 := Unsigned_8 (16#FF# and Value);
      High : constant Unsigned_8 := Unsigned_8 (16#FF# and (Value / 256));

      use type System.Bit_Order;
   begin
      if System.Default_Bit_Order = Bit_Order then
         return Hex_Image (Low) & Hex_Image (High);
      else
         return Hex_Image (High) & Hex_Image (Low);
      end if;
   end Hex_Image;

   function Hex_Image (Value : Unsigned_16) return String is
     (Hex_Image (Value, System.High_Order_First));

   function GUID (ID : Device_ID) return String is
     (Hex_Image (ID.Bus, System.Low_Order_First) & "0000" &
      Hex_Image (ID.Vendor, System.Low_Order_First) & "0000" &
      Hex_Image (ID.Product, System.Low_Order_First) & "0000" &
      Hex_Image (ID.Version, System.Low_Order_First) & "0000");

   ----------------------------------------------------------------------------

   use all type Event_Device.Input_Dev.Access_Mode;
   use type Event_Device.Input_Dev.Unsigned_14;

   function ID (Object : Input_Device) return Device_ID is
      Result : aliased Device_ID;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#02#, Result'Size / System.Storage_Unit), Result'Address);
   begin
      return (if Error_Code /= -1 then Result else (others => 0));
   end ID;

   function Location (Object : Input_Device) return String is
      Result : aliased String (1 .. 128) := (others => ' ');

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#07#, Result'Length), Result'Address);
   begin
      return (if Length > 0 then Result (1 .. Length - 1) else "");
   end Location;

   function Unique_ID (Object : Input_Device) return String is
      Result : aliased String (1 .. 128) := (others => ' ');

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#08#, Result'Length), Result'Address);
   begin
      return (if Length > 0 then Result (1 .. Length - 1) else "");
   end Unique_ID;

   ----------------------------------------------------------------------------

   function Properties (Object : Input_Device) return Device_Properties is
      Result : aliased Device_Properties;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#09#, Result'Size / System.Storage_Unit), Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Properties;

   function Events (Object : Input_Device) return Device_Events is
      Result : aliased Device_Events;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20#, Result'Size / System.Storage_Unit), Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Events;

   function Convert is new Ada.Unchecked_Conversion
     (Source => Event_Kind, Target => Interfaces.C.unsigned_short);

   function Features (Object : Input_Device) return Synchronization_Features is
      Result : aliased Synchronization_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Synchronization)),
         Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Key_Features is
      Result : aliased Key_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Key)),
         Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);

      return Result;
   end Features;

   function Features (Object : Input_Device) return Relative_Axis_Features is
      Result : aliased Internal_Relative_Axis_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (Relative)), Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);

      return
        (X                          => Result.X,
         Y                          => Result.Y,
         Z                          => Result.Z,
         Rx                         => Result.Rx,
         Ry                         => Result.Ry,
         Rz                         => Result.Rz,
         Horizontal_Wheel           => Result.Horizontal_Wheel,
         Diagonal                   => Result.Diagonal,
         Wheel                      => Result.Wheel,
         Misc                       => Result.Misc,
         Wheel_High_Res             => Result.Wheel_High_Res,
         Horizontal_Wheel_High_Res  => Result.Horizontal_Wheel_High_Res);
   end Features;

   function Features (Object : Input_Device) return Absolute_Axis_Features is
      Result : aliased Internal_Absolute_Axis_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (Absolute)), Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);

      return
        (X              => Result.X,
         Y              => Result.Y,
         Z              => Result.Z,
         Rx             => Result.Rx,
         Ry             => Result.Ry,
         Rz             => Result.Rz,
         Throttle       => Result.Throttle,
         Rudder         => Result.Rudder,
         Wheel          => Result.Wheel,
         Gas            => Result.Gas,
         Brake          => Result.Brake,
         Hat_0X         => Result.Hat_0X,
         Hat_0Y         => Result.Hat_0Y,
         Hat_1X         => Result.Hat_1X,
         Hat_1Y         => Result.Hat_1Y,
         Hat_2X         => Result.Hat_2X,
         Hat_2Y         => Result.Hat_2Y,
         Hat_3X         => Result.Hat_3X,
         Hat_3Y         => Result.Hat_3Y,
         Pressure       => Result.Pressure,
         Distance       => Result.Distance,
         Tilt_X         => Result.Tilt_X,
         Tilt_Y         => Result.Tilt_Y,
         Tool_Width     => Result.Tool_Width,
         Volume         => Result.Volume,
         Misc           => Result.Misc,
         MT_Slot        => Result.MT_Slot,
         MT_Touch_Major => Result.MT_Touch_Major,
         MT_Touch_Minor => Result.MT_Touch_Minor,
         MT_Width_Major => Result.MT_Width_Major,
         MT_Width_Minor => Result.MT_Width_Minor,
         MT_Orientation => Result.MT_Orientation,
         MT_Position_X  => Result.MT_Position_X,
         MT_Position_Y  => Result.MT_Position_Y,
         MT_Tool_Type   => Result.MT_Tool_Type,
         MT_Blob_ID     => Result.MT_Blob_ID,
         MT_Tracking_ID => Result.MT_Tracking_ID,
         MT_Pressure    => Result.MT_Pressure,
         MT_Distance    => Result.MT_Distance,
         MT_Tool_X      => Result.MT_Tool_X,
         MT_Tool_Y      => Result.MT_Tool_Y);
   end Features;

   function Features (Object : Input_Device) return Switch_Features is
      Result : aliased Switch_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (Switch)), Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Miscellaneous_Features is
      Result : aliased Miscellaneous_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (Miscellaneous)),
          Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return LED_Features is
      Result : aliased LED_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (LED)), Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Repeat_Features is
      Result : aliased Repeat_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (Repeat)), Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Sound_Features is
      Result : aliased Sound_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (Sound)), Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Force_Feedback_Features is
      Result : aliased Internal_Force_Feedback_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#20# + Unsigned_8 (Convert (Force_Feedback)),
          Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);

      return
        (Rumble      => Result.Rumble,
         Periodic    => Result.Periodic,
         Constant_V  => Result.Constant_V,
         Spring      => Result.Spring,
         Friction    => Result.Friction,
         Damper      => Result.Damper,
         Inertia     => Result.Inertia,
         Ramp        => Result.Ramp,
         Square      => Result.Square,
         Triangle    => Result.Triangle,
         Sine        => Result.Sine,
         Saw_Up      => Result.Saw_Up,
         Saw_Down    => Result.Saw_Down,
         Custom      => Result.Custom,
         Gain        => Result.Gain,
         Auto_Center => Result.Auto_Center);
   end Features;

   ----------------------------------------------------------------------------

   function Axis (Object : Input_Device; Axis : Absolute_Axis_Kind) return Axis_Info is
      Result : aliased Axis_Info;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Absolute_Axis_Info_Kind, Target => Unsigned_64);

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#40# + Unsigned_8 (Convert (Absolute_Axis_Info_Kind (Axis))),
          Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Axis;

   function Key_Statuses (Object : Input_Device) return Key_Features is
      Result : aliased Key_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#18#,
         Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);

      return Result;
   end Key_Statuses;

   function LED_Statuses (Object : Input_Device) return LED_Features is
      Result : aliased LED_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#19#, Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end LED_Statuses;

   function Sound_Statuses (Object : Input_Device) return Sound_Features is
      Result : aliased Sound_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#1A#, Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Sound_Statuses;

   function Switch_Statuses (Object : Input_Device) return Switch_Features is
      Result : aliased Switch_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD,
         (Read, 'E', 16#1B#, Result'Size / System.Storage_Unit),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Switch_Statuses;

   function Force_Feedback_Effects (Object : Input_Device) return Natural is
      Result : aliased Integer;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#84#, Result'Size / System.Storage_Unit), Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Force_Feedback_Effects;

   procedure Set_Force_Feedback_Gain
     (Object : Input_Device;
      Value  : Force_Feedback_Gain)
   is
      FF_Gain_Code : constant := 16#60#;

      Event : constant Input_Dev.Input_Event :=
        (Time  => (0, 0),
         Event => Force_Feedback,
         Code  => FF_Gain_Code,
         Value => Interfaces.C.int (16#FF_FF.00# * Value));

      Result_Unused : constant Input_Dev.Result := Input_Dev.Write (Object.FD, Event);
   begin
      --  Ignore any possible errors. If the device has been disconnected
      --  then playing a force-feedback effect will fail, which can be
      --  detected by the boolean returned by Play_Force_Feedback_Effect.
      null;
   end Set_Force_Feedback_Gain;

   procedure Set_Force_Feedback_Auto_Center
     (Object : Input_Device;
      Value  : Force_Feedback_Auto_Center)
   is
      FF_Auto_Center_Code : constant := 16#61#;

      Event : constant Input_Dev.Input_Event :=
        (Time  => (0, 0),
         Event => Force_Feedback,
         Code  => FF_Auto_Center_Code,
         Value => Interfaces.C.int (16#FF_FF.00# * Value));

      Result_Unused : constant Input_Dev.Result := Input_Dev.Write (Object.FD, Event);
   begin
      --  Ignore any possible errors. If the device has been disconnected
      --  then playing a force-feedback effect will fail, which can be
      --  detected by the boolean returned by Play_Force_Feedback_Effect.
      null;
   end Set_Force_Feedback_Auto_Center;

   function Play_Force_Feedback_Effect
     (Object     : Input_Device;
      Identifier : Uploaded_Force_Feedback_Effect_ID;
      Count      : Natural := 1) return Boolean
   is
      Event : constant Input_Dev.Input_Event :=
        (Time  => (0, 0),
         Event => Force_Feedback,
         Code  => Interfaces.C.unsigned_short (Identifier),
         Value => Interfaces.C.int (Count));

      Result : constant Input_Dev.Result := Input_Dev.Write (Object.FD, Event);
   begin
      return Result.Is_Success;
   end Play_Force_Feedback_Effect;

   function Name (Object : Input_Device) return String is
      Result : aliased String (1 .. 128) := (others => ' ');

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#06#, Result'Length), Result'Address);
   begin
      return (if Length > 0 then Result (1 .. Length - 1) else "");
   end Name;

   function Is_Open (Object : Input_Device) return Boolean is
     (Object.Open);

   function Open
     (Object    : in out Input_Device;
      File_Name : String;
      Blocking  : Boolean := True) return Boolean
   is
      Result : constant Input_Dev.Result := Input_Dev.Open (File_Name, Blocking => Blocking);
   begin
      if Result.Is_Success then
         Object.FD := Result.FD;
      end if;
      Object.Open := Result.Is_Success;
      return Object.Open;
   end Open;

   procedure Close (Object : in out Input_Device) is
      Result : constant Input_Dev.Result := Input_Dev.Close (Object.FD);
   begin
      Object.FD   := -1;
      Object.Open := not Result.Is_Success;
   end Close;

   overriding procedure Finalize (Object : in out Input_Device) is
   begin
      if Object.Is_Open then
         Object.Close;
      end if;
   end Finalize;

   function Read
     (Object : Input_Device;
      Value  : out State) return Read_Result
   is
      use Event_Device.Input_Dev;
      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.int;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Synchronization_Kind);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Unsigned_16);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Relative_Axis_Info_Kind);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Unsigned_64, Target => Absolute_Axis_Info_Kind);
      --  Convert to Absolute_Axis_Info_Kind first before converting to
      --  Absolute_Axis_Kind, because the former has a representation clause

      Event  : Input_Event;
      Result : Input_Dev.Result;

      Has_Dropped : Boolean := False;
   begin
      loop
         Result := Input_Dev.Read (Object.FD, Event);

         if not Result.Is_Success then
            if Result.Error = Would_Block then
               return Would_Block;
            else
               Value := (others => <>);
               return Error;
            end if;
         end if;

         case Event.Event is
            when Key =>
               declare
                  Code : constant Key_Code := Key_Code (Unsigned_16'(Convert (Event.Code)));
               begin
                  Value.Keys (Key_Code_Index (Code)) :=
                    (if Event.Value /= 0 then Pressed else Released);
               end;
            when Relative =>
               if not Has_Dropped then
                  declare
                     Code : constant Relative_Axis_Kind :=
                       Relative_Axis_Kind (Relative_Axis_Info_Kind'(Convert (Event.Code)));
                  begin
                     Value.Relative (Code) := Integer (Event.Value);
                  end;
               end if;
            when Absolute =>
               if not Has_Dropped then
                  declare
                     Code : constant Absolute_Axis_Kind :=
                       Absolute_Axis_Kind (Convert (Unsigned_64 (Event.Code)));
                  begin
                     Value.Absolute (Code) := Integer (Event.Value);
                  end;
               end if;
            when Synchronization =>
               declare
                  Code : constant Synchronization_Kind := Convert (Event.Code);
               begin
                  case Code is
                     when Report =>
                        Value.Time := Duration (Event.Time.Seconds)
                          + Duration (Event.Time.Microseconds) / 1.0e6;
                        exit;
                     when Dropped =>
                        Has_Dropped := True;
                     when others =>
                        null;
                  end case;
               end;
            when others =>
               null;
         end case;
      end loop;

      return OK;
   end Read;

end Event_Device;
