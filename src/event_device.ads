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

private with Ada.Finalization;

package Event_Device is
   pragma Preelaborate;

   type Unsigned_8 is mod 2 ** 8
     with Size => 8;

   type Unsigned_16 is mod 2 ** 16
     with Size => 16;

   function Hex_Image (Value : Unsigned_8) return String;

   function Hex_Image (Value : Unsigned_16) return String;

   type Device_ID is record
      Bus, Vendor, Product, Version : Unsigned_16;
   end record
     with Convention => C_Pass_By_Copy;

   function GUID (ID : Device_ID) return String;

   type Axis_Info is record
      Value, Minimum, Maximum, Fuzz, Flat, Resolution : Integer;
   end record
     with Convention => C_Pass_By_Copy;

   type Event_Kind is
     (Synchronization,
      Key,
      Relative,
      Absolute,
      Miscellaneous,
      Switch,
      LED,
      Sound,
      Repeat,
      Force_Feedback,
      Power,
      Feedback_Status);

   type Synchronization_Kind is (Report, Config, MT_Report, Dropped);

   type Key_Kind is
     (Key_Unknown,
      Button_South,
      Button_East,
      Button_North,
      Button_West,
      Button_Trigger_Left_1,
      Button_Trigger_Right_1,
      Button_Trigger_Left_2,
      Button_Trigger_Right_2,
      Button_Select,
      Button_Start,
      Button_Mode,
      Button_Thumb_Left,
      Button_Thumb_Right);

   type Key_Code is new Unsigned_16 range 0 .. 767;
   --  Maximum equal to KEY_MAX in /usr/include/linux/input-event-codes.h

   function To_Code (Kind : Key_Kind) return Key_Code;
   --  Return the key code of the given enum value

   function To_Key (Code : Key_Code) return Key_Kind;
   --  Return the enum value that belongs to the given key code or
   --  return Key_Unknown if the corresponding enum value is missing
   --  in the type definition

   type Relative_Axis_Info_Kind is
     (X,
      Y,
      Z,
      Rx,
      Ry,
      Rz,
      Horizontal_Wheel,
      Diagonal,
      Wheel,
      Misc,
      Wheel_High_Res,
      Horizontal_Wheel_High_Res);

   type Relative_Axis_Kind is new Relative_Axis_Info_Kind;

   type Absolute_Axis_Info_Kind is
     (X,
      Y,
      Z,
      Rx,
      Ry,
      Rz,
      Throttle,
      Rudder,
      Wheel,
      Gas,
      Brake,
      Hat_0X,
      Hat_0Y,
      Hat_1X,
      Hat_1Y,
      Hat_2X,
      Hat_2Y,
      Hat_3X,
      Hat_3Y,
      Pressure,
      Distance,
      Tilt_X,
      Tilt_Y,
      Tool_Width,
      Volume,
      Misc,
      MT_Slot,
      MT_Touch_Major,
      MT_Touch_Minor,
      MT_Width_Major,
      MT_Width_Minor,
      MT_Orientation,
      MT_Position_X,
      MT_Position_Y,
      MT_Tool_Type,
      MT_Blob_ID,
      MT_Tracking_ID,
      MT_Pressure,
      MT_Distance,
      MT_Tool_X,
      MT_Tool_Y);

   type Absolute_Axis_Kind is new Absolute_Axis_Info_Kind;

   type Switch_Kind is
     (Lid,
      Tablet_Mode,
      Headphone_Insert,
      Rfkill_all,
      Microphone_Insert,
      Dock,
      Lineout_Insert,
      Jack_Physical_Insert,
      Video_Out_Insert,
      Camera_Lens_Cover,
      Keypad_Slide,
      Front_Proximity,
      Rotate_Lock,
      Line_In_Insert,
      Mute_Device,
      Pen_Inserted,
      Machine_Cover);

   type Miscellaneous_Kind is (Serial, Pulse_LED, Gesture, Raw, Scan, Timestamp);

   type LED_Kind is
     (Num_Lock, Caps_Lock, Scroll_Lock, Compose, Kana, Sleep, Suspend, Mute, Misc, Mail, Charging);

   type Repeat_Kind is (Repeat_Delay, Repeat_Period);

   type Sound_Kind is (Click, Bell, Tone);

   type Force_Feedback_Kind is
     (Rumble,
      Periodic,
      Constant_V,
      Spring,
      Friction,
      Damper,
      Inertia,
      Ramp,
      Square,
      Triangle,
      Sine,
      Saw_Up,
      Saw_Down,
      Custom,
      Gain,
      Auto_Center);

   type Synchronization_Features is array (Synchronization_Kind) of Boolean
     with Component_Size => 1;

   type Key_Code_Index is range 0 .. Key_Code'Last;

   type Key_Features is array (Key_Code_Index) of Boolean
     with Component_Size => 1;

   type Relative_Axis_Features is array (Relative_Axis_Kind) of Boolean
     with Component_Size => 1;

   type Absolute_Axis_Features is array (Absolute_Axis_Kind) of Boolean
     with Component_Size => 1;

   type Switch_Features is array (Switch_Kind) of Boolean
     with Component_Size => 1;

   type Miscellaneous_Features is array (Miscellaneous_Kind) of Boolean
     with Component_Size => 1;

   type LED_Features is array (LED_Kind) of Boolean
     with Component_Size => 1;

   type Repeat_Features is array (Repeat_Kind) of Boolean
     with Component_Size => 1;

   type Sound_Features is array (Sound_Kind) of Boolean
     with Component_Size => 1;

   type Force_Feedback_Features is array (Force_Feedback_Kind) of Boolean
     with Component_Size => 1;

   type Device_Properties is record
      Pointer          : Boolean := False;
      Direct           : Boolean := False;
      Button_Pad       : Boolean := False;
      Semi_Multi_Touch : Boolean := False;
      Top_Button_Pad   : Boolean := False;
      Pointing_Stick   : Boolean := False;
      Accelerometer    : Boolean := False;
   end record;

   type Device_Events is record
      Synchronization : Boolean := False;
      Keys            : Boolean := False;
      Relative_Axes   : Boolean := False;
      Absolute_Axes   : Boolean := False;
      Miscellaneous   : Boolean := False;
      Switches        : Boolean := False;
      LEDs            : Boolean := False;
      Sound           : Boolean := False;
      Repeat          : Boolean := False;
      Force_Feedback  : Boolean := False;
      Power           : Boolean := False;
      Feedback_Status : Boolean := False;
   end record;

   ----------------------------------------------------------------------------

   type Input_Device is tagged limited private;

   function Name (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function ID (Object : Input_Device) return Device_ID
     with Pre => Object.Is_Open;

   function Location (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function Unique_ID (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   ----------------------------------------------------------------------------

   type Key_State is (Released, Pressed);

   type Key_Values is array (Key_Code_Index) of Key_State;

   type Relative_Axis_Values is array (Relative_Axis_Kind) of Integer;
   type Absolute_Axis_Values is array (Absolute_Axis_Kind) of Integer;

   type State is record
      Keys     : Key_Values           := (others => Released);
      Relative : Relative_Axis_Values := (others => 0);
      Absolute : Absolute_Axis_Values := (others => 0);
      Time     : Duration             := 0.0;
   end record;

   type Read_Result is (Error, Would_Block, OK);

   function Read
     (Object : Input_Device;
      Value  : out State) return Read_Result
   with Pre => Object.Is_Open;
   --  Read keys, relative, and absolute axes of device and return whether
   --  reading was successful
   --
   --  Resolution of accelerometer and gyro axes:
   --
   --            Accelerometer  No accelerometer
   --            -------------  --------
   --  X/Y/Z:    units/g        units/mm
   --  Rx/Ry/Rz: units/deg/s    units/rad

   function Axis (Object : Input_Device; Axis : Absolute_Axis_Kind) return Axis_Info
     with Pre => Object.Is_Open;

   function Key_Statuses (Object : Input_Device) return Key_Features
     with Pre => Object.Is_Open;

   function LED_Statuses (Object : Input_Device) return LED_Features
     with Pre => Object.Is_Open;

   function Sound_Statuses (Object : Input_Device) return Sound_Features
     with Pre => Object.Is_Open;

   function Switch_Statuses (Object : Input_Device) return Switch_Features
     with Pre => Object.Is_Open;

   ----------------------------------------------------------------------------

   function Force_Feedback_Effects (Object : Input_Device) return Natural
     with Pre => Object.Is_Open;
   --  Return the number of concurrent force-feedback effects that
   --  the device supports

   type Force_Feedback_Gain is delta 0.01 digits 3 range 0.0 .. 1.0;

   type Force_Feedback_Auto_Center is delta 0.01 digits 3 range 0.0 .. 1.0;

   procedure Set_Force_Feedback_Gain
     (Object : Input_Device;
      Value  : Force_Feedback_Gain);

   procedure Set_Force_Feedback_Auto_Center
     (Object : Input_Device;
      Value  : Force_Feedback_Auto_Center);

   type Force_Feedback_Effect_ID is range -1 .. 95
     with Size => Interfaces.C.short'Size;

   subtype Uploaded_Force_Feedback_Effect_ID is Force_Feedback_Effect_ID
     range 0 .. Force_Feedback_Effect_ID'Last;

   function Play_Force_Feedback_Effect
     (Object     : Input_Device;
      Identifier : Uploaded_Force_Feedback_Effect_ID;
      Count      : Natural := 1) return Boolean;
   --  Play a force feedback effect and return whether playing is successful

   ----------------------------------------------------------------------------

   function Properties (Object : Input_Device) return Device_Properties
     with Pre => Object.Is_Open;

   function Events (Object : Input_Device) return Device_Events
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Synchronization_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Key_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Relative_Axis_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Absolute_Axis_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Switch_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Miscellaneous_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return LED_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Repeat_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Sound_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Force_Feedback_Features
     with Pre => Object.Is_Open;

   ----------------------------------------------------------------------------

   function Is_Open (Object : Input_Device) return Boolean;

   function Open
     (Object    : in out Input_Device;
      File_Name : String;
      Blocking  : Boolean := True) return Boolean
   with Pre => not Object.Is_Open;

   procedure Close (Object : in out Input_Device)
     with Pre => Object.Is_Open;

private

   type File_Descriptor is new Integer;

   type Input_Device is limited new Ada.Finalization.Limited_Controlled with record
      FD   : File_Descriptor := -1;
      Open : Boolean := False;
   end record;

   overriding procedure Finalize (Object : in out Input_Device);

   type Unsigned_64 is mod 2 ** 64
     with Size => 64;

   for Device_Properties use record
      Pointer          at 0 range 0 .. 0;
      Direct           at 0 range 1 .. 1;
      Button_Pad       at 0 range 2 .. 2;
      Semi_Multi_Touch at 0 range 3 .. 3;
      Top_Button_Pad   at 0 range 4 .. 4;
      Pointing_Stick   at 0 range 5 .. 5;
      Accelerometer    at 0 range 6 .. 6;
   end record;
   for Device_Properties'Size use 32;

   for Device_Events use record
      Synchronization at 0 range 0 .. 0;
      Keys            at 0 range 1 .. 1;
      Relative_Axes   at 0 range 2 .. 2;
      Absolute_Axes   at 0 range 3 .. 3;
      Miscellaneous   at 0 range 4 .. 4;
      Switches        at 0 range 5 .. 5;
      LEDs            at 0 range 17 .. 17;
      Sound           at 0 range 18 .. 18;
      Repeat          at 0 range 20 .. 20;
      Force_Feedback  at 0 range 21 .. 21;
      Power           at 0 range 22 .. 22;
      Feedback_Status at 0 range 23 .. 23;
   end record;
   for Device_Events'Size use 32;

   for Event_Kind use
     (Synchronization => 16#00#,
      Key             => 16#01#,
      Relative        => 16#02#,
      Absolute        => 16#03#,
      Miscellaneous   => 16#04#,
      Switch          => 16#05#,
      LED             => 16#11#,
      Sound           => 16#12#,
      Repeat          => 16#14#,
      Force_Feedback  => 16#15#,
      Power           => 16#16#,
      Feedback_Status => 16#17#);
   for Event_Kind'Size use Interfaces.C.unsigned_short'Size;

   for Synchronization_Kind use
     (Report    => 0,
      Config    => 1,
      MT_Report => 2,
      Dropped   => 3);
   for Synchronization_Kind'Size use 16;

   --  Representation clause for the *_Info_Kind types are needed
   --  because of holes in the representation values

   for Key_Kind use
     (Key_Unknown            => 240,
      Button_South           => 16#130#,
      Button_East            => 16#131#,
      Button_North           => 16#133#,
      Button_West            => 16#134#,
      Button_Trigger_Left_1  => 16#136#,
      Button_Trigger_Right_1 => 16#137#,
      Button_Trigger_Left_2  => 16#138#,
      Button_Trigger_Right_2 => 16#139#,
      Button_Select          => 16#13A#,
      Button_Start           => 16#13B#,
      Button_Mode            => 16#13C#,
      Button_Thumb_Left      => 16#13D#,
      Button_Thumb_Right     => 16#13E#);
   for Key_Kind'Size use Key_Code'Size;

   for Relative_Axis_Info_Kind use
     (X                         => 16#00#,
      Y                         => 16#01#,
      Z                         => 16#02#,
      Rx                        => 16#03#,
      Ry                        => 16#04#,
      Rz                        => 16#05#,
      Horizontal_Wheel          => 16#06#,
      Diagonal                  => 16#07#,
      Wheel                     => 16#08#,
      Misc                      => 16#09#,
      Wheel_High_Res            => 16#0B#,
      Horizontal_Wheel_High_Res => 16#0C#);
   for Relative_Axis_Info_Kind'Size use 16;

   for Relative_Axis_Kind'Size use Relative_Axis_Info_Kind'Size;

   for Absolute_Axis_Info_Kind use
     (X                => 16#00#,
      Y                => 16#01#,
      Z                => 16#02#,
      Rx               => 16#03#,
      Ry               => 16#04#,
      Rz               => 16#05#,
      Throttle         => 16#06#,
      Rudder           => 16#07#,
      Wheel            => 16#08#,
      Gas              => 16#09#,
      Brake            => 16#0A#,
      Hat_0X           => 16#10#,
      Hat_0Y           => 16#11#,
      Hat_1X           => 16#12#,
      Hat_1Y           => 16#13#,
      Hat_2X           => 16#14#,
      Hat_2Y           => 16#15#,
      Hat_3X           => 16#16#,
      Hat_3Y           => 16#17#,
      Pressure         => 16#18#,
      Distance         => 16#19#,
      Tilt_X           => 16#1A#,
      Tilt_Y           => 16#1B#,
      Tool_Width       => 16#1C#,
      Volume           => 16#20#,
      Misc             => 16#28#,
      MT_Slot          => 16#2F#,
      MT_Touch_Major   => 16#30#,
      MT_Touch_Minor   => 16#31#,
      MT_Width_Major   => 16#32#,
      MT_Width_Minor   => 16#33#,
      MT_Orientation   => 16#34#,
      MT_Position_X    => 16#35#,
      MT_Position_Y    => 16#36#,
      MT_Tool_Type     => 16#37#,
      MT_Blob_ID       => 16#38#,
      MT_Tracking_ID   => 16#39#,
      MT_Pressure      => 16#3A#,
      MT_Distance      => 16#3B#,
      MT_Tool_X        => 16#3C#,
      MT_Tool_Y        => 16#3D#);
   for Absolute_Axis_Info_Kind'Size use 64;

   for Absolute_Axis_Kind'Size use Absolute_Axis_Info_Kind'Size;

   for Switch_Kind use
     (Lid                  => 16#00#,
      Tablet_Mode          => 16#01#,
      Headphone_Insert     => 16#02#,
      Rfkill_all           => 16#03#,
      Microphone_Insert    => 16#04#,
      Dock                 => 16#05#,
      Lineout_Insert       => 16#06#,
      Jack_Physical_Insert => 16#07#,
      Video_Out_Insert     => 16#08#,
      Camera_Lens_Cover    => 16#09#,
      Keypad_Slide         => 16#0A#,
      Front_Proximity      => 16#0B#,
      Rotate_Lock          => 16#0C#,
      Line_In_Insert       => 16#0D#,
      Mute_Device          => 16#0E#,
      Pen_Inserted         => 16#0F#,
      Machine_Cover        => 16#10#);
   for Switch_Kind'Size use 17;

   for Miscellaneous_Kind use
     (Serial    => 16#00#,
      Pulse_LED => 16#01#,
      Gesture   => 16#02#,
      Raw       => 16#03#,
      Scan      => 16#04#,
      Timestamp => 16#05#);
   for Miscellaneous_Kind'Size use 8;

   for LED_Kind use
     (Num_Lock    => 16#00#,
      Caps_Lock   => 16#01#,
      Scroll_Lock => 16#02#,
      Compose     => 16#03#,
      Kana        => 16#04#,
      Sleep       => 16#05#,
      Suspend     => 16#06#,
      Mute        => 16#07#,
      Misc        => 16#08#,
      Mail        => 16#09#,
      Charging    => 16#0A#);
   for LED_Kind'Size use 16;

   for Repeat_Kind use
     (Repeat_Delay  => 16#00#,
      Repeat_Period => 16#01#);
   for Repeat_Kind'Size use 2;

   for Sound_Kind use
     (Click => 16#00#,
      Bell  => 16#01#,
      Tone  => 16#02#);
   for Sound_Kind'Size use 8;

end Event_Device;
