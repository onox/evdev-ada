with Interfaces.C;

with System;

private with Ada.Unchecked_Conversion;

private package Event_Device.Input_Dev is

   use Interfaces.C;

   type Timeval is record
      Seconds      : long;
      Microseconds : long;
   end record;

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

   --  Rel_Code_Kind:
   --  X, Y, Z, Rx, Ry, Rz, Horizontal_Wheel, Diagonal, Wheel, Misc

   --  X, Y, Z, Rx, Ry, Rz
   --  Throttle, Rudder, Wheel, Gas, Brake,
   --  Hat0x, Hat0y, Hat1x, Hat1y, Hat2x, Hat2y, Hat3x, Hat3y,
   --  Pressure, Distance, Tilt_X, Tilt_Y, Tool_Width, Volume

   --  Switch_Code_Kind:
   --  Lid, Tablet_Mode, Headphone_Insert, Rfkill_all, Radio
   --  Microphone_Insert, Dock, Lineout_Insert, JAck_Physical_Insert, Video_Out_Insert,
   --  Camera_Lens_Cover,  Keypad_Slide, Front_Proximity, Rotate_Lock, Line_In_Insert,
   --  Mute_Device, Pen_Inserted, Machine_Cover

   --  Clock_Code_Kind:
   --  Serial, Pulse_Led, Gesture, Raw, Scan, Timestamp

   --  LEDs:
   --  Num_Lock, Caps_Lock Scroll_Lock, Compose, Kana, Sleep, Suspend,
   --  Mute, Misc, Mail, Charging

   --  Autorepeat:
   --  Delay, Period

   --  Sounds:
   --  Click, Bell, Tone

   type Sync_Code_Kind is (Report, Config, Mt_Report, Dropped);

   type Input_Event is record
      Time  : Timeval;
      Event : Event_Kind;
      Code  : unsigned_short;
      Value : int;
   end record;

   type Access_Mode is (None, Write, Read, Read_Write);

   for Access_Mode use
     (None       => 0,
      Write      => 1,
      Read       => 2,
      Read_Write => 3);
   for Access_Mode'Size use 2;

   type Unsigned_14 is mod 2 ** 14
     with Size => 14;

   type IOCTL_Command is record
      Mode   : Access_Mode;
      Letter : Character;
      Number : Unsigned_8;
      Size   : Unsigned_14;
   end record;

   function IO_Control
     (FD      : Integer;
      Command : IOCTL_Command;
      Value   : System.Address) return Integer;

   Msc_Scan      : constant := 4;
   Msc_Timestamp : constant := 5;

   --  Force-feedback / rumble
   FF_Rumble    : constant := 80;
   FF_Periodic  : constant := 81;
   FF_Square    : constant := 88;
   FF_Triangle  : constant := 89;
   FF_Sine      : constant := 90;
   FF_Gain      : constant := 96;

private

   for Abs_Code_Kind use
     (X  => 0,
      Y  => 1,
      Z  => 2,
      Rx => 3,
      Ry => 4,
      Rz => 5);
   for Abs_Code_Kind'Size use unsigned_short'Size;

   for Sync_Code_Kind use
     (Report    => 0,
      Config    => 1,
      Mt_Report => 2,
      Dropped   => 3);
   for Sync_Code_Kind'Size use unsigned_short'Size;

   for Event_Kind use
     (Synchronization => 0,
      Key             => 1,
      Relative        => 2,
      Absolute        => 3,
      Miscellaneous   => 4,
      Switch          => 5,
      LED             => 17,
      Sound           => 18,
      Repeat          => 20,
      Force_Feedback  => 21,
      Power           => 22,
      Feedback_Status => 23);
   for Event_Kind'Size use unsigned_short'Size;

   for Timeval'Size use 2 * long'Size;
   for Input_Event'Size use 2 * long'Size + 2 * unsigned_short'Size + int'Size;

   for IOCTL_Command use record
     Number at 0 range  0 .. 7;
     Letter at 0 range  8 .. 15;
     Size   at 0 range 16 .. 29;
     Mode   at 0 range 30 .. 31;
   end record;
   for IOCTL_Command'Size use 32;

   function ioctl
     (FD      : int;
      Request : unsigned_long;
      Value   : System.Address) return int
   with Import, Convention => C, External_Name => "ioctl";

   function Convert is new Ada.Unchecked_Conversion
     (Source => Event_Device.Input_Dev.IOCTL_Command,
      Target => unsigned);

   function IO_Control
     (FD      : Integer;
      Command : IOCTL_Command;
      Value   : System.Address) return Integer
   is (Integer (ioctl (int (FD), unsigned_long (Convert (Command)), Value)));

end Event_Device.Input_Dev;
