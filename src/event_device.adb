with System;

with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO.C_Streams;

with Event_Device.Input_Dev;

package body Event_Device is

   subtype Unused_Type is Boolean range False .. False;

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

   function FD (Object : Input_Device) return Integer is
     (C_Streams.ICS.fileno (C_Streams.C_Stream (Object.Event_File_Type)));

   function Hex_Image (Value : Unsigned_8) return String is
      use Ada.Streams;

      Hex : constant array (Unsigned_8 range 0 .. 15) of Character := "0123456789abcdef";
   begin
      return Hex (Value / 16) & Hex (Value mod 16);
   end Hex_Image;

   function Hex_Image (Value : Unsigned_16) return String is
      Low  : constant Unsigned_8 := Unsigned_8 (16#FF# and Value);
      High : constant Unsigned_8 := Unsigned_8 (16#FF# and (Value / 256));
   begin
      return Hex_Image (High) & Hex_Image (Low);
   end Hex_Image;

   use all type Event_Device.Input_Dev.Access_Mode;
   use type Event_Device.Input_Dev.Unsigned_14;

   function ID (Object : Input_Device) return Device_ID is
      Result : aliased Device_ID;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#02#, Result'Size / System.Storage_Unit), Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end ID;

   function Location (Object : Input_Device) return String is
      Result : aliased String (1 .. 128) := (others => ' ');

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#07#, Result'Length), Result'Address);
   begin
      pragma Assert (Length >= 0);
      return Result (1 .. Length);
   end Location;

   function Unique_ID (Object : Input_Device) return String is
      Result : aliased String (1 .. 128) := (others => ' ');

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#08#, Result'Length), Result'Address);
   begin
      pragma Assert (Length >= 0);
      return Result (1 .. Length);
   end Unique_ID;

   function Properties (Object : Input_Device) return Device_Properties is
      Result : aliased Device_Properties;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#09#, Result'Size), Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Properties;

   function Events (Object : Input_Device) return Device_Events is
      Result : aliased Device_Events;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20#, Result'Size), Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Events;

   function Convert is new Ada.Unchecked_Conversion
     (Source => Event_Kind, Target => Interfaces.C.unsigned_short);

   function Features (Object : Input_Device) return Synchronization_Features is
      Result : aliased Synchronization_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Synchronization)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Relative_Axis_Features is
      Result : aliased Relative_Axis_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Relative)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Absolute_Axis_Features is
      Result : aliased Absolute_Axis_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Absolute)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Switch_Features is
      Result : aliased Switch_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Switch)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Miscellaneous_Features is
      Result : aliased Miscellaneous_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Miscellaneous)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return LED_Features is
      Result : aliased LED_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (LED)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Repeat_Features is
      Result : aliased Repeat_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Repeat)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Sound_Features is
      Result : aliased Sound_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Sound)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Features;

   function Features (Object : Input_Device) return Force_Feedback_Features is
      Result : aliased Internal_Force_Feedback_Features;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#20# + Unsigned_8 (Convert (Force_Feedback)), Result'Size),
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

   function Axis (Object : Input_Device; Axis : Absolute_Axis_Kind) return Axis_Info is
      Result : aliased Axis_Info;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Absolute_Axis_Kind, Target => Unsigned_64);

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#40# + Unsigned_8 (Convert (Axis)), Result'Size),
         Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Axis;

   function Name (Object : Input_Device) return String is
      Result : aliased String (1 .. 128) := (others => ' ');

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#06#, Result'Length), Result'Address);
   begin
      pragma Assert (Length >= 0);
      return Result (1 .. Length);
   end Name;

   function File_Name (Object : Input_Device) return String is
     (Name (Object.Event_File_Type));

   function Is_Open (Object : Input_Device) return Boolean is
     (Is_Open (Object.Event_File_Type));

   procedure Open (Object : in out Input_Device; File_Name : String) is
   begin
      Open (Object.Event_File_Type, In_File, File_Name);
      Object.Event_Stream := Stream (Object.Event_File_Type);
   exception
      when Ada.IO_Exceptions.Use_Error =>
         raise Ada.IO_Exceptions.Use_Error with "Could not open device " & File_Name;
   end Open;

   procedure Close (Object : in out Input_Device) is
   begin
      Close (Object.Event_File_Type);
   end Close;

   overriding procedure Finalize (Object : in out Input_Device) is
   begin
      if Object.Is_Open then
         Object.Close;
      end if;
   end Finalize;

end Event_Device;
