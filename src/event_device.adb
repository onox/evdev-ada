with System;

with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO.C_Streams;

with Event_Device.Input_Dev;

package body Event_Device is

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

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#02#, Result'Size / System.Storage_Unit), Result'Address);
   begin
      pragma Assert (Length >= 0);
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

   function Axis (Object : Input_Device; Axis : Axis_Kind) return Axis_Info is
      Result : aliased Axis_Info;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Axis_Kind, Target => Unsigned_8);

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 16#40# + Convert (Axis), Result'Size), Result'Address);
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
