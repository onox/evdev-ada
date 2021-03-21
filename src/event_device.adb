with Interfaces.C.Strings;

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

   function ID (Object : Input_Device) return Device_ID is
      Result : Device_ID;

      use all type Event_Device.Input_Dev.Access_Mode;

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 2, Result'Size / System.Storage_Unit), Result'Address);
   begin
      return Result;
   end ID;

   function Location (Object : Input_Device) return String is
      Result : String (1 .. 128) := (others => ' ');

      use all type Event_Device.Input_Dev.Access_Mode;

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 7, Result'Length), Result'Address);
   begin
      pragma Assert (Length >= 0);
      return Result (1 .. Length);
   end Location;

   function Unique_ID (Object : Input_Device) return String is
      Result : String (1 .. 128) := (others => ' ');

      use all type Event_Device.Input_Dev.Access_Mode;

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 8, Result'Length), Result'Address);
   begin
      pragma Assert (Length >= 0);
      return Result (1 .. Length);
   end Unique_ID;

   function Properties (Object : Input_Device) return Device_Properties is
      Result : Device_Properties;

      use all type Event_Device.Input_Dev.Access_Mode;

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 9, Result'Size), Result'Address);
   begin
      pragma Assert (Error_Code /= -1);
      return Result;
   end Properties;

   function Axis (Object : Input_Device) return Axis_Info is
      Result : Axis_Info;
   begin
      --  FIXME Implement
      return Result;
   end Axis;

   function Name (Object : Input_Device) return String is
      Result : String (1 .. 128) := (others => ' ');

      use all type Event_Device.Input_Dev.Access_Mode;

      Length : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Read, 'E', 6, Result'Length), Result'Address);
   begin
      pragma Assert (Length >= 0);
      return Result (1 .. Length);
   end Name;

   function File_Name (Object : Input_Device) return String is
     (Name (Object.Event_File_Type));

   function Is_Open (Object : Input_Device) return Boolean is
     (Is_Open (Object.Event_File_Type));

   procedure Read (Object : Input_Device; Item : out Position) is
      use Event_Device.Input_Dev;
      use type Interfaces.C.long;
      use type Interfaces.C.unsigned_short;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Sync_Code_Kind);
      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Abs_Code_Kind);

      Event : Input_Event;
      Has_Dropped : Boolean := False;
   begin
      Item.Time := 0.0;

      loop
         Input_Event'Read (Object.Event_Stream, Event);

         if Item.Time = 0.0 then
            Item.Time := Duration (Event.Time.Seconds)
              + Duration (Event.Time.Microseconds) / 1.0e6;
         end if;

         case Event.Event is
            when Absolute =>
               if not Has_Dropped then
                  declare
                     Code : constant Abs_Code_Kind := Convert (Event.Code);

                     Resolution_Absolute   : constant := 8192.0;
                     Resolution_Rotational : constant := 1024.0;
                  begin
                     case Code is
                        --  Accelerometer (resolution = units/mm)
                        --  If INPUT_PROP_ACCELEROMETER is set, resolution = units/g
                        --  DS4 = 8192 units
                        when X =>
                           Item.X := Accel_Unit (Event.Value) / Resolution_Absolute;
                        when Y =>
                           Item.Y := Accel_Unit (Event.Value) / Resolution_Absolute;
                        when Z =>
                           Item.Z := Accel_Unit (Event.Value) / Resolution_Absolute;

                        --  Gyro (resolution = units/rad)
                        --  If INPUT_PROP_ACCELEROMETER is set, resolution = units/deg/s
                        --  DS4 = 1024 units
                        when Rx =>
                           Item.Rx := Gyro_Unit (Event.Value) / Resolution_Rotational;
                        when Ry =>
                           Item.Ry := Gyro_Unit (Event.Value) / Resolution_Rotational;
                        when Rz =>
                           Item.Rz := Gyro_Unit (Event.Value) / Resolution_Rotational;
                     end case;
                  end;
               end if;
            when Synchronization =>
               declare
                  Code : constant Sync_Code_Kind := Convert (Event.Code);
               begin
                  case Code is
                     when Report =>
                        exit;
                     when Dropped =>
                        Has_Dropped := True;
                     when others =>
                        null;
                  end case;
               end;
            when Master_Clock =>
               null;
            when others =>
               raise Program_Error with Event.Event'Image;
         end case;
      end loop;
   end Read;

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
