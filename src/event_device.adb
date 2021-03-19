with Interfaces.C.Strings;

with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO.C_Streams;

with Event_Device.Input_Dev;

package body Event_Device is

   function FD (Object : Input_Device) return Integer is
     (C_Streams.ICS.fileno (C_Streams.C_Stream (Object.Event_File_Type)));

   function Device (Object : Input_Device) return Device_Info is
      Result : Device_Info;
   begin
      --  FIXME Implement
      return Result;
   end Device;

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
