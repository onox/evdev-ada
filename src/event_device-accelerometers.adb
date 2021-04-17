with Interfaces.C;

with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;

with Event_Device.Input_Dev;

package body Event_Device.Accelerometers is

   overriding
   procedure Open (Object : in out Accelerometer; File_Name : String) is
   begin
      Input_Device (Object).Open (File_Name);
   end Open;

   procedure Read (Object : Accelerometer; Value : out Measurement) is
      use Event_Device.Input_Dev;
      use type Interfaces.C.long;
      use type Interfaces.C.unsigned_short;

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Synchronization_Kind);
      function Convert is new Ada.Unchecked_Conversion
        (Source => Unsigned_64, Target => Absolute_Axis_Kind);

      Event : Input_Event;
      Has_Dropped : Boolean := False;
   begin
      loop
         Input_Dev.Read (Object.FD, Event);

         case Event.Event is
            when Absolute =>
               if not Has_Dropped then
                  declare
                     Code : constant Absolute_Axis_Kind := Convert (Unsigned_64 (Event.Code));

                     Resolution_Absolute   : constant := 8192.0;
                     Resolution_Rotational : constant := 1024.0;
                  begin
                     case Code is
                        --  Accelerometer (resolution = units/mm)
                        --  If INPUT_PROP_ACCELEROMETER is set, resolution = units/g
                        --  DS4 = 8192 units
                        when X =>
                           Value.X := Accel_Unit (Event.Value) / Resolution_Absolute;
                        when Y =>
                           Value.Y := Accel_Unit (Event.Value) / Resolution_Absolute;
                        when Z =>
                           Value.Z := Accel_Unit (Event.Value) / Resolution_Absolute;

                        --  Gyro (resolution = units/rad)
                        --  If INPUT_PROP_ACCELEROMETER is set, resolution = units/deg/s
                        --  DS4 = 1024 units
                        when Rx =>
                           Value.Rx := Gyro_Unit (Event.Value) / Resolution_Rotational;
                        when Ry =>
                           Value.Ry := Gyro_Unit (Event.Value) / Resolution_Rotational;
                        when Rz =>
                           Value.Rz := Gyro_Unit (Event.Value) / Resolution_Rotational;

                        when others =>
                           raise Ada.IO_Exceptions.Data_Error with "Unexpected axis " & Code'Image;
                     end case;
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
            when Miscellaneous =>
               null;
            when others =>
               raise Ada.IO_Exceptions.Data_Error with "Unexpected event " & Event.Event'Image;
         end case;
      end loop;
   end Read;

end Event_Device.Accelerometers;
