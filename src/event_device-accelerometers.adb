with Interfaces.C;

with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;

with Event_Device.Input_Dev;

package body Event_Device.Accelerometers is

   procedure Read
     (Object : Input_Device;
      Axes   : Absolute_Axis_Features;
      Value  : out State)
   is
      use Event_Device.Input_Dev;
      use type Interfaces.C.long;
      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.int;

      type Long_Axis_Value is delta 2.0 ** (-16)
        range -(2.0 ** 47) ..
              +(2.0 ** 47 - 2.0 ** (-16));

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Synchronization_Kind);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Key_Info_Kind);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Interfaces.C.unsigned_short, Target => Relative_Axis_Kind);

      function Convert is new Ada.Unchecked_Conversion
        (Source => Unsigned_64, Target => Absolute_Axis_Info_Kind);
      --  Convert to Absolute_Axis_Info_Kind first before converting to
      --  Absolute_Axis_Kind, because the former has a representation clause

      Event : Input_Event;
      Has_Dropped : Boolean := False;
   begin
      loop
         Input_Dev.Read (Object.FD, Event);

         case Event.Event is
            when Key =>
               declare
                  Code : constant Key_Kind := Key_Kind (Key_Info_Kind'(Convert (Event.Code)));
               begin
                  Value.Keys (Code) := (if Event.Value /= 0 then Pressed else Released);
               end;
            when Relative =>
               if not Has_Dropped then
                  declare
                     Code : constant Relative_Axis_Kind := Convert (Event.Code);
                  begin
                     --  TODO What values to expect? Just Integers?
                     Value.Relative (Code) := Integer (Event.Value);
                  end;
               end if;
            when Absolute =>
               if not Has_Dropped then
                  declare
                     Code : constant Absolute_Axis_Kind :=
                       Absolute_Axis_Kind (Convert (Unsigned_64 (Event.Code)));
                  begin
                     if Axes (Code) then
                        declare
                           Axis : constant Event_Device.Axis_Info := Object.Axis (Code);

                           Resolution : constant Integer :=
                             (if Axis.Resolution > 0 then
                                Axis.Resolution
                              else
                                (Axis.Maximum - Axis.Minimum));
                        begin
                           Value.Absolute (Code) :=
                             Axis_Value (Long_Axis_Value (Event.Value) / Resolution);
                        end;
                     else
                        raise Ada.IO_Exceptions.Data_Error with
                          "Unexpected absolute axis " & Code'Image;
                     end if;
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
