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

with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;

package body Event_Device.Input_Dev is

   package L1 renames Ada.Characters.Latin_1;

   subtype Size_Type is Interfaces.C.size_t;

   type Signed_Size_Type is range -(2 ** (Size_Type'Size - 1)) ..
                                  +(2 ** (Size_Type'Size - 1) - 1);

   type Access_Flag is (Read_Only, Write_Only, Read_Write);

   for Access_Flag use
     (Read_Only  => 0,
      Write_Only => 1,
      Read_Write => 2);
   for Access_Flag'Size use int'Size;

   function C_Read
     (File_Descriptor : Interfaces.C.int;
      Buffer          : out Input_Event;
      Count           : Size_Type) return Signed_Size_Type
   with Import, Convention => C, External_Name => "read";

   function C_Write
     (File_Descriptor : Interfaces.C.int;
      Buffer          : Input_Event;
      Count           : Size_Type) return Signed_Size_Type
   with Import, Convention => C, External_Name => "write";

   function C_Open
     (Path_Name : String;
      Flags     : Access_Flag) return int
   with Import, Convention => C, External_Name => "open";

   function C_Close
     (File_Descriptor : Interfaces.C.int) return Interfaces.C.int
   with Import, Convention => C, External_Name => "close";

   function C_Ioctl
     (FD      : int;
      Request : unsigned_long;
      Value   : System.Address) return int
   with Import, Convention => C, External_Name => "ioctl";

   function C_Ioctl
     (FD      : int;
      Request : unsigned_long;
      Value   : int) return int
   with Import, Convention => C, External_Name => "ioctl";

   function Convert is new Ada.Unchecked_Conversion
     (Source => IOCTL_Command,
      Target => unsigned);

   ----------------------------------------------------------------------------

   function IO_Control
     (FD      : File_Descriptor;
      Command : IOCTL_Command;
      Value   : System.Address) return Integer
   is (Integer (C_Ioctl (int (FD), unsigned_long (Convert (Command)), Value)));

   function IO_Control
     (FD      : File_Descriptor;
      Command : IOCTL_Command;
      Value   : Integer) return Integer
   is (Integer (C_Ioctl (int (FD), unsigned_long (Convert (Command)), int (Value))));

   ----------------------------------------------------------------------------

   procedure Read
     (FD    : File_Descriptor;
      Event : out Input_Dev.Input_Event)
   is
      Count : constant Size_Type := Event'Size / System.Storage_Unit;

      Bytes_Read : Signed_Size_Type;
   begin
      Bytes_Read :=
        C_Read
          (File_Descriptor => Interfaces.C.int (FD),
           Buffer          => Event,
           Count           => Count);
      pragma Assert (Bytes_Read >= -1);

      case Bytes_Read is
         when Signed_Size_Type'First .. -1 =>
            raise Ada.IO_Exceptions.Device_Error;
         when 0 =>
            raise Ada.IO_Exceptions.End_Error;
         when others =>
            if Size_Type (Bytes_Read) /= Count then
               raise Ada.IO_Exceptions.Data_Error;
            end if;
      end case;
   end Read;

   procedure Write
     (FD    : File_Descriptor;
      Event : Input_Dev.Input_Event)
   is
      Count : constant Size_Type := Event'Size / System.Storage_Unit;

      Bytes_Written : Signed_Size_Type;
   begin
      Bytes_Written :=
        C_Write
          (File_Descriptor => Interfaces.C.int (FD),
           Buffer          => Event,
           Count           => Count);
      pragma Assert (Bytes_Written >= -1);

      case Bytes_Written is
         when Signed_Size_Type'First .. -1 =>
            raise Ada.IO_Exceptions.Device_Error;
         when others =>
            if Size_Type (Bytes_Written) /= Count then
               raise Ada.IO_Exceptions.Data_Error;
            end if;
      end case;
   end Write;

   function Open (File_Path : String) return File_Descriptor is
      FD : constant Integer := Integer (C_Open (File_Path & L1.NUL, Read_Write));
   begin
      if FD = -1 then
         raise Ada.IO_Exceptions.Use_Error with "Could not open device " & File_Path;
      else
         return File_Descriptor (FD);
      end if;
   end Open;

   procedure Close (FD : File_Descriptor) is
      Error_Code : constant Interfaces.C.int := C_Close (Interfaces.C.int (FD));
   begin
      if Error_Code = -1 then
         raise Ada.IO_Exceptions.Device_Error;
      end if;
   end Close;

end Event_Device.Input_Dev;
