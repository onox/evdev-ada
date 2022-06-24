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

with Ada.Characters.Latin_1;

package body Event_Device.Input_Dev is

   package L1 renames Ada.Characters.Latin_1;

   subtype Size_Type is Interfaces.C.size_t;

   type Signed_Size_Type is range -(2 ** (Size_Type'Size - 1)) ..
                                  +(2 ** (Size_Type'Size - 1) - 1);

   type Access_Flag is
     (Read_Only, Write_Only, Read_Write,
      Non_Block_Read_Only, Non_Block_Write_Only, Non_Block_Read_Write);

   for Access_Flag use
     (Read_Only            => 0,
      Write_Only           => 1,
      Read_Write           => 2,
      Non_Block_Read_Only  => 4000,
      Non_Block_Write_Only => 4001,
      Non_Block_Read_Write => 4002);
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

   function Errno return int
     with Import, Convention => C, External_Name => "get_errno";

   function Error_Number return Integer is (Integer (Errno));

   Error_Again : constant := 11;

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

   function Read
     (FD    : File_Descriptor;
      Event : out Input_Event) return Result
   is
      Count : constant Size_Type := Event'Size / System.Storage_Unit;

      Bytes_Read : Signed_Size_Type;
   begin
      Bytes_Read :=
        C_Read
          (File_Descriptor => Interfaces.C.int (FD),
           Buffer          => Event,
           Count           => Count);

      case Bytes_Read is
         when Signed_Size_Type'First .. -1 =>
            if Error_Number = Error_Again then
               return (Is_Success => False, Error => Would_Block);
            else
               return (Is_Success => False, Error => Device);
            end if;
         when 0 =>
            return (Is_Success => False, Error => End_Of_File);
         when others =>
            if Size_Type (Bytes_Read) /= Count then
               return (Is_Success => False, Error => Data);
            end if;
            return (Is_Success => True, FD => FD);
      end case;
   end Read;

   function Write
     (FD    : File_Descriptor;
      Event : Input_Event) return Result
   is
      Count : constant Size_Type := Event'Size / System.Storage_Unit;

      Bytes_Written : Signed_Size_Type;
   begin
      Bytes_Written :=
        C_Write
          (File_Descriptor => Interfaces.C.int (FD),
           Buffer          => Event,
           Count           => Count);

      case Bytes_Written is
         when Signed_Size_Type'First .. -1 =>
            return (Is_Success => False, Error => Device);
         when others =>
            if Size_Type (Bytes_Written) /= Count then
               return (Is_Success => False, Error => Data);
            end if;
            return (Is_Success => True, FD => FD);
      end case;
   end Write;

   function Open (File_Path : String; Blocking : Boolean) return Result is
      FD : constant Integer := Integer (C_Open (File_Path & L1.NUL,
        (if Blocking then Read_Write else Non_Block_Read_Write)));
   begin
      if FD /= -1 then
         return (Is_Success => True, FD => File_Descriptor (FD));
      else
         return (Is_Success => False, Error => Device);
      end if;
   end Open;

   function Close (FD : File_Descriptor) return Result is
      Error_Code : constant Interfaces.C.int := C_Close (Interfaces.C.int (FD));
   begin
      if Error_Code /= -1 then
         return (Is_Success => True, FD => FD);
      else
         return (Is_Success => False, Error => Device);
      end if;
   end Close;

end Event_Device.Input_Dev;
