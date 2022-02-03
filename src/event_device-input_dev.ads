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

with System;

private with Ada.Unchecked_Conversion;

private package Event_Device.Input_Dev is
   pragma Preelaborate;

   type Error_Kind is (Device, End_Of_File, Data, Would_Block);

   type Result (Is_Success : Boolean := False) is record
      case Is_Success is
         when True  => FD    : File_Descriptor;
         when False => Error : Error_Kind;
      end case;
   end record;

   use Interfaces.C;

   type Timeval is record
      Seconds      : long;
      Microseconds : long;
   end record;

   type Input_Event is record
      Time  : Timeval;
      Event : Event_Kind;
      Code  : unsigned_short;
      Value : int;
   end record
     with Convention => C;

   type Access_Mode is (None, Write, Read, Read_Write);

   type Unsigned_14 is mod 2 ** 14
     with Size => 14;

   type IOCTL_Command is record
      Mode   : Access_Mode;
      Letter : Character;
      Number : Unsigned_8;
      Size   : Unsigned_14;
   end record;

   function IO_Control
     (FD      : File_Descriptor;
      Command : IOCTL_Command;
      Value   : System.Address) return Integer;

   function IO_Control
     (FD      : File_Descriptor;
      Command : IOCTL_Command;
      Value   : Integer) return Integer;

   function Read
     (FD    : File_Descriptor;
      Event : out Input_Dev.Input_Event) return Result;

   function Write
     (FD    : File_Descriptor;
      Event : Input_Dev.Input_Event) return Result;

   function Open (File_Path : String; Blocking : Boolean) return Result;

   function Close (FD : File_Descriptor) return Result;

private

   for Access_Mode use
     (None       => 0,
      Write      => 1,
      Read       => 2,
      Read_Write => 3);
   for Access_Mode'Size use 2;

   for Timeval'Size use 2 * long'Size;
   for Input_Event'Size use 2 * long'Size + 2 * unsigned_short'Size + int'Size;

   for IOCTL_Command use record
     Number at 0 range  0 .. 7;
     Letter at 0 range  8 .. 15;
     Size   at 0 range 16 .. 29;
     Mode   at 0 range 30 .. 31;
   end record;
   for IOCTL_Command'Size use 32;

end Event_Device.Input_Dev;
