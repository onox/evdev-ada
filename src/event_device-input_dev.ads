with System;

private with Ada.Unchecked_Conversion;

private package Event_Device.Input_Dev is
   pragma Pure;

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

   procedure Read
     (FD    : File_Descriptor;
      Event : out Input_Dev.Input_Event);

   procedure Write
     (FD    : File_Descriptor;
      Event : Input_Dev.Input_Event);

   function Open (File_Path : String) return File_Descriptor;

   procedure Close (FD : File_Descriptor);

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

   function ioctl
     (FD      : int;
      Request : unsigned_long;
      Value   : System.Address) return int
   with Import, Convention => C, External_Name => "ioctl";

   function ioctl
     (FD      : int;
      Request : unsigned_long;
      Value   : int) return int
   with Import, Convention => C, External_Name => "ioctl";

   function Convert is new Ada.Unchecked_Conversion
     (Source => IOCTL_Command,
      Target => unsigned);

   function IO_Control
     (FD      : File_Descriptor;
      Command : IOCTL_Command;
      Value   : System.Address) return Integer
   is (Integer (ioctl (int (FD), unsigned_long (Convert (Command)), Value)));

   function IO_Control
     (FD      : File_Descriptor;
      Command : IOCTL_Command;
      Value   : Integer) return Integer
   is (Integer (ioctl (int (FD), unsigned_long (Convert (Command)), int (Value))));

end Event_Device.Input_Dev;
