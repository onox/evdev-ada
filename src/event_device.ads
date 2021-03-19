private with Ada.Streams.Stream_IO;
private with Ada.Finalization;

package Event_Device is

   type Accel_Unit is delta 2.0 ** (-13) range -(2.0 ** 15) .. +(2.0 ** 15 - 1.0);
   --  32768 / 8192 = 4
   type Gyro_Unit is delta 2.0 ** (-10) range -(2.0 ** 21) .. +(2.0 ** 21 - 1.0);
   --  2097152 / 1024 = 2048

   type Unsigned_16 is mod 2 ** 16
     with Size => 16;

   type Position is record
      X, Y, Z    : Accel_Unit := 0.0;
      --  Linear acceleration

      Rx, Ry, Rz : Gyro_Unit  := 0.0;
      --  Angular velocity

      Time       : Duration   := 0.0;
   end record;

   type Device_Info is record
      Bus_Type, Vendor, Product, Version : Unsigned_16;
   end record;

   type Axis_Info is record
      Value, Minimum, Maximum, Fuzz, Flat, Resolution : Integer;
   end record;

   type Input_Device is tagged limited private;

   function Device (Object : Input_Device) return Device_Info;

   function Axis (Object : Input_Device) return Axis_Info;

   function Name (Object : Input_Device) return String;

   function File_Name (Object : Input_Device) return String;

   function Is_Open (Object : Input_Device) return Boolean;

   procedure Read (Object : Input_Device; Item : out Position)
     with Pre => Object.Is_Open;

   procedure Open (Object : in out Input_Device; File_Name : String)
     with Pre  => not Object.Is_Open,
          Post =>     Object.Is_Open;

   procedure Close (Object : in out Input_Device)
     with Pre =>      Object.Is_Open,
          Post => not Object.Is_Open;

private

   use Ada.Streams.Stream_IO;

   type Input_Device is limited new Ada.Finalization.Limited_Controlled with record
      Event_File_Type : File_Type;
      Event_Stream    : Stream_Access;
   end record;

   overriding procedure Finalize (Object : in out Input_Device);

end Event_Device;
