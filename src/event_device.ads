private with Ada.Streams.Stream_IO;
private with Ada.Finalization;

package Event_Device is

   type Unsigned_8 is mod 2 ** 8
     with Size => 8;

   type Unsigned_16 is mod 2 ** 16
     with Size => 16;

   function Hex_Image (Value : Unsigned_8) return String;

   function Hex_Image (Value : Unsigned_16) return String;

   type Device_ID is record
      Bus_Type, Vendor, Product, Version : Unsigned_16;
   end record;

   type Axis_Info is record
      Value, Minimum, Maximum, Fuzz, Flat, Resolution : Integer;
   end record;

   type Axis_Kind is
     (X, Y, Z, Rx, Ry, Rz,
      Throttle, Rudder, Wheel, Gas, Brake,
      Hat_0X, Hat_0Y, Hat_1X, Hat_1Y,
      Hat_2X, Hat_2Y, Hat_3X, Hat_3Y,
      Pressure, Distance, Tilt_X, Tilt_Y,
      Tool_Width, Volume);

   type Device_Properties is record
      Pointer          : Boolean := False;
      Direct           : Boolean := False;
      Button_Pad       : Boolean := False;
      Semi_Multi_Touch : Boolean := False;
      Top_Button_Pad   : Boolean := False;
      Pointing_Stick   : Boolean := False;
      Accelerometer    : Boolean := False;
   end record;

   type Device_Events is record
      Synchronization : Boolean := False;
      Keys            : Boolean := False;
      Relative_Axes   : Boolean := False;
      Absolute_Axes   : Boolean := False;
      Miscellaneous   : Boolean := False;
      Switches        : Boolean := False;
      LEDs            : Boolean := False;
      Sound           : Boolean := False;
      Repeat          : Boolean := False;
      Force_Feedback  : Boolean := False;
      Power           : Boolean := False;
      Feedback_Status : Boolean := False;
   end record;

   type Input_Device is tagged limited private;

   function ID (Object : Input_Device) return Device_ID
     with Pre => Object.Is_Open;

   function Location (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function Unique_ID (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function Properties (Object : Input_Device) return Device_Properties
     with Pre => Object.Is_Open;

   function Events (Object : Input_Device) return Device_Events
     with Pre => Object.Is_Open;

   function Name (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function File_Name (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function Axis (Object : Input_Device; Axis : Axis_Kind) return Axis_Info
     with Pre => Object.Is_Open;

   function Is_Open (Object : Input_Device) return Boolean;

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

   for Device_Properties use record
      Pointer          at 0 range 0 .. 0;
      Direct           at 0 range 1 .. 1;
      Button_Pad       at 0 range 2 .. 2;
      Semi_Multi_Touch at 0 range 3 .. 3;
      Top_Button_Pad   at 0 range 4 .. 4;
      Pointing_Stick   at 0 range 5 .. 5;
      Accelerometer    at 0 range 6 .. 6;
   end record;
   for Device_Properties'Size use 8;

   for Device_Events use record
      Synchronization at 0 range 0 .. 0;
      Keys            at 0 range 1 .. 1;
      Relative_Axes   at 0 range 2 .. 2;
      Absolute_Axes   at 0 range 3 .. 3;
      Miscellaneous   at 0 range 4 .. 4;
      Switches        at 0 range 5 .. 5;
      LEDs            at 0 range 17 .. 17;
      Sound           at 0 range 18 .. 18;
      Repeat          at 0 range 20 .. 20;
      Force_Feedback  at 0 range 21 .. 21;
      Power           at 0 range 22 .. 22;
      Feedback_Status at 0 range 23 .. 23;
   end record;
   for Device_Events'Size use 32;

   for Axis_Kind use
     (X          => 0,
      Y          => 1,
      Z          => 2,
      Rx         => 3,
      Ry         => 4,
      Rz         => 5,
      Throttle   => 6,
      Rudder     => 7,
      Wheel      => 8,
      Gas        => 9,
      Brake      => 10,
      Hat_0X     => 16,
      Hat_0Y     => 17,
      Hat_1X     => 18,
      Hat_1Y     => 19,
      Hat_2X     => 20,
      Hat_2Y     => 21,
      Hat_3X     => 22,
      Hat_3Y     => 23,
      Pressure   => 24,
      Distance   => 25,
      Tilt_X     => 26,
      Tilt_Y     => 27,
      Tool_Width => 28,
      Volume     => 32);
   for Axis_Kind'Size use Unsigned_8'Size;

end Event_Device;
