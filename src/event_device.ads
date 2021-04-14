private with Interfaces.C;

private with Ada.Finalization;
private with Ada.Streams.Stream_IO;
private with Ada.Unchecked_Conversion;

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

   type Event_Kind is
     (Synchronization,
      Key,
      Relative,
      Absolute,
      Miscellaneous,
      Switch,
      LED,
      Sound,
      Repeat,
      Force_Feedback,
      Power,
      Feedback_Status);

   type Synchronization_Kind is (Report, Config, MT_Report, Dropped);

   type Relative_Axis_Kind is (X, Y, Z, Rx, Ry, Rz, Horizontal_Wheel, Diagonal, Wheel, Misc);

   type Absolute_Axis_Kind is
     (X,
      Y,
      Z,
      Rx,
      Ry,
      Rz,
      Throttle,
      Rudder,
      Wheel,
      Gas,
      Brake,
      Hat_0X,
      Hat_0Y,
      Hat_1X,
      Hat_1Y,
      Hat_2X,
      Hat_2Y,
      Hat_3X,
      Hat_3Y,
      Pressure,
      Distance,
      Tilt_X,
      Tilt_Y,
      Tool_Width,
      Volume,
      Misc,
      MT_Slot,
      MT_Touch_Major,
      MT_Touch_Minor,
      MT_Width_Major,
      MT_Width_Minor,
      MT_Orientation,
      MT_Position_X,
      MT_Position_Y,
      MT_Tool_Type,
      MT_Blob_ID,
      MT_Tracking_ID,
      MT_Pressure,
      MT_Distance,
      MT_Tool_X,
      MT_Tool_Y);

   type Switch_Kind is
     (Lid,
      Tablet_Mode,
      Headphone_Insert,
      Rfkill_all,
      Microphone_Insert,
      Dock,
      Lineout_Insert,
      Jack_Physical_Insert,
      Video_Out_Insert,
      Camera_Lens_Cover,
      Keypad_Slide,
      Front_Proximity,
      Rotate_Lock,
      Line_In_Insert,
      Mute_Device,
      Pen_Inserted,
      Machine_Cover);

   type Miscellaneous_Kind is (Serial, Pulse_LED, Gesture, Raw, Scan, Timestamp);

   type LED_Kind is
     (Num_Lock, Caps_Lock, Scroll_Lock, Compose, Kana, Sleep, Suspend, Mute, Misc, Mail, Charging);

   type Repeat_Kind is (Repeat_Delay, Repeat_Period);

   type Sound_Kind is (Click, Bell, Tone);

   type Force_Feedback_Kind is
     (Rumble,
      Periodic,
      Constant_V,
      Spring,
      Friction,
      Damper,
      Inertia,
      Ramp,
      Square,
      Triangle,
      Sine,
      Saw_Up,
      Saw_Down,
      Custom,
      Gain,
      Auto_Center);

   type Synchronization_Features is array (Synchronization_Kind) of Boolean
     with Component_Size => 1;

   type Relative_Axis_Features is array (Relative_Axis_Kind) of Boolean
     with Component_Size => 1;

   type Absolute_Axis_Features is array (Absolute_Axis_Kind) of Boolean
     with Component_Size => 1;

   type Switch_Features is array (Switch_Kind) of Boolean
     with Component_Size => 1;

   type Miscellaneous_Features is array (Miscellaneous_Kind) of Boolean
     with Component_Size => 1;

   type LED_Features is array (LED_Kind) of Boolean
     with Component_Size => 1;

   type Repeat_Features is array (Repeat_Kind) of Boolean
     with Component_Size => 1;

   type Sound_Features is array (Sound_Kind) of Boolean
     with Component_Size => 1;

   type Force_Feedback_Features is array (Force_Feedback_Kind) of Boolean
     with Component_Size => 1;

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

   function Features (Object : Input_Device) return Synchronization_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Relative_Axis_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Absolute_Axis_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Switch_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Miscellaneous_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return LED_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Repeat_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Sound_Features
     with Pre => Object.Is_Open;

   function Features (Object : Input_Device) return Force_Feedback_Features
     with Pre => Object.Is_Open;

   function Name (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function File_Name (Object : Input_Device) return String
     with Pre => Object.Is_Open;

   function Axis (Object : Input_Device; Axis : Absolute_Axis_Kind) return Axis_Info
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

   type Unsigned_64 is mod 2 ** 64
     with Size => 64;

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
   --  TODO INPUT_PROP_CNT = 32

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

   for Event_Kind use
     (Synchronization => 16#00#,
      Key             => 16#01#,
      Relative        => 16#02#,
      Absolute        => 16#03#,
      Miscellaneous   => 16#04#,
      Switch          => 16#05#,
      LED             => 16#11#,
      Sound           => 16#12#,
      Repeat          => 16#14#,
      Force_Feedback  => 16#15#,
      Power           => 16#16#,
      Feedback_Status => 16#17#);
   for Event_Kind'Size use Interfaces.C.unsigned_short'Size;

   for Synchronization_Kind use
     (Report    => 0,
      Config    => 1,
      MT_Report => 2,
      Dropped   => 3);
   for Synchronization_Kind'Size use 16;

   --   for Key_'Size use 768;

   for Relative_Axis_Kind use
     (X                => 16#00#,
      Y                => 16#01#,
      Z                => 16#02#,
      Rx               => 16#03#,
      Ry               => 16#04#,
      Rz               => 16#05#,
      Horizontal_Wheel => 16#06#,
      Diagonal         => 16#07#,
      Wheel            => 16#08#,
      Misc             => 16#09#);
   for Relative_Axis_Kind'Size use 16;

   for Absolute_Axis_Kind'Size use 64;

   for Switch_Kind use
     (Lid                  => 16#00#,
      Tablet_Mode          => 16#01#,
      Headphone_Insert     => 16#02#,
      Rfkill_all           => 16#03#,
      Microphone_Insert    => 16#04#,
      Dock                 => 16#05#,
      Lineout_Insert       => 16#06#,
      Jack_Physical_Insert => 16#07#,
      Video_Out_Insert     => 16#08#,
      Camera_Lens_Cover    => 16#09#,
      Keypad_Slide         => 16#0A#,
      Front_Proximity      => 16#0B#,
      Rotate_Lock          => 16#0C#,
      Line_In_Insert       => 16#0D#,
      Mute_Device          => 16#0E#,
      Pen_Inserted         => 16#0F#,
      Machine_Cover        => 16#10#);
   for Switch_Kind'Size use 17;

   for Miscellaneous_Kind use
     (Serial    => 16#00#,
      Pulse_LED => 16#01#,
      Gesture   => 16#02#,
      Raw       => 16#03#,
      Scan      => 16#04#,
      Timestamp => 16#05#);
   for Miscellaneous_Kind'Size use 8;

   for LED_Kind use
     (Num_Lock    => 16#00#,
      Caps_Lock   => 16#01#,
      Scroll_Lock => 16#02#,
      Compose     => 16#03#,
      Kana        => 16#04#,
      Sleep       => 16#05#,
      Suspend     => 16#06#,
      Mute        => 16#07#,
      Misc        => 16#08#,
      Mail        => 16#09#,
      Charging    => 16#0A#);
   for LED_Kind'Size use 16;

   for Repeat_Kind use
     (Repeat_Delay  => 16#00#,
      Repeat_Period => 16#01#);
   for Repeat_Kind'Size use 2;

   for Sound_Kind use
     (Click => 16#00#,
      Bell  => 16#01#,
      Tone  => 16#02#);
   for Sound_Kind'Size use 8;

end Event_Device;
