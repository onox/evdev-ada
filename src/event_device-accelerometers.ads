package Event_Device.Accelerometers is
   pragma Preelaborate;

   type Key_State is (Released, Pressed);

   type Axis_Value is delta 2.0 ** (-16) range -(2.0 ** 15) .. +(2.0 ** 15 - 2.0 ** (-16));

   type Key_Values is array (Key_Kind) of Key_State;

   type Relative_Axis_Values is array (Relative_Axis_Kind) of Integer;
   type Absolute_Axis_Values is array (Absolute_Axis_Kind) of Axis_Value;

   type State is record
      Keys :     Key_Values           := (others => Released);
      Relative : Relative_Axis_Values := (others => 0);
      Absolute : Absolute_Axis_Values := (others => 0.0);
      Time     : Duration    := 0.0;
   end record;

   procedure Read
     (Object : Input_Device;
      Axes   : Absolute_Axis_Features;
      Value  : out State)
   with Pre => Object.Is_Open;
   --  Read keys, relative, and absolute axes of device
   --
   --  Resolution of accelerometer and gyro axes:
   --
   --            Accelerometer  No accelerometer
   --            -------------  --------
   --  X/Y/Z:    units/g        units/mm
   --  Rx/Ry/Rz: units/deg/s    units/rad

end Event_Device.Accelerometers;
