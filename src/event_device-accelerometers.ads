package Event_Device.Accelerometers is
   pragma Pure;

   type Accel_Unit is delta 2.0 ** (-13) range -(2.0 ** 15) .. +(2.0 ** 15 - 2.0 ** (-13));
   --  32768 / 8192 = 4
   type Gyro_Unit is delta 2.0 ** (-10) range -(2.0 ** 21) .. +(2.0 ** 21 - 2.0 ** (-10));
   --  2097152 / 1024 = 2048

   type Measurement is record
      X, Y, Z    : Accel_Unit := 0.0;
      --  Linear acceleration

      Rx, Ry, Rz : Gyro_Unit  := 0.0;
      --  Angular velocity

      Time       : Duration   := 0.0;
   end record;

   type Accelerometer is limited new Input_Device with private;

   procedure Read (Object : Accelerometer; Value : out Measurement)
     with Pre => Object.Is_Open and Object.Properties.Accelerometer;

private

   type Accelerometer is limited new Input_Device with null record;

end Event_Device.Accelerometers;
