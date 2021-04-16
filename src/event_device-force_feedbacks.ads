with Interfaces.C;

with System;

package Event_Device.Force_Feedbacks is

   use Interfaces.C;

   type Duration_MS is private;

   function From_Duration (Value : Duration) return Duration_MS;

   function Image (Value : Duration_MS) return String;

   type Force_Feedback_Replay is record
      Length      : Duration_MS;
      Start_Delay : Duration_MS;
   end record
     with Convention => C_Pass_By_Copy;

   type Force_Feedback_Trigger is record
      Button   : Unsigned_16;
      Interval : Duration_MS;
   end record
     with Convention => C_Pass_By_Copy;

   type Force_Feedback_Envelope is record
      Attack_Length : Duration_MS;
      Attack_Level  : Unsigned_16;
      Fade_Length   : Duration_MS;
      Fade_Level    : Unsigned_16;
   end record
     with Convention => C_Pass_By_Copy;

   type Force_Feedback_Constant_Effect is record
      Level    : short;
      Envelope : Force_Feedback_Envelope;
   end record
     with Convention => C_Pass_By_Copy;

   type Force_Feedback_Ramp_Effect is record
      Start_Level : short;
      End_Level   : short;
      Envelope    : Force_Feedback_Envelope;
   end record
     with Convention => C_Pass_By_Copy;

   type Force_Feedback_Condition_Effect is record
      Right_Saturation : Unsigned_16;
      Left_Saturation  : Unsigned_16;
      Right_Coeff      : short;
      Left_Coeff       : short;
      Deadband         : Unsigned_16;
      Center           : short;
   end record
     with Convention => C_Pass_By_Copy;

   type Periodic_Wave_Kind is (Square, Triangle, Sine, Saw_Up, Saw_Down);
   --  Custom is not supported

   type Force_Feedback_Periodic_Effect is record
      Waveform    : Periodic_Wave_Kind;
      Period      : Duration_MS;
      Magnitude   : short;
      Offset      : short;
      Phase       : Unsigned_16;  --  0 .. 359 deg => 0 .. 2**16
      Envelope    : Force_Feedback_Envelope;
      Custom_Len  : unsigned       := 0;
      Custom_Data : System.Address := System.Null_Address;
   end record
     with Convention => C_Pass_By_Copy;

   type Force_Feedback_Rumble_Effect is record
      Strong_Magnitude : Unsigned_16;
      Weak_Magnitude   : Unsigned_16;
   end record
     with Convention => C_Pass_By_Copy;

   type Force_Feedback_Effect_Kind is
     (Rumble, Periodic, Constant_V, Spring, Friction, Damper, Inertia, Ramp);

   for Force_Feedback_Effect_Kind use
     (Rumble     => 16#50#,
      Periodic   => 16#51#,
      Constant_V => 16#52#,
      Spring     => 16#53#,
      Friction   => 16#54#,
      Damper     => 16#55#,
      Inertia    => 16#56#,
      Ramp       => 16#57#);
   for Force_Feedback_Effect_Kind'Size use Unsigned_16'Size;

   type Direction_Kind is (Down, Left, Up, Right);

   type Axis_Direction is (X, Y);

   type Condition_Effect_Array is array (Axis_Direction) of Force_Feedback_Condition_Effect
     with Convention => C;

   type Force_Feedback_Effect_Union (Kind : Force_Feedback_Effect_Kind := Rumble) is record
      case Kind is
         when Constant_V =>
            Constant_Effect : Force_Feedback_Constant_Effect;
         when Ramp =>
            Ramp_Effect     : Force_Feedback_Ramp_Effect;
         when Periodic =>
            Periodic_Effect : Force_Feedback_Periodic_Effect;
         when Spring | Friction | Damper | Inertia =>
            Condition_Effects : Condition_Effect_Array;
         when Rumble =>
            Rumble_Effect   : Force_Feedback_Rumble_Effect;
      end case;
   end record
     with Convention => C_Pass_By_Copy, Unchecked_Union;

   type Force_Feedback_Effect is record
      Kind      : Force_Feedback_Effect_Kind;
      ID        : aliased Force_Feedback_Effect_ID;
      Direction : Direction_Kind;

      Trigger   : Force_Feedback_Trigger;
      Replay    : Force_Feedback_Replay;

      Effect    : Force_Feedback_Effect_Union;
   end record
     with Convention => C_Pass_By_Copy;

   procedure Upload_Force_Feedback_Effect
     (Object : Input_Device;
      Effect : aliased in out Force_Feedbacks.Force_Feedback_Effect);

   procedure Remove_Force_Feedback_Effect
     (Object     : Input_Device;
      Identifier : Uploaded_Force_Feedback_Effect_ID);

private

   type Duration_MS is new Unsigned_16 range 0 .. Unsigned_16'Last / 2;

   function From_Duration (Value : Duration) return Duration_MS is
     (Duration_MS'Min (Duration_MS'Base (Value * 1e3), Duration_MS'Last));

   function Image (Value : Duration_MS) return String is (Value'Image & " ms");

   for Periodic_Wave_Kind use
     (Square   => 16#58#,
      Triangle => 16#59#,
      Sine     => 16#5A#,
      Saw_Up   => 16#5B#,
      Saw_Down => 16#5C#);
   for Periodic_Wave_Kind'Size use Unsigned_16'Size;

   for Direction_Kind use
     (Down  => 16#0000#,
      Left  => 16#4000#,
      Up    => 16#8000#,
      Right => 16#C000#);
   for Direction_Kind'Size use Unsigned_16'Size;

end Event_Device.Force_Feedbacks;
