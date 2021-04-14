package Event_Device.Force_Feedbacks is

   type Duration_MS is private;

   function In_Milliseconds (Value : Duration) return Duration_MS;

   type Force_Feedback_Replay is record
      Length      : Duration_MS;
      Start_Delay : Duration_MS;
   end record;

   type Force_Feedback_Trigger is record
      Button   : unsigned_short;
      Interval : Duration_MS;
   end record;

   type Force_Feedback_Envelope is record
      Attack_Length : Duration_MS;
      Attack_Level  : unsigned_short;
      Fade_Length   : Duration_MS;
      Fade_Level    : unsigned_short;
   end record;

   type Force_Feedback_Constant_Effect is record
      Level    : short;
      Envelope : Force_Feedback_Envelope;
   end record;

   type Force_Feedback_Ramp_Effect is record
      Start_Level : short;
      End_Level   : short;
      Envelope    : Force_Feedback_Envelope;
   end record;

   type Force_Feedback_Condition_Effect is record
      Right_Saturation : unsigned_short;
      Left_Saturation  : unsigned_short;
      Right_Coeff      : short;
      Left_Coeff       : short;
      Deadband         : unsigned_short;
      Center           : short;
   end record;

   type Periodic_Wave_Kind is (Square, Triangle, Sine, Saw_Up, Saw_Down);
   --  Custom is not supported

   type Force_Feedback_Periodic_Effect is record
      Waveform    : Periodic_Wave_Kind;
      Period      : Duration_MS;
      Magnitude   : short;
      Offset      : short;
      Phase       : unsigned_short;
      Envelope    : Force_Feedback_Envelope;
      Custom_Len  : unsigned       := 0;
      Custom_Data : System.Address := System.Null_Address;
   end record;

   type Force_Feedback_Rumble_Effect is record
      Strong_Magnitude : unsigned_short;
      Weak_Magnitude   : unsigned_short;
   end record;

   type Force_Feedback_Effect_Kind is
     (Constant_V, Periodict, Ramp, Spring, Friction, Damper, Rumble, Inertia);

   type Direction_Type is (Down, Left, Up, Right);

   type Axis_Direction is (X, Y);

   type Condition_Effect_Array is array (Axis_Direction) of Force_Feedback_Condition_Effect;

   type Force_Feedback_Effect (Kind : Force_Feedback_Effect_Kind) is record
      ID        : Force_Feedback_Effect_ID;
      Direction : Direction_Type;

      Trigger   : Force_Feedback_Trigger;
      Replay    : Force_Feedback_Replay;

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
     with Convention => C_Pass_By_Copy;

private

   type Duration_MS is new Unsigned_16;

   function In_Milliseconds (Value : Duration) return Duration_MS is
     (Duration_MS (Value * 1e3));

   for Periodic_Wave_Kind use
     (Square   => 16#58#,
      Triangle => 16#59#,
      Sine     => 16#5A#,
      Saw_Up   => 16#5B#,
      Saw_Down => 16#5C#,
      Custom   => 16#5D#);
   for Periodic_Wave_Kind'Size use Unsigned_16'Size;

   for Force_Feedback_Effect_Kind use
     (Rumble     => 16#50#,
      Periodict  => 16#51#,
      Constant_V => 16#52#,
      Spring     => 16#53#,
      Friction   => 16#54#,
      Damper     => 16#55#,
      Inertia    => 16#56#,
      Ramp       => 16#57#);
   for Force_Feedback_Effect_Kind'Size use Unsigned_16'Size;

   for Direction_Type use
     (Down  => 16#0000#,
      Left  => 16#4000#,
      Up    => 16#8000#,
      Right => 16#C000#);
   for Direction_Type'Size use Unsigned_16'Size;

end Event_Device.Force_Feedbacks;
