with Event_Device.Input_Dev;

package body Event_Device.Force_Feedbacks is

   use all type Event_Device.Input_Dev.Access_Mode;
   use type Event_Device.Input_Dev.Unsigned_14;

   procedure Upload_Force_Feedback_Effect
     (Object : Input_Device;
      Effect : aliased in out Force_Feedbacks.Force_Feedback_Effect)
   is
      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Write, 'E', 16#80#,
                     Effect'Size / System.Storage_Unit),
         Effect'Address);
   begin
      pragma Assert (Error_Code /= -1);
   end Upload_Force_Feedback_Effect;

   procedure Remove_Force_Feedback_Effect
     (Object     : Input_Device;
      Identifier : Uploaded_Force_Feedback_Effect_ID)
   is
      ID : aliased constant Integer := Integer (Identifier);

      Error_Code : constant Integer := Event_Device.Input_Dev.IO_Control
        (Object.FD, (Write, 'E', 16#81#, ID'Size / System.Storage_Unit),
         ID);
   begin
      pragma Assert (Error_Code /= -1);
   end Remove_Force_Feedback_Effect;

end Event_Device.Force_Feedbacks;
