package body Event_Device.Input_Dev is

   subtype Size_Type is Interfaces.C.size_t;

   type Signed_Size_Type is range -(2 ** (Size_Type'Size - 1)) ..
                                  +(2 ** (Size_Type'Size - 1) - 1);

   procedure Write
     (FD    : Integer;
      Event : Input_Dev.Input_Event)
   is
      function C_Write
        (File_Descriptor : Interfaces.C.int;
         Buffer          : Input_Dev.Input_Event;
         Count           : Size_Type) return Signed_Size_Type
      with Import, Convention => C, External_Name => "write";

      Error_Code : Signed_Size_Type;
   begin
      Error_Code :=
        C_Write
          (File_Descriptor => Interfaces.C.int (FD),
           Buffer          => Event,
           Count           => Event'Size / System.Storage_Unit);
      pragma Assert (Error_Code /= -1);
   end Write;

end Event_Device.Input_Dev;
