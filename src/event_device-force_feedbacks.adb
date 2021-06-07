--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2021 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with Event_Device.Input_Dev;

package body Event_Device.Force_Feedbacks is

   use all type Event_Device.Input_Dev.Access_Mode;
   use type Event_Device.Input_Dev.Unsigned_14;

   procedure Upload_Force_Feedback_Effect
     (Object : Input_Device;
      Effect : in out Force_Feedbacks.Force_Feedback_Effect)
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
