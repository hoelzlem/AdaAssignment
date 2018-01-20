pragma Profile (Ravenscar);

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with System;

package PSU_Logging is

   type logged_signal_names_t is (
      t,
      U_V1,
      I_L1,
      U_C1,
      I_L2,
      U_C2,
      I_Load
   );

private
   TASK_PERIOD : constant Time_Span := Milliseconds (100);

   task logging_task is
      pragma Priority (System.Priority'Last);
   end logging_task;

   generic
      type Item_Type_t is private;
      procedure csv_put (File : in File_Type; Item : in Item_Type_t);

   procedure write_header (File : in File_Type);
   procedure write_current_data (File : in File_Type; timestamp : in Duration);

end PSU_Logging;