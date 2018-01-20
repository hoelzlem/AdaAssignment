pragma Profile (Ravenscar);

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with System;

package PSU_Logging is

private
   TASK_PERIOD : constant Time_Span := Milliseconds (100);

   task logging_task is
      pragma Priority (System.Priority'Last);
   end logging_task;

   procedure csv_put (File : in File_Type; Item : in String);
   procedure csv_put_line (File : in File_Type; Item : in String);

end PSU_Logging;