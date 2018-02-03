pragma Profile (Ravenscar);

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with System;

--  @TODO add logging of minimum and maximum values of a signal during two logging samples
--  @TODO add current load to logged signals

package PSU_Logging is

   type File_CHandle is access constant File_Type;

   type logged_signal_names_t is (
      t,
      U_V1,
      I_L1,
      U_C1,
      I_L2,
      U_C2,
      I_Load,
      R_Load
   );

   protected type Logger_Interface_T is
      function is_all_config_set return Boolean;

      procedure set_logfile (new_logfile : in File_CHandle);
      function get_logfile return File_CHandle;
   private
      logfile : File_CHandle;
      logfile_is_set : Boolean := False;
   end Logger_Interface_T;

   logger_interface : Logger_Interface_T;

private
   TASK_PERIOD : constant Time_Span := Milliseconds (100);

   task logging_task is
      pragma Priority (System.Priority'First);
   end logging_task;

   generic
      type Item_Type_t is private;
      with function Image (Item : Item_Type_t) return String;
      procedure csv_put (File : in File_CHandle; Item : in Item_Type_t);

   generic
      type Item_Type_t is private;
      with function Image (Item : Item_Type_t) return String;
      procedure csv_end_line (File : in File_CHandle; Item : in Item_Type_t);

   procedure write_header (File : in File_CHandle);
   procedure write_current_data (File : in File_CHandle; timestamp : in Duration);

end PSU_Logging;
