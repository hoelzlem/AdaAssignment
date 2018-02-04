pragma Profile (Ravenscar);

with global_constants; use global_constants;
with PSU_Simulation; use PSU_Simulation;

package body PSU_Logging is

   protected body Logger_Interface_T is

      function is_all_config_set return Boolean is
      begin
         return logfile_is_set;
      end is_all_config_set;

      procedure set_logfile (new_logfile : in File_CHandle) is
      begin
         logfile := new_logfile;
         logfile_is_set := True;
      end set_logfile;

      function get_logfile return File_CHandle is
      begin
         return logfile;
      end get_logfile;

   end Logger_Interface_T;

   procedure csv_put (File : in File_CHandle; Item : in Item_Type_t) is
   begin
      Put (File.all, Image (Item));
      Put (File.all, ", ");
   end csv_put;

   procedure csv_put_float is new csv_put (Item_Type_t => Float, Image => Float'Image);
   procedure csv_put_duration is new csv_put (Item_Type_t => Duration, Image => Duration'Image);
   procedure csv_put_signal_name is new csv_put (Item_Type_t => logged_signal_names_t, Image => logged_signal_names_t'Image);

   procedure csv_end_line (File : in File_CHandle; Item : in Item_Type_t) is
   begin
      Put_Line (File.all, Image (Item));
   end csv_end_line;

   procedure csv_end_line_float is new csv_end_line (Item_Type_t => Float, Image => Float'Image);
   procedure csv_end_line_signal_name is new csv_end_line (Item_Type_t => logged_signal_names_t, Image => logged_signal_names_t'Image);

   task body logging_task is
      start_time : constant Time := Clock;
      next_time : Time := start_time;
   begin
      while not logger_interface.is_all_config_set loop
         next_time := next_time + TASK_PERIOD;
         delay until next_time;
      end loop;

      write_header (logger_interface.get_logfile);

      loop
         Put_Line (vt100_YELLOW & "Running logging task" & vt100_RESET);
         write_current_data (logger_interface.get_logfile, To_Duration (next_time - start_time));

         next_time := next_time + TASK_PERIOD;
         delay until next_time;
      end loop;
   end logging_task;

   procedure write_header (File : in File_CHandle) is
   begin
      for signal_name in logged_signal_names_t range logged_signal_names_t'First .. logged_signal_names_t'Pred (logged_signal_names_t'Last) loop
         csv_put_signal_name (File, signal_name);
      end loop;

      csv_end_line_signal_name (File, logged_signal_names_t'Last);
   end write_header;

   procedure write_current_data (File : in File_CHandle; timestamp : in Duration) is
   begin
      csv_put_duration (File, timestamp);
      csv_put_float (File, Sim.Get_U_V1);
      csv_put_float (File, Sim.Get_I_L1);
      csv_put_float (File, Sim.Get_U_C1);
      csv_put_float (File, Sim.Get_I_L2);
      csv_put_float (File, Sim.Get_U_C2);
      csv_put_float (File, Sim.Get_I_Load);
      csv_end_line_float (File, Sim.Get_Load);
   end write_current_data;

end PSU_Logging;
