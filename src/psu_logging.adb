pragma Profile (Ravenscar);

with PSU_Simulation; use PSU_Simulation;

package body PSU_Logging is

   procedure csv_put (File : in File_Type; Item : in Item_Type_t) is
   begin
      Put (File, Image (Item));
      Put (File, ", ");
   end csv_put;

   procedure csv_put_float is new csv_put (Item_Type_t => Float, Image => Float'Image);
   procedure csv_put_duration is new csv_put (Item_Type_t => Duration, Image => Duration'Image);
   procedure csv_put_signal_name is new csv_put (Item_Type_t => logged_signal_names_t, Image => logged_signal_names_t'Image);

   procedure csv_end_line (File : in File_Type; Item : in Item_Type_t) is
   begin
      Put_Line (File, Image (Item));
   end csv_end_line;

   procedure csv_end_line_float is new csv_end_line (Item_Type_t => Float, Image => Float'Image);
   procedure csv_end_line_signal_name is new csv_end_line (Item_Type_t => logged_signal_names_t, Image => logged_signal_names_t'Image);

   task body logging_task is
      START_TIME : constant Time := Clock;
      next_time : Time := Clock;

      output_file : File_Type;
   begin
      Create (File => output_file, Mode => Out_File, Name => "PSU.log");

      write_header (output_file);

      loop
         Put_Line ("Running logging task");

         write_current_data (output_file, To_Duration (next_time - START_TIME));

         next_time := next_time + TASK_PERIOD;
         delay until next_time;
      end loop;
   exception
      when others =>
         Put_Line (Standard_Error, "An exception occured");
   end logging_task;

   procedure write_header (File : in File_Type) is
   begin
      for signal_name in logged_signal_names_t range logged_signal_names_t'First .. logged_signal_names_t'Pred (logged_signal_names_t'Last) loop
         csv_put_signal_name (File, signal_name);
      end loop;

      csv_end_line_signal_name (File, logged_signal_names_t'Last);
   end write_header;

   procedure write_current_data (File : in File_Type; timestamp : in Duration) is
   begin
      csv_put_duration (File, timestamp);
      csv_put_float (File, Sim.Get_U_V1);
      csv_put_float (File, Sim.Get_I_L1);
      csv_put_float (File, Sim.Get_U_C1);
      csv_put_float (File, Sim.Get_I_L2);
      csv_put_float (File, Sim.Get_U_C2);
      csv_end_line_float (File, Sim.Get_I_Load);
   end write_current_data;

end PSU_Logging;