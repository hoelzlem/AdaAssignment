pragma Profile (Ravenscar);

with PSU_Simulation; use PSU_Simulation;

package body PSU_Logging is

   task body logging_task is
      START_TIME : constant Time := Clock;
      next_time : Time := Clock;

      output_file : File_Type;
   begin
      Create (File => output_file, Mode => Out_File, Name => "PSU.log");

      loop
         Put_Line ("Running logging task");
         --  @TODO write a csv header that names the columns
         --  write all values from the simulation interface into the log file
         --  Write timestamp
         csv_put (output_file, Duration'Image (To_Duration (next_time - START_TIME)));
         csv_put_line (output_file, "end");

         next_time := next_time + TASK_PERIOD;
         delay until next_time;
      end loop;
   exception
      when others =>
         Put_Line (Standard_Error, "An exception occured");
   end logging_task;

   procedure csv_put (File : in File_Type; Item : in String) is
   begin
      Put (File, Item);
      Put (File, ", ");
   end csv_put;

   procedure csv_put_line (File : in File_Type; Item : in String) is
   begin
      Put_Line (File, Item);
   end csv_put_line;

end PSU_Logging;
