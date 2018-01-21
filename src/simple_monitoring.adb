pragma Profile (Ravenscar);
pragma SPARK_Mode;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body simple_monitoring is

   function sign (val : in Float) return Float is
      res : Float;
   begin
      pragma Assert (val >= 10.0);
      res := 100.0 / val;

      return res;

   end sign;

   task body myTask is
      TASK_PERIOD : constant Time_Span := Milliseconds (100);
      next_time   : Time := Clock;
      myVar       : Float;
   begin
      loop
         Put_Line ("Running task");
         myVar := sign (20.0);
         Put_Line (myVar'Image);

         next_time := next_time + TASK_PERIOD;
         delay until next_time;
      end loop;
   end myTask;

end simple_monitoring;
