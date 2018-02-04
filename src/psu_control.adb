pragma Profile (Ravenscar);
with PSU_Simulation; use PSU_Simulation;
with global_constants; use global_constants;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;
package body PSU_Control is

   protected body Control_I_T is

      function Is_Ready return Boolean is
      begin
         return Conf_ALL_OK;
      end Is_Ready;

      function Get_Config return PID_Config_A_T is
      begin
         return Conf;
      end Get_Config;

      function Get_Config (Id : in PID_Target_T) return PID_Config_T is
      begin
         return Conf (Id);
      end Get_Config;

      function Get_W_U_C1 return Float is
      begin
         return W_U_C1;
      end Get_W_U_C1;

      function Get_W_U_C2 return Float is
      begin
         return W_U_C2;
      end Get_W_U_C2;

      function Get_Safety_State return Boolean is
      begin
         return Safety_State;
      end Get_Safety_State;

      procedure Set_Config (Val : in PID_Config_A_T) is
      begin
         Conf := Val;
         Conf_ALL_OK := True;
         Conf_State := (others => True);
      end Set_Config;

      procedure Set_Config (Val : PID_Config_T; Id : PID_Target_T) is
         status : Boolean := True;
      begin
         Conf (Id) := Val;
         Conf_State (Id) := True;
         --  And reduce would certainly be nicer here
         for id in Conf_State'Range loop
            status := status and Conf_State (id);
         end loop;
         Conf_ALL_OK := status;
      end Set_Config;

      procedure Set_W_U_C1 (Val : in Float) is
      begin
         W_U_C1 := Val;
      end Set_W_U_C1;

      procedure Set_W_U_C2 (Val : in Float) is
      begin
         W_U_C2 := Val;
      end Set_W_U_C2;

      procedure Set_Safety_State (Val : in Boolean) is
      begin
         Safety_State := Val;
      end Set_Safety_State;

   end Control_I_T;

   function calculate_U
      (C : in out PID_Controller_T; W : in Float; Y : in Float)
       return Float is
   begin
      --  Standard implemetation of PID controller
      C.E1 := C.E;
      C.E := W - Y;
      C.P := C.E * C.Conf.Kp;
      --  Intergrator with anti windup through back-calculation (fixed gain here)
      C.I := C.I + (C.E * C.Conf.Kp * C.Conf.Ki + C.Sat) * C.Conf.T;
      C.D := C.Conf.Kp * (C.E + (C.E - C.E1) * C.Conf.Kd / C.Conf.T);
      C.U := C.P + C.I + C.D;
      --  Saturation
      if (C.U < C.Conf.Sn) then
         C.Sat := C.Conf.Sn - C.U;
      elsif (C.U > C.Conf.Sp) then
         C.Sat := C.Conf.Sp - C.U;
      else
         C.Sat := 0.0;
      end if;
      return C.U + C.Sat;
   end calculate_U;

   function Do_Filtering (F : in out IIR_Filter_T; I : in Float) return Float is
      z0     : Float;
      result : Float;
   begin
      --  Standard implementation of discrete direct form II filter
      z0 := F.gi * I - F.z1 * F.d1 - F.z2 * F.d2;
      result := z0 * F.n0 + F.z1 * F.n1 + F.z2 * F.n2;
      F.z2 := F.z1;
      F.z1 := z0;
      return result * F.go;
   end Do_Filtering;

   procedure Set_Config (F : in out IIR_Filter_T; gi, go, d1, d2, n0, n1, n2 : in Float) is
   begin
      F.gi := gi;
      F.go := go;
      F.d1 := d1;
      F.d2 := d2;
      F.n0 := n0;
      F.n1 := n1;
      F.n2 := n2;
   end Set_Config;

   procedure Reset (C : in out PID_Controller_T) is
   begin
      C.E   := 0.0;
      C.E1  := 0.0;
      C.I   := 0.0;
      C.Sat := 0.0;
   end Reset;

   task body Control_Task_T is
      Conf             : PID_Config_A_T;
      Next_Time        : Time := Clock;
      D_M1, D_M2_5     : Float := 0.0;
      I_L1, I_L2       : Float := 0.0;
      U_V1_p           : Float := 1.0;
      Controllers      : PID_Controller_A_T;
      Filter           : IIR_Filter_T;
      Period           : Time_Span := Milliseconds (100);
   begin
      Next_Time := Clock;
      while (not Sim.Is_Ready or not Ctrl.Is_Ready)
      loop
         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
      Conf := Ctrl.Get_Config;
      --  Write Config to Controllers could be done with access type
      for id in PID_Controller_A_T'Range loop
         Controllers (id).Conf := Conf (id);
      end loop;
      --  Theoretically controllers could run on different frequencies but only one is used here
      --  If one would attempt to do that multiple tasks could be used for that
      Period := Milliseconds (Integer (Conf (PID_I_L2).T * RT_MUL_S2MS));
      --  Hard coded configuration of the filter used to track the peak value of V1
      --  This filter is designed as lowpass with -60db at 50Hz
      Set_Config (F  => Filter,
                  gi => 0.000000044710606114963880230393891744914,
                  go => 0.707945784384137910549839034501928836107,
                  d1 => -1.999675711776111031170444221061188727617,
                  d2 => 0.999675890618535456511040138138923794031,
                  n0 => 1.0,
                  n1 => 2.0,
                  n2 => 1.0);
      loop
         --  Filter the Voltage V1 to get rectified value and multiply with Crest factor
         U_V1_p := Do_Filtering (Filter, abs Sim.Get_U_V1) * 1.414;
         if (Ctrl.Get_Safety_State) then
            --  Run the contorllers for calculate desired currents and dutycycles
            I_L1 := calculate_U (C => Controllers (PID_U_C1),
                                 W => Ctrl.Get_W_U_C1,
                                 Y => Sim.Get_U_C1);
            --  Scale disired current to achieve pfc
            I_L1 := I_L1 * Sim.Get_U_V1 / U_V1_p;
            I_L2 := calculate_U (C => Controllers (PID_U_C2),
                                 W => Ctrl.Get_W_U_C2,
                                 Y => Sim.Get_U_C2);
            D_M1 := calculate_U (C => Controllers (PID_I_L1),
                                 W => I_L1,
                                 Y => Sim.Get_I_L1);
            D_M2_5 := calculate_U (C => Controllers (PID_I_L2),
                                   W => I_L2,
                                   Y => Sim.Get_I_L2);
            --  Set the dutycyles of the simulation to the calculated values
            Sim.Set_D_M1 (D_M1);
            Sim.Set_D_M2_5 (D_M2_5);
         else
            --  Reset all controllers if system is shut down via safety input
            Reset (C => Controllers (PID_U_C1));
            Reset (C => Controllers (PID_U_C2));
            Reset (C => Controllers (PID_I_L1));
            Reset (C => Controllers (PID_I_L2));
            --  Set the dutycycles of all transistors to 0.0 during shut down
            Sim.Set_D_M1 (0.0);
            Sim.Set_D_M2_5 (0.0);
         end if;
         --  Print the values assigned to the dutycycles of M1 and M2 to M5
         Ada.Text_IO.Put_Line (vt100_CYAN & "Running control task " & vt100_RESET & "-> D_M1: " & D_M1'Image & " D_M2_5: " & D_M2_5'Image);
         --  Calculate next time the thread should run and delay until said time
         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
   end Control_Task_T;
   Control_Task : Control_Task_T;

end PSU_Control;
