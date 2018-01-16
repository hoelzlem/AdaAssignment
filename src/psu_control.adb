pragma Profile (Ravenscar);
with PSU_Simulation; use PSU_Simulation;
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
      C.E1 := C.E;
      C.E := W - Y;
      C.P := C.E * C.Conf.Kp;
      C.I := C.I + (C.E * C.Conf.Kp * C.Conf.Ki + C.Sat) * C.Conf.T;
      C.D := C.Conf.Kp * (C.E + (C.E - C.E1) * C.Conf.Kd / C.Conf.T);
      C.Y := C.P + C.I + C.D;
      if (C.Y < C.Conf.Sn) then
         C.Sat := C.Conf.Sn - C.Y;
      elsif (C.Y > C.Conf.Sp) then
         C.Sat := C.Conf.Sp - C.Y;
      else
         C.Sat := 0.0;
      end if;
      return C.Y + C.Sat;
   end calculate_U;

   procedure reset (C : in out PID_Controller_T) is
   begin
      C.E   := 0.0;
      C.E1  := 0.0;
      C.I   := 0.0;
      C.Sat := 0.0;
   end reset;

   task body Control_Task_T is
      Conf           : PID_Config_A_T;
      Next_Time      : Time := Clock;
      D_M1, D_M2_5   : Float := 0.0;
      I_L1, I_L2     : Float := 0.0;
      Controllers    : PID_Controller_A_T;
   begin
      Next_Time := Clock;
      while (not Sim.Is_Ready or not Ctrl.Is_Ready)
      loop
         Next_Time := Next_Time + Milliseconds (200);
         delay until Next_Time;
      end loop;
      Conf := Ctrl.Get_Config;
      --  Write Config to Controllers could be done with access type
      for id in PID_Controller_A_T'Range loop
         Controllers (id).Conf := Conf (id);
      end loop;

      loop
         Ada.Text_IO.Put_Line ("Control task active");
         --  Run the controllers

         --  Set dutycycle for Simulation
         if (Ctrl.Get_Safety_State) then
            I_L1 := calculate_U (C => Controllers (PID_U_C1),
                                 W => Ctrl.Get_W_U_C1,
                                 Y => Sim.Get_U_C1);
            I_L2 := calculate_U (C => Controllers (PID_U_C2),
                                 W => Ctrl.Get_W_U_C2,
                                 Y => Sim.Get_U_C2);
            D_M1 := calculate_U (C => Controllers (PID_I_L1),
                                 W => I_L1,
                                 Y => Sim.Get_I_L1);
            D_M2_5 := calculate_U (C => Controllers (PID_I_L2),
                                   W => I_L2,
                                   Y => Sim.Get_I_L2);
            Sim.Set_D_M1 (D_M1);
            Sim.Set_D_M2_5 (D_M2_5);
         else
            reset (C => Controllers (PID_U_C1));
            reset (C => Controllers (PID_U_C2));
            reset (C => Controllers (PID_I_L1));
            reset (C => Controllers (PID_I_L2));
            Sim.Set_D_M1 (0.0);
            Sim.Set_D_M2_5 (0.0);
         end if;

         Next_Time := Next_Time +
            Milliseconds (Integer (Conf (PID_U_C1).T * 1.0e6));
         delay until Next_Time;
      end loop;
   end Control_Task_T;
   Control_Task : Control_Task_T;

end PSU_Control;
