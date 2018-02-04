pragma Profile (Ravenscar);
--  with System.Dim.Mks - Would be nice to use here
with Ada.Numerics.Elementary_Functions;
with global_constants; use global_constants;
with Ada.Numerics;
with Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
package body PSU_Simulation is

   protected body Simulation_I_T is

      function Is_Ready return Boolean is
      begin
         return Conf_OK and Load_OK;
      end Is_Ready;

      function Get_Config return Sim_Config_T is
      begin
         return Conf;
      end Get_Config;

      function Get_U_V1 return Float is
      begin
         return Sim_Out.U_V1;
      end Get_U_V1;

      function Get_I_L1 return Float is
      begin
         return Sim_Out.I_L1;
      end Get_I_L1;

      function Get_U_C1 return Float is
      begin
         return Sim_Out.U_C1;
      end Get_U_C1;

      function Get_I_L2 return Float is
      begin
         return Sim_Out.I_L2;
      end Get_I_L2;

      function Get_U_C2 return Float is
      begin
         return Sim_Out.U_C2;
      end Get_U_C2;

      function Get_I_Load return Float is
      begin
         return Sim_Out.I_Load;
      end Get_I_Load;

      function Get_Sim_All return Sim_Output_T is
      begin
         return Sim_Out;
      end Get_Sim_All;

      function Get_D_M1 return Float is
      begin
         return D_M1;
      end Get_D_M1;

      function Get_D_M2_5 return Float is
      begin
         return D_M2_5;
      end Get_D_M2_5;

      function Get_Load return loadArray_T is
      begin
         return Loads;
      end Get_Load;

      function Get_Current_Load return Float is
      begin
         return current_load;
      end Get_Current_Load;

      function  Get_Start_Time return Ada.Real_Time.Time is
      begin
         return T_Start;
      end Get_Start_Time;

      procedure Set_Config (Val : in Sim_Config_T) is
      begin
         Conf    := Val;
         Conf_OK := True;
      end Set_Config;

      procedure Set_D_M1 (Val : in Float) is
      begin
         D_M1 := Val;
      end Set_D_M1;

      procedure Set_D_M2_5 (Val : in Float) is
      begin
         D_M2_5 := Val;
      end Set_D_M2_5;

      procedure Set_Sim_Out (Val : in Sim_Output_T) is
      begin
         Sim_Out := Val;
      end Set_Sim_Out;

      procedure Set_Load (Val : in loadArray_T) is
      begin
         Loads := Val;
         Load_OK := True;
      end Set_Load;

      procedure Set_Current_Load (Val : in Float) is
      begin
         current_load := Val;
      end Set_Current_Load;

      procedure Set_Start_Time (Val : Ada.Real_Time.Time) is
      begin
         T_Start := Val;
      end Set_Start_Time;

   end Simulation_I_T;

   function Get_Load_Actual (ST : Time; LO : loadArray_T) return Float is
      Run_Time : Time_Span;
   begin
      Run_Time := Clock - ST;
      for i in LO'Range (1) loop
         --  Return previous value if timestamp is not reached jet
         if ((RT_MUL_S2MS * LO (i, 1)) > Float (To_Duration (Run_Time))) then
            --  Return Inf if Start value not at time 0
            if (i - 1 in LO'Range (1)) then
               return LO (i - 1, 2);
            else
               return (1.0e12);
            end if;
         end if;
      end loop;
      --  Return Inf if no further load is specified
      return (1.0e12);
   end Get_Load_Actual;

   task body Simulation_Task_T is
      Angle      : Float := 0.0;
      Load       : Float := 100.0;
      Load_A     : loadArray_T;
      Start_Time : Time;
      Next_Time  : Time;
      Conf       : Sim_Config_T;
      Act, Prev  : Sim_Output_T;
   begin
      Next_Time  := Clock;
      --  Wait for configuration and load settings
      while (not Sim.Is_Ready)
      loop
         Next_Time := Next_Time + Milliseconds (200);
         delay until Next_Time;
      end loop;
      Ada.Text_IO.Put_Line ("Sim task active");
      --  Aquire settings (done once only)
      Conf       := Sim.Get_Config;
      Load_A     := Sim.Get_Load;
      Start_Time := Clock;
      Sim.Set_Start_Time (Start_Time);
      loop
         --  Get new new value for load and calculate the electrical terms
         Load := Get_Load_Actual (Start_Time, Load_A);
         Sim.Set_Current_Load (Load);
         Ada.Text_IO.Put_Line ("Load set to: " & Load'Image);
         Act.I_L1   := Prev.I_L1 + (Sim.Get_D_M1 * abs (Prev.U_V1) - (1.0 - Sim.Get_D_M1) * Prev.U_C1) * Conf.T / Conf.L1;
         Act.I_L2   := Prev.I_L2 + (Sim.Get_D_M2_5 * Prev.U_C1 - Prev.U_C2) * Conf.T / Conf.L2;
         Act.U_C2   := (Prev.I_L2 - Prev.I_Load) * Conf.T / Conf.C2;
         Act.U_C1   := (Prev.I_L1 - Sim.Get_D_M2_5 * Prev.I_L2) * Conf.T / Conf.C1;
         Act.I_Load := Prev.U_C2 / Load;
         --  Calculate mains voltage (Check if DC)
         if (Conf.f_V1 > 0.0001) then
            Angle    := Float'Remainder ((Angle + Conf.T * Conf.f_V1 * 2.0 * Ada.Numerics.Pi), (2.0 * Ada.Numerics.Pi));
            Act.U_V1 := Conf.Up_V1 * Ada.Numerics.Elementary_Functions.Sin (Angle);
         else
            Act.U_V1 := Conf.Up_V1;
         end if;
         --  Set the output
         Ada.Text_IO.Put_Line ("V1: " & Act.U_V1'Image & " UC1: " & Act.U_C1'Image & " UC2: " & Act.U_C2'Image);
         Prev      := Act;
         Sim.Set_Sim_Out (Act);
         Next_Time := Next_Time + Microseconds (Integer (RT_MUL_S2US * Conf.T));
         delay until Next_Time;
      end loop;
   end Simulation_Task_T;
   Simuation_Task : Simulation_Task_T;

end PSU_Simulation;
