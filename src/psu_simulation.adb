pragma Profile (Ravenscar);
--  with System.Dim.Mks - Would be nice to use here
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics;
with Ada.Text_IO;
package body PSU_Simulation is

   protected body Simulation_I_T is

      function Is_Ready return Boolean is
      begin
         return Conf_OK;
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

      procedure Set_Config (Val : in Sim_Config_T) is
      begin
         Conf    := Val;
         Conf_OK := True;
      end Set_Config;

      procedure Set_D (M1, M2_5 : in Float) is
      begin
         D_M1 := M1;
         D_M2_5 := M2_5;
      end Set_D;

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

   end Simulation_I_T;

   task body Simulation_Task_T is
      Angle      : Float := 0.0;
      Load       : Float := 100.0;
      Next_Time  : Time := Clock;
      Conf       : Sim_Config_T;
      Act, Prev  : Sim_Output_T;
   begin
      Next_Time := Clock;
      while (not Sim.Is_Ready)
      loop
         Next_Time := Next_Time + Milliseconds (200);
         delay until Next_Time;
      end loop;
      Conf := Sim.Get_Config;
      loop
         Act.I_L1   := Prev.I_L1 +
            (Sim.Get_D_M1 * abs (Prev.U_V1) -
             (1.0 - Sim.Get_D_M1) * Prev.U_C1) * Conf.T / Conf.L1;
         Act.I_Load := Prev.U_C2 / Load;
         Act.I_L2   := Prev.I_L2 +
            (Sim.Get_D_M2_5 * Prev.U_C1 - Prev.U_C2) * Conf.T / Conf.L2;
         Act.U_C2   := (Prev.I_L2 - Prev.I_Load) * Conf.T / Conf.C2;
         Act.U_C1   := (Prev.I_L1 - Sim.Get_D_M2_5 * Prev.I_L2)
            * Conf.T / Conf.C1;
         if (Conf.f_V1 > 0.0001) then
            Angle    := Float'Remainder
               ((Angle + Conf.T * Conf.f_V1 * 2.0 * Ada.Numerics.Pi),
                (2.0 * Ada.Numerics.Pi));
            Act.U_V1 := Conf.Up_V1 *
               Ada.Numerics.Elementary_Functions.Sin (Angle);
         else
            Act.U_V1 := Conf.Up_V1;
         end if;
         Sim.Set_Sim_Out (Act);
         Ada.Text_IO.Put (Act.U_V1'Image & "  " &
                             Act.U_C1'Image & "  " & Act.U_C2'Image);
         Ada.Text_IO.Put_Line ("");
         Prev       := Act;
         Next_Time := Next_Time + Milliseconds (Integer (1.0e6 * Conf.T));
         delay until Next_Time;
      end loop;
   end Simulation_Task_T;
   Simuation_Task : Simulation_Task_T;

end PSU_Simulation;
