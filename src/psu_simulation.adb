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

      function Get_Load_A return loadArray_T is
      begin
         return Load_A;
      end Get_Load_A;

      function Get_Load return Float is
      begin
         return Load;
      end Get_Load;

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

      procedure Set_Load_A (Val : in loadArray_T) is
      begin
         Load_A := Val;
         Load_OK := True;
      end Set_Load_A;

      procedure Set_Load (Val : in Float) is
      begin
         Load := Val;
      end Set_Load;

      procedure Set_Start_Time (Val : Ada.Real_Time.Time) is
      begin
         T_Start := Val;
      end Set_Start_Time;

   end Simulation_I_T;

   function Get_Load_Actual (ST : Time; LO : loadArray_T) return Float is
      Run_Time : Time_Span;
   begin
      Run_Time := Clock - ST;
      --  This is highly inefficient for large arrays - but we do not care in this case
      for i in LO'Range (1) loop
         --  Return previous value if timestamp is not reached jet
         if ((RT_MUL_S2MS * LO (i, 1)) > Float (To_Duration (Run_Time))) then
            --  Return 1 GOhm if Start value not at time 0
            --  This leads to open circuit if no load is specified
            if (i - 1 in LO'Range (1)) then
               return LO (i - 1, 2);
            else
               return (1.0e9);
            end if;
         end if;
      end loop;
      --  Return 1 GOhm if no further load is specified
      --  There is no load connected after last specified timestamp
      return (1.0e9);
   end Get_Load_Actual;

   task body Simulation_Task_T is
      Angle      : Float := 0.0;
      Load       : Float := 100.0;
      Load_A     : loadArray_T;
      Start_Time : Time;
      Next_Time  : Time;
      Conf       : Sim_Config_T;
      Act, Prev  : Sim_Output_T;
      Period     : Time_Span := Milliseconds (100);
   begin
      Next_Time  := Clock;
      --  Wait for configuration and load settings
      while (not Sim.Is_Ready)
      loop
         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
      --  Aquire settings (done once only)
      Conf       := Sim.Get_Config;
      Load_A     := Sim.Get_Load_A;
      Period     := Microseconds (Integer (RT_MUL_S2US * Conf.T));
      Start_Time := Clock;
      Sim.Set_Start_Time (Start_Time);
      loop
         Ada.Text_IO.Put_Line (vt100_GREEN & "Running simulation task" & vt100_RESET);
         --  Get new new value for load and calculate the electrical terms
         Load := Get_Load_Actual (Start_Time, Load_A);
         Sim.Set_Load (Load);
         Ada.Text_IO.Put ("Sim task -> Load: " & Load'Image);
         --  Calculate the electrical terms (see schematic of the circuit)
         Act.I_L1   := Prev.I_L1 + (Sim.Get_D_M1 * abs (Prev.U_V1) - (1.0 - Sim.Get_D_M1) * Prev.U_C1) * Conf.T / Conf.L1;
         Act.I_L2   := Prev.I_L2 + (Sim.Get_D_M2_5 * Prev.U_C1 - Prev.U_C2) * Conf.T / Conf.L2;
         Act.U_C2   := (Prev.I_L2 - Prev.I_Load) * Conf.T / Conf.C2;
         Act.U_C1   := (Prev.I_L1 - Sim.Get_D_M2_5 * Prev.I_L2) * Conf.T / Conf.C1;
         Act.I_Load := Prev.U_C2 / Load;
         --  Calculate the next value of the mains voltage
         if (Conf.f_V1 > 0.0001) then
            Angle    := Float'Remainder ((Angle + Conf.T * Conf.f_V1 * 2.0 * Ada.Numerics.Pi), (2.0 * Ada.Numerics.Pi));
            Act.U_V1 := Conf.Up_V1 * Ada.Numerics.Elementary_Functions.Sin (Angle);
         else
            --  If running on DC voltage
            Act.U_V1 := Conf.Up_V1;
         end if;
         --  Set the outputs
         Ada.Text_IO.Put_Line (" V1: " & Act.U_V1'Image & " UC1: " & Act.U_C1'Image & " UC2: " & Act.U_C2'Image);
         Prev      := Act;
         Sim.Set_Sim_Out (Act);
         --  Calculate next time the thread should run and delay until said time
         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
   end Simulation_Task_T;
   Simuation_Task : Simulation_Task_T;

end PSU_Simulation;
