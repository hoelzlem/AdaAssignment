pragma Profile (Ravenscar);
package PSU_Simulation is

   type Sim_Config_T is record
      L1     : Float := 1.0e-3;
      C1     : Float := 1.0e-3;
      L2     : Float := 1.0e-3;
      C2     : Float := 1.0e-3;
      f_V1   : Float := 1.0e-3;
      Up_V1  : Float := 200.0;
      T      : Float := 1.0;
   end record;

   type Sim_Output_T is record
      U_C2   : Float := 0.0;
      I_Load : Float := 0.0;
      I_L1   : Float := 0.0;
      I_L2   : Float := 0.0;
      U_C1   : Float := 0.0;
      U_V1   : Float := 0.0;
   end record;

   protected type Simulation_I_T is
      function  Is_Ready return Boolean;
      function  Get_Config return Sim_Config_T;
      function  Get_U_V1 return Float;
      function  Get_I_L1 return Float;
      function  Get_U_C1 return Float;
      function  Get_I_L2 return Float;
      function  Get_U_C2 return Float;
      function  Get_I_Load return Float;
      function  Get_Sim_All return Sim_Output_T;
      function  Get_D_M1 return Float;
      function  Get_D_M2_5 return Float;
      procedure Set_Config (Val : in Sim_Config_T);
      procedure Set_D (M1, M2_5 : in Float);
      procedure Set_D_M1 (Val : in Float);
      procedure Set_D_M2_5 (Val : in Float);
      procedure Set_Sim_Out (Val : in Sim_Output_T);
   private
      Sim_Out   : Sim_Output_T;
      D_M2_5    : Float := 0.0;
      D_M1      : Float := 0.0;
      Conf      : Sim_Config_T;
      Conf_OK   : Boolean := False;
   end Simulation_I_T;

   Sim : Simulation_I_T;

private

   task type Simulation_Task_T is
   end Simulation_Task_T;

end PSU_Simulation;
