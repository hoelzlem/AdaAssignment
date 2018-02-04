pragma Profile (Ravenscar);
pragma SPARK_Mode;

--  See https://goo.gl/QQ8HAA
pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
package PSU_Simulation is

   subtype Float_Signed1000 is Float range -1.0e3 .. 1.0e3;

   type Sim_Config_T is record
      L1     : Float := 1.0e-3;  --  Inductance of L1
      C1     : Float := 1.0e-3;  --  Capacity of C1
      L2     : Float := 1.0e-3;  --  Inductance of L2
      C2     : Float := 1.0e-3;  --  Capacity of C2
      f_V1   : Float := 0.0;     --  Frequency of V1
      Up_V1  : Float := 1.0e+2;  --  Peak Voltage of V1
      T      : Float := 1.0e-3;  --  Time for simulation step
   end record;

   type Sim_Output_T is record
      I_L1   : Float := 0.0;     --  Current through L1
      I_L2   : Float := 0.0;     --  Current through L2
      I_Load : Float := 0.0;     --  Current through Load
      U_C1   : Float := 0.0;     --  Voltage over C1
      U_C2   : Float := 0.0;     --  Voltage over C2
      U_V1   : Float := 0.0;     --  Voltage of V1
   end record;

   maxLoadValues : constant Integer := 10; -- Constant max number of load values
   type loadArray_T is array (Integer range 1 .. maxLoadValues, Integer range 1 .. 2) of Float;


   protected type Simulation_I_T is
      function  Is_Ready return Boolean;                --  Synchronize threads at startup (returns true when everything is configured)
      function  Get_Config return Sim_Config_T;         --  Get the hardware configuration of this simulation
      --  See https://goo.gl/nJKP8B
      function  Get_I_L1 return Float
         with Annotate => (GNATprove, Terminating),
         Post => Get_I_L1'Result in Float_Signed1000;     --  Get the current through L1
      --  See https://goo.gl/nJKP8B
      function  Get_I_L2 return Float
         with Annotate => (GNATprove, Terminating),
         Post => Get_I_L2'Result in Float_Signed1000;     --  Get the current through L2
      function  Get_I_Load return Float;                --  Get the current through Load
      --  See https://goo.gl/nJKP8B
      function  Get_U_C1 return Float
         with Annotate => (GNATprove, Terminating),
         Post => Get_U_C1'Result in Float_Signed1000;     --  Get the voltage over C1
      --  See https://goo.gl/nJKP8B
      function  Get_U_C2 return Float
         with Annotate => (GNATprove, Terminating),
         Post => Get_U_C2'Result in Float_Signed1000;     --  Get the voltage over C2
      function  Get_U_V1 return Float;                  --  Get the voltage of V1
      function  Get_Sim_All return Sim_Output_T;        --  Get all output values
      function  Get_D_M1 return Float;                  --  Get the dutycycle for M1
      function  Get_D_M2_5 return Float;                --  Get the dutycycle for M2 to M5
      function  Get_Load return loadArray_T;            --  Get the load configuration
      function  Get_Current_Load return Float;          --  Get the current load resistance (for logging)
      function  Get_Start_Time return Ada.Real_Time.Time;
      procedure Set_Config (Val : in Sim_Config_T);     --  Set the hardware configuration of this simulation
      procedure Set_D_M1 (Val : in Float);              --  Set the dutycycle for M1
      procedure Set_D_M2_5 (Val : in Float);            --  Set the dutycycle for M2 to M5
      procedure Set_Sim_Out (Val : in Sim_Output_T);    --  Set all output values
      procedure Set_Load (Val : in loadArray_T);        --  Set the load configuration
      procedure Set_Current_Load (Val : in Float);      --  Set the current load resistance (for logging)
      procedure Set_Start_Time (Val : Ada.Real_Time.Time);

   private
      Sim_Out   : Sim_Output_T;                                     --  Buffer
      D_M2_5    : Float := 0.0;                                     --  Buffer
      D_M1      : Float := 0.0;                                     --  Buffer
      Conf      : Sim_Config_T;                                     --  Buffer
      Loads     : loadArray_T := (others => (others => 0.0));
      current_load : Float := 0.0;                                  --  Current load resistance
      Conf_OK   : Boolean := False;                                 --  Status of initilization (true when configuration is set)
      Load_OK   : Boolean := False;                                 --  Status of initilization (true when load is set)
      T_Start   : Ada.Real_Time.Time := Ada.Real_Time.Time_First;   --  Buffer
   end Simulation_I_T;

   Sim : Simulation_I_T;  --  Instantiation of Interface

private

   task type Simulation_Task_T is
   end Simulation_Task_T;
   pragma Annotate (GNATprove, False_Positive, "task might terminate", "cheked by Simon");

   function Get_Load_Actual (ST : Ada.Real_Time.Time; LO : loadArray_T) return Float;

end PSU_Simulation;
