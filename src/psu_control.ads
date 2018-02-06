pragma Profile (Ravenscar);
package PSU_Control is

   --  This type provides settings for a PID controller
   type PID_Config_T is record
      Kp  : Float := 1.0;            --  P gain
      Ki  : Float := 1.0;            --  I gain (multiplied with Kp)
      Kd  : Float := 1.0;            --  D gain (multiplied with Kp)
      Sp  : Float := Float'Last;     --  Upper bound for saturation
      Sn  : Float := Float'First;    --  Lower bound for saturation
      T   : Float := 1.0;            --  Period time of calculation
   end record;

   --  This type provides a PID controller
   --  Use of a tagged object would not be necesarry here
   type PID_Controller_T is tagged
      record
         Conf : PID_Config_T;  --  Configuration
         E    : Float := 0.0;  --  Error
         E1   : Float := 0.0;  --  Previous Error
         P    : Float := 0.0;  --  Proportional part
         I    : Float := 0.0;  --  Intergrator part
         D    : Float := 0.0;  --  Differential part
         U    : Float := 0.0;  --  Output
         Sat  : Float := 0.0;  --  Saturation
      end record;
   function calculate_U (C : in out PID_Controller_T; W : in Float; Y : in Float) return Float; --  Run the PID controller
   procedure Reset (C : in out PID_Controller_T);  --  Reset the controller (intergrator)

   --  Direct-Form II based IIR-Filter
   type IIR_Filter_T is tagged
      record
         z1   : Float := 0.0;  --  Previous input (z-1)
         z2   : Float := 0.0;  --  Input * z-2
         gi   : Float := 0.0;  --  Input Gain
         go   : Float := 0.0;  --  Output Gain
         d1   : Float := 0.0;  --  Denumerator 1
         d2   : Float := 0.0;  --  Denumerator 2
         n0   : Float := 0.0;  --  Numerator 0
         n1   : Float := 0.0;  --  Numerator 1
         n2   : Float := 0.0;  --  Numerator 2
      end record;
   function Do_Filtering (F : in out IIR_Filter_T; I : in Float) return Float;         --  Filter the signal (called in a fixed interval)
   procedure Set_Config (F : in out IIR_Filter_T; gi, go, d1, d2, n0, n1, n2 : in Float);  --  Set the configuration of the Filter

   type PID_Target_T is (PID_U_C1, PID_U_C2, PID_I_L1, PID_I_L2);            --  Enum: List of all controllers
   type PID_Config_A_T is array (PID_Target_T)  of PID_Config_T;             --  Config for all Controllers
   type PID_Config_Status_A_T is array (PID_Target_T) of Boolean;            --  Status of configuration
   type PID_Controller_A_T is array (PID_Target_T) of PID_Controller_T;      --  Used to access multiple controllers


   protected type Control_I_T is
      function Is_Ready return Boolean;                                    --  Synchronize threads at startup (returns true when everything is configured)
      function Get_Config return PID_Config_A_T;                           --  Get the configuration for all the controllers
      function Get_Config (Id : in PID_Target_T) return PID_Config_T;      --  Get the configuration for a single controller
      function Get_W_U_C1 return Float;                                    --  Get the setpoint of the voltage over C1
      function Get_W_U_C2 return Float;                                    --  Get the setpoint of the voltage over C2
      function Get_Safety_State return Boolean;                            --  Get the current saftey state (supplied by monitoring)
      procedure Set_Config (Val : in PID_Config_A_T);                      --  Set the configuration of the controllers
      procedure Set_Config (Val : PID_Config_T; Id : PID_Target_T);        --  Set the configuration of a single controller
      procedure Set_W_U_C1 (Val : in Float);                               --  Get the setpoint of the voltage over C1
      procedure Set_W_U_C2 (Val : in Float);                               --  Get the setpoint of the voltage over C2
      procedure Set_Safety_State (Val : in Boolean);                       --  Get the current saftey state (supplied by monitoring)
   private
      W_U_C1       : Float := 0.0;                                         --  Buffer
      W_U_C2       : Float := 0.0;                                         --  Buffer
      Conf         : PID_Config_A_T;                                       --  Buffer
      Conf_ALL_OK  : Boolean := False;                                     --  Status of configuration
      Conf_State   : PID_Config_Status_A_T := (others => False);           --  Status of a single configuration
      Safety_State : Boolean := False;                                     --  Buffer
   end Control_I_T;
   Ctrl : Control_I_T;

   task type Control_Task_T is
   end Control_Task_T;

end PSU_Control;
