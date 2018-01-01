pragma Profile (Ravenscar);
package PSU_Control is

  -- This type provides settings for a PID controller
  type PID_Config_T is record
    Kp  : Float := 1.0; -- P gain
    Tn  : Float := 1.0; -- Reset time
    Tv  : Float := 1.0; -- Lead time
    T   : Float := 1.0; -- Period time
  end record;
  
  type Ctrl_Config_T is record
    PFC_Config    : PID_Config_T;
    OUT_Config    : PID_Config_T;
  end record;

  protected type Control_I_T is
    entry Wait_For_Config;
    function Get_Config return Ctrl_Config_T;
    function Get_PFC_Config return PID_Config_T;
    function Get_OUT_Config return PID_Config_T;
    function Get_W_U_C1 return Float;
    function Get_W_U_C2 return Float;
    procedure Set_Config (Val : in Ctrl_Config_T);
    procedure Set_PFC_Config (Val : PID_Config_T); 
    procedure Set_OUT_Config (Val : PID_Config_T);    
    procedure Set_W_U_C1 (Val : in Float);
    procedure Set_W_U_C2 (Val : in Float);
  private
    W_U_C1       : Float := 0.0;
    W_U_C2       : Float := 0.0;
    Conf         : Ctrl_Config_T;   
    Conf_ALL_OK  : Boolean := False;
    Conf_PFC_OK  : Boolean := False;
    Conf_OUT_OK  : Boolean := False;
  end Control_I_T;
  Ctrl : Control_I_T;
  
private
  -- This type provides a PID controller
  -- Use of a tagged object would not be necesarry here
  type PID_Controller_T is tagged
    record
      Conf : PID_Config_T;
      E    : Float := 0.0;
      E1   : Float := 0.0;
      I    : Float := 0.0;
      U    : Float := 0.0;
    end record;
  
  function calculate_U (C : in out PID_Controller_T; W : in Float; Y : in Float) return Float;
  
  task type Control_Task_T is
  end Control_Task_T;   

end PSU_Control;
