pragma Profile (Ravenscar);
with PSU_Simulation; use PSU_Simulation;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;
package body PSU_Control is

  protected body Control_I_T is

    entry Wait_For_Config when Conf_ALL_OK is
    begin
      null;
    end Wait_For_Config;

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

    procedure Set_Config (Val : in PID_Config_A_T) is
    begin
      Conf := Val;
      Conf_ALL_OK := True;
      Conf_Status := (others => True);
    end Set_Config;

    procedure Set_Config (Val : PID_Config_T; Id : PID_Target_T) is
      status : Boolean := True;
    begin
      Conf (Id) := Val;
      Conf_Status (Id) := True;
      -- And reduce would certainly be nicer here
      for id in Conf_Status'Range loop
        status := status and Conf_Status (id);
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

  end Control_I_T;

  function calculate_U (C : in out PID_Controller_T; W : in Float; Y : in Float) return Float is
  begin
    C.E1 := C.E;
    C.E := W - Y;
    C.I := C.I + C.E * C.Conf.Kp / C.Conf.Tn * C.Conf.T;
    return C.I + C.Conf.Kp * (C.E + C.Conf.Tv / C.Conf.T * (C.E - C.E1));
  end calculate_U;

  task body Control_Task_T is
    Conf           : PID_Config_A_T;
    Next_Time      : Time := Clock;
    D_M1, D_M2_5   : Float := 0.0;
    I_L1, I_L2     : Float := 0.0;
    Controllers    : PID_Controller_A_T;
    OUT_Controller : PID_Controller_T;
  begin
    Sim.Wait_For_Config;
    Ctrl.Wait_For_Config;
    Conf := Ctrl.Get_Config;
    -- Write Config to Controllers could be done with access type
    for id in PID_Controller_A_T'Range loop
      Controllers (id).Conf := Conf (id);
    end loop;
    Next_Time := Clock;

    loop
      Ada.Text_IO.Put_Line ("Doing contorl");
      -- Run the controllers
      I_L1 := calculate_U (C => Controllers (PID_U_C1), W => Ctrl.Get_W_U_C1, Y => Sim.Get_U_C1);
      I_L2 := calculate_U (C => Controllers (PID_U_C2), W => Ctrl.Get_W_U_C2, Y => Sim.Get_U_C2);
      D_M1 := calculate_U (C => Controllers (PID_I_L1), W => I_L1, Y => sim.Get_I_L1);
      D_M2_5 := calculate_U (C => Controllers (PID_I_L2), W => I_L2, Y => sim.Get_I_L2);
      -- Saturate (will be part of controller)
      if (D_M1 > 1.0)then
        D_M1 := 1.0;
      elsif (D_M1 < 0.0)then
        D_M1 := 0.0;
      end if;
      if (D_M2_5 > 1.0)then
        D_M2_5 := 1.0;
      elsif (D_M2_5 < 0.0)then
        D_M2_5 := 0.0;
      end if;
      -- Set for Simulation
      Sim.set_D_M1 (D_M1);
      Sim.set_D_M2_5 (D_M2_5);

      Next_Time := Next_Time + Milliseconds (Integer (Float (Conf (PID_U_C1).T) * 1.0e6));
      delay until Next_Time;
    end loop;
  end Control_Task_T;
  Control_Task : Control_Task_T;

end PSU_Control;
