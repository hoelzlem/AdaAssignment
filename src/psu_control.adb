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

    function Get_Config return Ctrl_Config_T is
    begin
      return Conf;
    end Get_Config;

    function Get_PFC_Config return PID_Config_T is
    begin
      return Conf.PFC_Config;
    end Get_PFC_Config;

    function Get_OUT_Config return PID_Config_T is
    begin
      return Conf.OUT_Config;
    end Get_OUT_Config;

    function Get_W_U_C1 return Float is
    begin
      return W_U_C1;
    end Get_W_U_C1;

    function Get_W_U_C2 return Float is
    begin
      return W_U_C2;
    end Get_W_U_C2;

    procedure Set_Config (Val : in Ctrl_Config_T) is
    begin
      Conf := Val;
      Conf_ALL_OK := True;
      Conf_PFC_OK := True;
      Conf_OUT_OK := True;
    end Set_Config;

    procedure Set_PFC_Config (Val : PID_Config_T)is
    begin
      Conf.PFC_Config := Val;
      Conf_PFC_OK := True;
      if (Conf_OUT_OK = True)then
        Conf_ALL_OK := True;
      end if;
    end Set_PFC_Config;

    procedure Set_OUT_Config (Val : PID_Config_T)is
    begin
      Conf.OUT_Config := Val;
      Conf_OUT_OK := True;
      if (Conf_PFC_OK = True)then
        Conf_ALL_OK := True;
      end if;
    end Set_OUT_Config;

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
    Sim_Conf       : Sim_Config_T;
    Conf           : Ctrl_Config_T;
    Next_Time      : Time := Clock;
    D_M1, D_M2_5   : Float := 0.0;
    PFC_Controller : PID_Controller_T;
    OUT_Controller : PID_Controller_T;
  begin
    Ada.Text_IO.Put_Line ("Doing contorl");
    Sim.get_Config (Sim_Conf);
    Ctrl.Wait_For_Config;
    Conf := Ctrl.Get_Config;
    PFC_Controller.Conf := Conf.PFC_Config;
    OUT_Controller.Conf := Conf.OUT_Config;
    Next_Time := Clock;
    loop
      Ada.Text_IO.Put_Line ("Doing contorl");
      D_M1 := calculate_U (C => PFC_Controller, W => Ctrl.Get_W_U_C1, Y => Sim.get_U_C1);
      if (D_M1 > 1.0)then
        D_M1 := 1.0;
      elsif (D_M1 < 0.0)then
        D_M1 := 0.0;
      end if;
      Sim.set_D_M1 (D_M1);

      D_M2_5 := calculate_U (C => OUT_Controller, W => Ctrl.Get_W_U_C2, Y => Sim.get_U_C2);
      if (D_M2_5 > 1.0)then
        D_M2_5 := 1.0;
      elsif (D_M2_5 < 0.0)then
        D_M2_5 := 0.0;
      end if;
      Sim.set_D_M2_5 (D_M2_5);

      Next_Time := Next_Time + Milliseconds (Integer (Float (Conf.PFC_Config.T) * 1.0e6));
      delay until Next_Time;
    end loop;
  end Control_Task_T;
  Control_Task : Control_Task_T;

end PSU_Control;
