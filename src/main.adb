with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;


procedure Main is
  Sim_Conf  : Sim_Config_T;
  Ctrl_Conf : PID_Config_A_T;
  Next_Time : Time := Clock;
begin
  --    Ctrl_Conf.PFC_Config.Kp := 2.0;
  --    Ctrl_Conf.PFC_Config.Tn := 4.0;
  --    Ctrl_Conf.PFC_Config.Tv := 0.0;
  --    Ctrl_Conf.PFC_Config.T  := 0.0001;
  --    Ctrl_Conf.OUT_Config.Kp := 2.0;
  --    Ctrl_Conf.OUT_Config.Tn := 4.0;
  --    Ctrl_Conf.OUT_Config.Tv := 0.0;
  --    Ctrl_Conf.OUT_Config.T  := 0.0001;
  Sim_Conf.T := 0.00001;
  Sim_Conf.f_V1  := 0.0;
  Ctrl.Set_Config (Ctrl_Conf);
  Sim.set_Config (Sim_Conf);
  Ctrl.Set_W_U_C1 (100.0);
  Ctrl.Set_W_U_C2 (10.0);

end Main;
