with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Indefinite_Vectors;
use  Ada.Containers;

with global_constants; use global_constants;
with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;
with PSU_Monitoring; use PSU_Monitoring;
with PSU_Logging; use PSU_Logging;

with CONFIG_Parser; use CONFIG_Parser;


procedure Main is
   package String_Vector is new Indefinite_Vectors (Natural, String);
   use String_Vector;

   simConfigFT         : File_Type;
   monitorConfigFT     : File_Type;
   loadDefFT           : File_Type;
   outputFT            : aliased File_Type;
   PID_Conf_A          : PID_Config_A_T;
   Sim_Config          : Sim_Config_T;
   configValid         : Boolean;
   monitorConfigValid  : Boolean;
   loadValid           : Boolean;
   loadArray           : loadArray_T;
   numLoadValues       : Integer;
   supervisor_config   : Supervisor_Config_T;
   pfc_voltage_config  : Monitor_Config_T;
   pfc_current_config  : Monitor_Config_T;
   out_voltage_config  : Monitor_Config_T;
   out_current_config  : Monitor_Config_T;

begin
   Put_Line ("Do you want to load default config files? [y/n]");
   if (Get_Line (Standard_Input) = "y") then
      Open_File (File => simConfigFT,
                 Name => "std_sim_config.txt");
      Open_File (File => monitorConfigFT,
                 Name => "std_monitor_config.txt");
      Open_File (File => loadDefFT,
                 Name => "std_load_config.txt");
      Create_File (File => outputFT,
                   Name => "sim_output.csv");
   else
      Put_Line ("Please specify the path to the configuration file of the simulation: ");
      Open_File (File => simConfigFT,
                 Name => Get_Line (Standard_Input));
      Put_Line ("Please specify the path to the configuration file of the monitor:");
      Open_File (File => monitorConfigFT,
                 Name => Get_Line (Standard_Input));
      Put_Line ("Please specify the path to the load file of the simulation:");
      Open_File (File => loadDefFT,
                 Name => Get_Line (Standard_Input));
      Put_Line ("Please specify the path where the output file of the simulation shall be created : ");
      Create_File (File => outputFT,
                   Name => Get_Line (Standard_Input));
   end if;

   parseSimConfig (simConfigFT, PID_Conf_A, Sim_Config, configValid);
   if (configValid) then
      Sim.Set_Config (Sim_Config);
      Ctrl.Set_Config (PID_Conf_A);
   end if;

   parseMonitorConfig (monitorConfigFT, pfc_voltage_config, pfc_current_config, out_voltage_config, out_current_config, monitorConfigValid);
   if (monitorConfigValid) then
      monitoring_interface.set_supervisor_config (supervisor_config);
      monitoring_interface.set_monitor_pfc_voltage_config (pfc_voltage_config);
      monitoring_interface.set_monitor_pfc_current_config (pfc_current_config);
      monitoring_interface.set_monitor_output_voltage_config (out_voltage_config);
      monitoring_interface.set_monitor_output_current_config (out_current_config);

      --  Check if config was accepted
      if monitoring_interface.is_config_erroneous then
         Put_Line (vt100_RED & "One of the monitor configuration structures was specified with lower_threshold > upper_threshold "
                   & "while monitoring mode was set to threshold_based. This configuration is not suitable and must be corrected. "
                   & "The monitoring module won't run otherwise." & vt100_RESET);
      end if;
   end if;

   parseLoad (loadDefFT, loadArray, loadValid, numLoadValues);
   if (loadValid) then
      Sim.Set_Load_A (loadArray);
   end if;

   logger_interface.set_logfile (outputFT'Unchecked_Access);

   --  Set the desired voltages
   Ctrl.Set_W_U_C1 (350.0);
   Ctrl.Set_W_U_C2 (24.0);

   Close_File (simConfigFT);
   Close_File (monitorConfigFT);
   Close_File (loadDefFT);

end Main;

