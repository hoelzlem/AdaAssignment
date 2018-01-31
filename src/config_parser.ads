pragma Profile (Ravenscar);
with Ada.Text_IO; use Ada.Text_IO;
with PSU_Simulation; use PSU_Simulation;
with PSU_Monitoring; use PSU_Monitoring;
with PSU_Control; use PSU_Control;

package CONFIG_Parser is
   procedure Open_File (File : in out File_Type;
                        Name : in String);

   procedure Create_File (File : in out File_Type;
                          Name : in String);

   procedure Close_File (File : in out File_Type);

   procedure parseSimConfig (fileHandle  : in File_Type;
                             PID_Config  : in out PID_Config_A_T;
                             Sim_Config  : in out Sim_Config_T;
                             configValid : out Boolean);

   procedure parseMonitorConfig (fileHandle         : in File_Type;
                                 pfc_voltage_config : out Monitor_Config_T;
                                 pfc_current_config : out Monitor_Config_T;
                                 out_voltage_config : out Monitor_Config_T;
                                 out_current_config : out Monitor_Config_T;
                                 configValid        : out Boolean);

   procedure parseLoad (fileHandle    : in File_Type;
                        loadArray     : out loadArray_T;
                        loadValid     : out Boolean;
                        numLoadValues : out Integer);
end CONFIG_Parser;
