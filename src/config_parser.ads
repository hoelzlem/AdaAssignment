pragma Profile (Ravenscar);
with Ada.Text_IO; use Ada.Text_IO;
with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;

package CONFIG_Parser is

    procedure parseConfig (fileHandle : in File_Type; PID_Config : in out PID_Config_A_T; Sim_Config : in out Sim_Config_T; configValid : out Boolean);
    procedure parseLoad (fileHandle : in File_Type; loadArray : out loadArray_T; loadValid : out Boolean);
end CONFIG_Parser;
