pragma Profile(Ravenscar);
with Ada.Text_IO; use Ada.Text_IO;
with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;

package CONFIG_Parser is

   procedure parseConfig (fileHandle : in File_Type; Ctrl_Conf : in out Ctrl_Config_T);
   -- Sim_Config_T
   -- Output: Sim_Output_T
end CONFIG_Parser;
