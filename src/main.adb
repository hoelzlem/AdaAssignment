with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Vectors;
use  Ada.Containers;

with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;

with CONFIG_Parser; use CONFIG_Parser;
with global_constants; use global_constants;

with PSU_Monitoring;
with PSU_Logging;

procedure Main is
   package SU   renames Ada.Strings.Unbounded;
   package String_Vector is new Indefinite_Vectors (Natural, String);
   use String_Vector;

   filePath_configFile        : SU.Unbounded_String;
   filePath_loadFile          : SU.Unbounded_String;
   filePath_logFile           : SU.Unbounded_String;
   configFT, loadFT, outputFT : File_Type;
   PID_Conf_A                 : PID_Config_A_T;
   Sim_Config                 : Sim_Config_T;
   configValid                : Boolean;
   loadValid                  : Boolean;
   loadArray                  : loadArray_T;
   numLoadValues              : Integer;

begin
   Put_Line ("Do you want to load default config files? [Y/N]");
   if (Get_Line (Standard_Input) = "Y") then
      filePath_configFile := SU.To_Unbounded_String ("good_config.txt");
      filePath_loadFile := SU.To_Unbounded_String ("good_load.txt");
      filePath_logFile    := SU.To_Unbounded_String ("out.txt");
   else
      Put_Line ("Please specify the path to the" & "configuration file of the simulation : ");
      filePath_configFile := SU.To_Unbounded_String (Get_Line (Standard_Input));
      Put_Line ("Please specify the path to the load file of the simulation:");
      filePath_loadFile := SU.To_Unbounded_String (Get_Line (Standard_Input));
      Put_Line ("Please specify the path where the output file" & "of the simulation shall be created : ");
      filePath_logFile    := SU.To_Unbounded_String (Get_Line (Standard_Input));
   end if;

   Open_File (File => configFT,
              Name => filePath_configFile);
   Open_File (File => loadFT,
              Name => filePath_loadFile);
   Create_File (File => outputFT,
                Name => filePath_logFile);

   Put_Line ("Parsing configuration file...");
   parseConfig (configFT, PID_Conf_A, Sim_Config, configValid);
   if (configValid) then
      Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Configuration is valid!");
      Sim.Set_Config (Sim_Config);
      Ctrl.Set_Config (PID_Conf_A);
   else Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Configuration is invalid!");
      return;
   end if;
   Put_Line ("Parsing electric_load file...");
   parseLoad (loadFT, loadArray, loadValid, numLoadValues);
   if (loadValid) then
      Put_Line ("[ " & vt100_GREEN & "OK" & vt100_RESET & " ]     Load definition is valid, contains " & numLoadValues'Image & " values!");
      Sim.Set_Load (loadArray);
   else Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load definition is invalid!");
      return;
   end if;

   Close_File (configFT);
   Close_File (loadFT);
   Close_File (outputFT);

   --  @TODO add code that configures the monitoring module
   --  The module is automatically started after full configuration

end Main;

