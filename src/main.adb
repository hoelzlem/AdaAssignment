with Ada.Command_Line, Ada.Text_IO; use Ada.Command_Line, Ada.Text_IO;
with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Vectors;
use  Ada.Containers;

with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;

with CONFIG_Parser; use CONFIG_Parser;
with global_constants; use global_constants;

procedure Main is
   package SU   renames Ada.Strings.Unbounded;
   package String_Vector is new Indefinite_Vectors (Natural, String);
   use String_Vector;

   filePath_configFile        : SU.Unbounded_String;
   filePath_loadFile          :  SU.Unbounded_String;
   filePath_logFile           :  SU.Unbounded_String;
   configFT, loadFT, outputFT : File_Type;
   PID_Conf_A                 : PID_Config_A_T;
   Sim_Config                 : Sim_Config_T;
   configValid                : Boolean;
   loadValid                  : Boolean;
   loadArray                  : loadArray_T;
   numLoadValues              : Integer;

begin
   Put_Line ("Please specify the path to the" &
                "configuration file of the simulation : ");
   filePath_configFile := SU.To_Unbounded_String
      ("good_config.txt");
   --  SU.To_Unbounded_String(Get_Line(Standard_Input));
   begin
      Open (File => configFT,
            Mode => In_File,
            Name => SU.To_String (filePath_configFile));
   exception
      when others =>
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET &
                      "]  Could not find or open the configuration file '"
                   & SU.To_String (filePath_configFile) & "'!");
         Set_Exit_Status (Failure);
         return;
   end;

   Put_Line ("Please specify the path to the load file of the simulation:");
   filePath_configFile := SU.To_Unbounded_String
      ("bad_load.txt");
   --  SU.To_Unbounded_String(Get_Line(Standard_Input));
   begin
      Open (File => loadFT,
            Mode => In_File,
            Name => SU.To_String (filePath_configFile));
   exception
      when others =>
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET &
                      "]  Could not find or open the load file '"
                   & SU.To_String (filePath_loadFile) & "'!");
         Set_Exit_Status (Failure);
         return;
   end;

   Put_Line ("Please specify the path where the output file" &
                "of the simulation shall be created : ");
   filePath_logFile := SU.To_Unbounded_String
      ("out.txt");
   --  SU.To_Unbounded_String(Get_Line(Standard_Input));
   begin
      Create (File => outputFT,
              Mode => Out_File,
              Name => SU.To_String (filePath_logFile));
   exception
      when others =>
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET &
                      "]  Could not create output file named '"
                   & SU.To_String (filePath_logFile) & "'.");
         Set_Exit_Status (Failure);
         return;
   end;
   Put_Line ("Parsing configuration file...");
   parseConfig (configFT, PID_Conf_A, Sim_Config, configValid);
   parseLoad (loadFT, loadArray, loadValid, numLoadValues);
   if (configValid) then
      Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Configuration is valid!");
      Sim.Set_Config (Sim_Config);
      Ctrl.Set_Config (PID_Conf_A);
   else Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Configuration is invalid!");
      return;
   end if;
   if (loadValid) then
      Put_Line ("[ " & vt100_GREEN & "OK" & vt100_RESET & " ]     Load definition is valid, contains " & numLoadValues'Image & " values!");
      Sim.Set_Load (loadArray);
   else Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load definition is invalid!");
      return;
   end if;

   if Is_Open (configFT) then
      Close (configFT);
   end if;
   if Is_Open (loadFT) then
      Close (loadFT);
   end if;
   if Is_Open (outputFT) then
      Close (outputFT);
   end if;
end Main;
