--  Unbounded Strings handling inspired by https://en.wikibooks.org/wiki/Ada_Programming/Strings
pragma Profile (Ravenscar);

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Command_Line;
with global_constants; use global_constants;
with Ada.Strings.Unbounded;

package body CONFIG_Parser is

   package SU   renames Ada.Strings.Unbounded;

   procedure Open_File (File : in out File_Type; Name : in String) is
   begin
      begin
         Open (File => File,
               Mode => In_File,
               Name => Name);
      exception
         when others =>
            Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Could not find or open the specified file '" & Name & "'!");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
      end;
   end Open_File;

   procedure Create_File (File : in out File_Type; Name : in String) is
   begin
      begin
         Create (File => File,
                 Mode => Out_File,
                 Name => Name);
      exception
         when others =>
            Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Could not create specified file named '" & Name & "'.");
            Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            return;
      end;
   end Create_File;

   procedure Close_File  (File : in out File_Type) is
   begin
      if Is_Open (File) then
         Close (File);
      end if;
   end Close_File;

   procedure parseSimConfig (fileHandle : in File_Type; PID_Config : in out PID_Config_A_T; Sim_Config : in out Sim_Config_T; configValid : out Boolean) is

      i           : Integer := 0;
      paramString : SU.Unbounded_String;
      valueString : SU.Unbounded_String;
      type ConfigParams is
         (PID_U_C1_Kp, PID_U_C1_Ki, PID_U_C1_Kd, PID_U_C1_Sp, PID_U_C1_Sn, PID_U_C1_T,
          PID_U_C2_Kp, PID_U_C2_Ki, PID_U_C2_Kd, PID_U_C2_Sp, PID_U_C2_Sn, PID_U_C2_T,
          PID_I_L1_Kp, PID_I_L1_Ki, PID_I_L1_Kd, PID_I_L1_Sp, PID_I_L1_Sn, PID_I_L1_T,
          PID_I_L2_Kp, PID_I_L2_Ki, PID_I_L2_Kd, PID_I_L2_Sp, PID_I_L2_Sn, PID_I_L2_T,
          Sim_L1, Sim_C1, Sim_L2, Sim_C2, Sim_f_V1, Sim_Up_V1, Sim_T);
      type paramConfigured_T is array (ConfigParams) of Boolean;
      paramConfigured : paramConfigured_T;
      paramValid  : Boolean;

   begin
      paramConfigured := (others => False);
      configValid := True;
      Put_Line ("Parsing configuration file...");
      loop
         declare
            Line : constant String := Get_Line (fileHandle);
         begin
            i := Index (Line, " ");
            if (Line'Length = 0) then
               null;
            else
               if ((Line (1) /= '-') and (i > 1 and i < Line'Last)) then
                  paramString := SU.To_Unbounded_String (Line (Line'First .. i - 1));
                  valueString := SU.To_Unbounded_String (Line (i + 1 .. Line'Last));
                  paramValid := True;
                  begin
                     if (ConfigParams'Value (SU.To_String (paramString))'Image = " ") then  -- do some useless stuff, but needed to check if input valid. Provokes exception if invalid
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Parameter " & SU.To_String (paramString) & " is not >= 0");
                        paramValid := False;
                     end if;
                  exception
                     when Constraint_Error =>
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Configuration parameter unknown: " & SU.To_String (paramString));
                        paramValid := False;
                  end;
                  begin
                     if (Float'Value (SU.To_String (valueString)) = Float'Invalid_Value) then -- do some useless stuff, but needed to check if input valid. Provokes exception if invalid
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Value " & SU.To_String (valueString) & " is not >= 0");
                        paramValid := False;
                     end if;
                  exception
                     when Constraint_Error =>
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Configuration value " & SU.To_String (paramString) & " invalid: " & SU.To_String (valueString));
                        paramValid := False;
                  end;

                  if (paramValid) then
                     --  Put_Line (ConfigParams'Value (SU.To_String (paramString))'Image);
                     case ConfigParams'Value (SU.To_String (paramString)) is
                        --  Parse Contorller for U_C1
                     when PID_U_C1_Kp =>
                        PID_Config (PID_U_C1).Kp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C1 Kp set to " & Float'Image (PID_Config (PID_U_C1).Kp));
                     when PID_U_C1_Ki =>
                        PID_Config (PID_U_C1).Ki := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C1 Ki set to " & Float'Image (PID_Config (PID_U_C1).Ki));
                     when PID_U_C1_Kd =>
                        PID_Config (PID_U_C1).Kd := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C1 Kd set to " & Float'Image (PID_Config (PID_U_C1).Kd));
                     when PID_U_C1_Sp =>
                        PID_Config (PID_U_C1).Sp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C1 Sp set to " & Float'Image (PID_Config (PID_U_C1).Sp));
                     when PID_U_C1_Sn =>
                        PID_Config (PID_U_C1).Sn := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C1 Sn set to " & Float'Image (PID_Config (PID_U_C1).Sn));
                     when PID_U_C1_T =>
                        PID_Config (PID_U_C1).T := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C1 T  set to " & Float'Image (PID_Config (PID_U_C1).T));
                        --  Parse Contorller for U_C2
                     when PID_U_C2_Kp =>
                        PID_Config (PID_U_C2).Kp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C2 Kp set to " & Float'Image (PID_Config (PID_U_C2).Kp));
                     when PID_U_C2_Ki =>
                        PID_Config (PID_U_C2).Ki := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C2 Ki set to " & Float'Image (PID_Config (PID_U_C2).Ki));
                     when PID_U_C2_Kd =>
                        PID_Config (PID_U_C2).Kd := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C2 Kd set to " & Float'Image (PID_Config (PID_U_C2).Kd));
                     when PID_U_C2_Sp =>
                        PID_Config (PID_U_C2).Sp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C2 Sp set to " & Float'Image (PID_Config (PID_U_C2).Sp));
                     when PID_U_C2_Sn =>
                        PID_Config (PID_U_C2).Sn := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C2 Sn set to " & Float'Image (PID_Config (PID_U_C2).Sn));
                     when PID_U_C2_T =>
                        PID_Config (PID_U_C2).T := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_U_C2 T  set to " & Float'Image (PID_Config (PID_U_C2).T));
                        --  Parse Contorller for I_L1
                     when PID_I_L1_Kp =>
                        PID_Config (PID_I_L1).Kp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L1 Kp set to " & Float'Image (PID_Config (PID_I_L1).Kp));
                     when PID_I_L1_Ki =>
                        PID_Config (PID_I_L1).Ki := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L1 Ki set to " & Float'Image (PID_Config (PID_I_L1).Ki));
                     when PID_I_L1_Kd =>
                        PID_Config (PID_I_L1).Kd := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L1 Kd set to " & Float'Image (PID_Config (PID_I_L1).Kd));
                     when PID_I_L1_Sp =>
                        PID_Config (PID_I_L1).Sp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L1 Sp set to " & Float'Image (PID_Config (PID_I_L1).Sp));
                     when PID_I_L1_Sn =>
                        PID_Config (PID_I_L1).Sn := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L1 Sn set to " & Float'Image (PID_Config (PID_I_L1).Sn));
                     when PID_I_L1_T =>
                        PID_Config (PID_I_L1).T := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L1 T  set to " & Float'Image (PID_Config (PID_I_L1).T));
                        --  Parse Contorller for I_L2
                     when PID_I_L2_Kp =>
                        PID_Config (PID_I_L2).Kp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L2 Kp set to " & Float'Image (PID_Config (PID_I_L2).Kp));
                     when PID_I_L2_Ki =>
                        PID_Config (PID_I_L2).Ki := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L2 Ki set to " & Float'Image (PID_Config (PID_I_L2).Ki));
                     when PID_I_L2_Kd =>
                        PID_Config (PID_I_L2).Kd := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L2 Kd set to " & Float'Image (PID_Config (PID_I_L2).Kd));
                     when PID_I_L2_Sp =>
                        PID_Config (PID_I_L2).Sp := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L2 Sp set to " & Float'Image (PID_Config (PID_I_L2).Sp));
                     when PID_I_L2_Sn =>
                        PID_Config (PID_I_L2).Sn := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L2 Sn set to " & Float'Image (PID_Config (PID_I_L2).Sn));
                     when PID_I_L2_T =>
                        PID_Config (PID_I_L2).T := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PID_I_L2 T  set to " & Float'Image (PID_Config (PID_I_L2).T));
                        --  Parse Simulation config
                     when Sim_L1 =>
                        Sim_Config.L1 := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Sim      L1 set to " & Float'Image  (Sim_Config.L1));
                     when Sim_C1 =>
                        Sim_Config.C1 := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Sim      C1 set to " & Float'Image (Sim_Config.C1));
                     when Sim_L2 =>
                        Sim_Config.L2 := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Sim      L2 set to " & Float'Image (Sim_Config.L2));
                     when Sim_C2 =>
                        Sim_Config.C2 := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Sim      C2 set to " & Float'Image (Sim_Config.C2));
                     when Sim_f_V1 =>
                        Sim_Config.f_V1 := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Sim    f_V1 set to " & Float'Image (Sim_Config.f_V1));
                     when Sim_Up_V1 =>
                        Sim_Config.Up_V1 := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Sim   Up_V1 set to " & Float'Image (Sim_Config.Up_V1));
                     when Sim_T =>
                        Sim_Config.T := Float'Value (SU.To_String (valueString));
                        Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Sim      T  set to " & Float'Image (Sim_Config.T));
                     end case;
                     paramConfigured (ConfigParams'Value (SU.To_String (paramString))) := True;
                  end if;
               end if;
            end if;
         end;
         if (End_Of_File (fileHandle)) then
            for i in paramConfigured'Range loop
               if (paramConfigured (i) = False) then
                  Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Simulation parameter not configured: " & i'Image);
                  configValid := False;
               end if;
            end loop;
            exit;
         end if;
      end loop;
      if (configValid) then
         Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Simulation configuration is valid!");
      else
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Simulation configuration is invalid!");
      end if;
   exception
      when others =>
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Simulation configuration is invalid!");
   end parseSimConfig;

   procedure parseMonitorConfig (fileHandle : in File_Type; pfc_voltage_config : out Monitor_Config_T; pfc_current_config : out Monitor_Config_T; out_voltage_config : out Monitor_Config_T; out_current_config : out Monitor_Config_T; configValid : out Boolean) is

      i           : Integer := 0;
      paramString : SU.Unbounded_String;
      valueString : SU.Unbounded_String;
      type ConfigParams is
         (PFC_monitor_voltage_mode, PFC_monitor_voltage_mean, PFC_monitor_voltage_maximum_deviation, PFC_monitor_voltage_lower_threshold, PFC_monitor_voltage_upper_threshold,
          PFC_monitor_current_mode, PFC_monitor_current_mean, PFC_monitor_current_maximum_deviation, PFC_monitor_current_lower_threshold, PFC_monitor_current_upper_threshold,
          OUT_monitor_voltage_mode, OUT_monitor_voltage_mean, OUT_monitor_voltage_maximum_deviation, OUT_monitor_voltage_lower_threshold, OUT_monitor_voltage_upper_threshold,
          OUT_monitor_current_mode, OUT_monitor_current_mean, OUT_monitor_current_maximum_deviation, OUT_monitor_current_lower_threshold, OUT_monitor_current_upper_threshold);
      type paramConfigured_T is array (ConfigParams) of Boolean;
      paramConfigured : paramConfigured_T;
      paramValid  : Boolean;

   begin
      paramConfigured := (others => False);
      configValid := True;
      Put_Line ("Parsing monitor configuration file...");
      loop
         declare
            Line : constant String := Get_Line (fileHandle);
         begin
            i := Index (Line, " ");
            if (Line'Length = 0) then
               null;
            else
               if ((Line (1) /= '-') and (i > 1 and i < Line'Last)) then
                  paramString := SU.To_Unbounded_String (Line (Line'First .. i - 1));
                  valueString := SU.To_Unbounded_String (Line (i + 1 .. Line'Last));
                  paramValid := True;
                  begin
                     if (ConfigParams'Value (SU.To_String (paramString))'Image = " ") then  -- do some useless stuff, but needed to check if input valid. Provokes exception if invalid
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Parameter " & SU.To_String (paramString) & " is empty!");
                        paramValid := False;
                     end if;
                  exception
                     when Constraint_Error =>
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Configuration parameter unknown: " & SU.To_String (paramString));
                        paramValid := False;
                  end;
                  begin
                     if (Ada.Strings.Fixed.Tail (SU.To_String (paramString), 4) = "mode") then
                        if (Monitoring_Mode_T'Value (SU.To_String (valueString)) = Monitoring_Mode_T'Invalid_Value) then
                           Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Configuration value " & SU.To_String (paramString) & " invalid: " & SU.To_String (valueString));
                        end if;
                     elsif (Float'Value (SU.To_String (valueString)) = Float'Invalid_Value) then -- do some useless stuff, but needed to check if input valid. Provokes exception if invalid
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Value " & SU.To_String (valueString) & " is not valid!");
                        paramValid := False;
                     end if;
                  exception
                     when Constraint_Error =>
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Configuration value " & SU.To_String (paramString) & " invalid: " & SU.To_String (valueString));
                        paramValid := False;
                  end;

                  if (paramValid) then
                     --  Put_Line (ConfigParams'Value (SU.To_String (paramString))'Image);
                     case ConfigParams'Value (SU.To_String (paramString)) is
                        --  PFC voltage
                        when PFC_monitor_voltage_mode =>
                           pfc_voltage_config.monitoring_mode := Monitoring_Mode_T'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor voltage_mode set to " & Monitoring_Mode_T'Image (pfc_voltage_config.monitoring_mode));
                        when PFC_monitor_voltage_mean =>
                           pfc_voltage_config.mean := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor voltage_mean set to " & Float'Image (pfc_voltage_config.mean));
                        when PFC_monitor_voltage_maximum_deviation =>
                           pfc_voltage_config.maximum_deviation := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor voltage_maximum_deviation set to " & Float'Image (pfc_voltage_config.maximum_deviation));
                        when PFC_monitor_voltage_lower_threshold =>
                           pfc_voltage_config.lower_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor voltage_lower_threshold set to " & Float'Image (pfc_voltage_config.lower_threshold));
                        when PFC_monitor_voltage_upper_threshold =>
                           pfc_voltage_config.upper_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor voltage_upper_threshold set to " & Float'Image (pfc_voltage_config.upper_threshold));
                           --  PFC current
                        when PFC_monitor_current_mode =>
                           pfc_current_config.monitoring_mode := Monitoring_Mode_T'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor current_mode set to " & Monitoring_Mode_T'Image (pfc_current_config.monitoring_mode));
                        when PFC_monitor_current_mean =>
                           pfc_current_config.mean := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor current_mean set to " & Float'Image (pfc_current_config.mean));
                        when PFC_monitor_current_maximum_deviation =>
                           pfc_current_config.maximum_deviation := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor current_maximum_deviation set to " & Float'Image (pfc_current_config.maximum_deviation));
                        when PFC_monitor_current_lower_threshold =>
                           pfc_current_config.lower_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor current_lower_threshold set to " & Float'Image (pfc_current_config.lower_threshold));
                        when PFC_monitor_current_upper_threshold =>
                           pfc_current_config.upper_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     PFC monitor current_upper_threshold set to " & Float'Image (pfc_current_config.upper_threshold));
                           --  OUT voltage
                        when OUT_monitor_voltage_mode =>
                           out_voltage_config.monitoring_mode := Monitoring_Mode_T'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor voltage_mode set to " & Monitoring_Mode_T'Image (out_voltage_config.monitoring_mode));
                        when OUT_monitor_voltage_mean =>
                           out_voltage_config.mean := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor voltage_mean set to " & Float'Image (out_voltage_config.mean));
                        when OUT_monitor_voltage_maximum_deviation =>
                           out_voltage_config.maximum_deviation := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor voltage_maximum_deviation set to " & Float'Image (out_voltage_config.maximum_deviation));
                        when OUT_monitor_voltage_lower_threshold =>
                           out_voltage_config.lower_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor voltage_lower_threshold set to " & Float'Image (out_voltage_config.lower_threshold));
                        when OUT_monitor_voltage_upper_threshold =>
                           out_voltage_config.upper_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor voltage_upper_threshold set to " & Float'Image (out_voltage_config.upper_threshold));
                           --  OUT current
                        when OUT_monitor_current_mode =>
                           out_current_config.monitoring_mode := Monitoring_Mode_T'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor current_mode set to " & Monitoring_Mode_T'Image (out_current_config.monitoring_mode));
                        when OUT_monitor_current_mean =>
                           out_current_config.mean := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor current_mean set to " & Float'Image (out_current_config.mean));
                        when OUT_monitor_current_maximum_deviation =>
                           out_current_config.maximum_deviation := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor current_maximum_deviation set to " & Float'Image (out_current_config.maximum_deviation));
                        when OUT_monitor_current_lower_threshold =>
                           out_current_config.lower_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor current_lower_threshold set to " & Float'Image (out_current_config.lower_threshold));
                        when OUT_monitor_current_upper_threshold =>
                           out_current_config.upper_threshold := Float'Value (SU.To_String (valueString));
                           Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     OUT monitor current_upper_threshold set to " & Float'Image (out_current_config.upper_threshold));
                     end case;
                     paramConfigured (ConfigParams'Value (SU.To_String (paramString))) := True;
                  end if;
               end if;
            end if;
         end;
         if (End_Of_File (fileHandle)) then
            for i in paramConfigured'Range loop
               if (paramConfigured (i) = False) then
                  Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Monitor parameter not configured: " & i'Image);
                  configValid := False;
               end if;
            end loop;
            exit;
         end if;
      end loop;
      if (configValid) then
         Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Monitor configuration is valid!");
      else
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Monitor configuration is invalid!");
      end if;
   exception
      when others =>
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Monitor configuration is invalid!");
   end parseMonitorConfig;

   procedure parseLoad (fileHandle : in File_Type; loadArray : out loadArray_T; loadValid : out Boolean; numLoadValues : out Integer) is
      i           : Integer;
      paramValid  : Boolean;
   begin
      numLoadValues := 0; -- global, from psu_simulation
      loadValid := True;
      Put_Line ("Parsing electric_load file...");
      loop
         declare
            Line        : constant String := Get_Line (fileHandle);
            timeString  : constant String := Line (Line'First .. Index (Line, " ") - 1);
            valueString : constant String := Line (Index (Line, " ") + 1 .. Line'Last);
         begin
            i := Index (Line, " ");
            if (Line'Length = 0) then
               null;
            else
               if ((Line (1) /= '-') and (i > 1 and i < Line'Last)) then
                  paramValid := True;
                  begin --  Timestamp and load value need to be at least 0. Checks needed to generate exceptions when Strings cannot be parsed to float
                     if (Float'Value (timeString) < 0.0) then -- do some useless stuff, but needed to check if valid. Provokes exception if invalid
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load timestamp " & timeString & " is not >= 0");
                        paramValid := False;
                     end if;
                  exception
                     when Constraint_Error =>
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load value: float expected, but found: " & timeString);
                        paramValid := False;
                  end;
                  begin
                     if (Float'Value (valueString) < 0.0) then -- do some useless stuff, but needed to check if valid. Provokes exception if invalid
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load value " & valueString & " is not >= 0");
                        paramValid := False;
                     end if;
                  exception
                     when Constraint_Error =>
                        Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load value at timestamp " & timeString & " invalid: " & valueString);
                        paramValid := False;
                  end;

                  if (paramValid) then
                     numLoadValues := numLoadValues + 1;
                     if (numLoadValues > maxLoadValues) then
                        Put_Line ("[ " & vt100_YELLOW & "WARN " & vt100_RESET & "]   Simulation currently has hard-coded limit of " & maxLoadValues'Image & " load values, but load definition file has more than " & maxLoadValues'Image & " valid entries. Will simulate the first " & maxLoadValues'Image & " load values only!");
                        numLoadValues := maxLoadValues;
                        exit;
                     end if;
                     loadArray (numLoadValues, 1) := Float'Value (timeString);
                     loadArray (numLoadValues, 2) := Float'Value (valueString);
                     Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Load at time " & loadArray (numLoadValues, 1)'Image & " set to " & loadArray  (numLoadValues, 2)'Image);
                  else loadValid := False;
                     Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load definition is invalid!");
                     exit; -- optional. But we don't get to see further config errors then
                  end if;
               end if;
            end if;
         end;
         if (End_Of_File (fileHandle)) then
            Put_Line ("[ " & vt100_GREEN & "OK " & vt100_RESET & "]     Load definition is valid, contains " & numLoadValues'Image & " values!");
            exit;
         end if;
      end loop;
   exception
      when others =>
         Put_Line ("[ " & vt100_RED & "ERROR " & vt100_RESET & "]  Load definition is invalid!");
   end parseLoad;

end CONFIG_Parser;
