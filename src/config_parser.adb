-- Unbounded Strings handling inspired by https://en.wikibooks.org/wiki/Ada_Programming/Strings
pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;

package body CONFIG_Parser is

   package SU   renames Ada.Strings.Unbounded;

   procedure parseConfig (fileHandle : in File_Type; PID_Config : in out PID_Config_A_T; Sim_Config : in out Sim_Config_T; configValid : out Boolean) is

   i : Integer := 0;
   paramString : SU.Unbounded_String;
    valueString : SU.Unbounded_String;
    type ConfigParams is (PID_U_C1_Kp, PID_U_C1_Ki, PID_U_C1_Kd, PID_U_C1_Sp, PID_U_C1_Sn, PID_U_C1_T,
			  PID_U_C2_Kp, PID_U_C2_Ki, PID_U_C2_Kd, PID_U_C2_Sp, PID_U_C2_Sn, PID_U_C2_T,
			  PID_I_L1_Kp, PID_I_L1_Ki, PID_I_L1_Kd, PID_I_L1_Sp, PID_I_L1_Sn, PID_I_L1_T,
			  PID_I_L2_Kp, PID_I_L2_Ki, PID_I_L2_Kd, PID_I_L2_Sp, PID_I_L2_Sn, PID_I_L2_T,
			  Sim_L1, Sim_C1, Sim_L2, Sim_C2, Sim_f_V1, Sim_Up_V1, Sim_T);
    type paramConfigured_T is array ( ConfigParams ) of Boolean;
    paramConfigured : paramConfigured_T;
    paramValid : Boolean;

  begin
    paramConfigured := (others => false);
    configValid := true;
      loop
         declare
            Line : String := Get_Line (fileHandle);
         begin
	i := Index (Line, " ");
	if (Line'Length = 0) then
	  null;
	else
	    if ((Line(1) /= '-') and (i > 1 and i < Line'Last)) then
              paramString := SU.To_Unbounded_String(Line(Line'First..i-1));
              valueString := SU.To_Unbounded_String(Line(i+1..Line'Last));
	      paramValid := true;
	      begin
	      	if(ConfigParams'Value (SU.To_String (paramString))'Image = " ") then -- do some useless stuff, but needed to check if valid. Provokes exception if invalid
	          		null; --Put_Line (ConfigParams'Value (SU.To_String (paramString))'Image);
	          	end if;
	      exception
	      	when Constraint_Error =>
	          		Put_Line (Standard_Error, "ERROR: Config parameter unknown: " & SU.To_String (paramString));
	          		paramValid := false;
	      end;
	      begin
	       	if(Float'Value(SU.To_String(valueString)) > 0.0) then -- do some useless stuff, but needed to check if valid. Provokes exception if invalid
	          		null;
	          	end if;
	      exception
	      	when Constraint_Error =>
	          		Put_Line (Standard_Error,  "ERROR: " & SU.To_String (paramString) & " config value invalid: " & SU.To_String (valueString));
	          		paramValid := false;
	      end;

	      if(paramValid = true) then
	        --Put_Line (ConfigParams'Value (SU.To_String (paramString))'Image);
	        case ConfigParams'Value (SU.To_String (paramString)) is
	          -- Parse C1
                      when PID_U_C1_Kp =>
                         	PID_Config(PID_U_C1).Kp := Float'Value(SU.To_String(valueString));
                         	Put_Line("[ OK ] PID_U_C1 Kp set to " & Float'Image(PID_Config(PID_U_C1).Kp));
                      when PID_U_C1_Ki =>
                         	PID_Config(PID_U_C1).Ki := Float'Value(SU.To_String(valueString));
	    	     	Put_Line ("[ OK ] PID_U_C1 Ki set to " & Float'Image (PID_Config (PID_U_C1).Ki));
	    	  when PID_U_C1_Kd =>
                           	PID_Config(PID_U_C1).Kd := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C1 Kd set to " & Float'Image (PID_Config (PID_U_C1).Kd));
	    	  when PID_U_C1_Sp =>
                           	PID_Config(PID_U_C1).Sp := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C1 Sp set to " & Float'Image (PID_Config (PID_U_C1).Sp));
	    	  when PID_U_C1_Sn =>
                           	PID_Config(PID_U_C1).Sn := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C1 Sn set to " & Float'Image (PID_Config (PID_U_C1).Sn));
	    	  when PID_U_C1_T =>
                           	PID_Config(PID_U_C1).T := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C1 T  set to " & Float'Image (PID_Config (PID_U_C1).T));
	          -- Parse C2
	    	  when PID_U_C2_Kp =>
                         	PID_Config(PID_U_C2).Kp := Float'Value(SU.To_String(valueString));
                         	Put_Line("[ OK ] PID_U_C2 Kp set to " & Float'Image(PID_Config(PID_U_C2).Kp));
                      when PID_U_C2_Ki =>
                         	PID_Config(PID_U_C2).Ki := Float'Value(SU.To_String(valueString));
	    	     	Put_Line ("[ OK ] PID_U_C2 Ki set to " & Float'Image (PID_Config (PID_U_C2).Ki));
	    	  when PID_U_C2_Kd =>
                           	PID_Config(PID_U_C2).Kd := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C2 Kd set to " & Float'Image (PID_Config (PID_U_C2).Kd));
	    	  when PID_U_C2_Sp =>
                           	PID_Config(PID_U_C2).Sp := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C2 Sp set to " & Float'Image (PID_Config (PID_U_C2).Sp));
	    	  when PID_U_C2_Sn =>
                           	PID_Config(PID_U_C2).Sn := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C2 Sn set to " & Float'Image (PID_Config (PID_U_C2).Sn));
	    	  when PID_U_C2_T =>
                           	PID_Config(PID_U_C2).T := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_U_C2 T  set to " & Float'Image (PID_Config (PID_U_C2).T));
	    	-- Parse L1
	    	  when PID_I_L1_Kp =>
                         	PID_Config(PID_I_L1).Kp := Float'Value(SU.To_String(valueString));
                         	Put_Line("[ OK ] PID_I_L1 Kp set to " & Float'Image(PID_Config(PID_I_L1).Kp));
                      when PID_I_L1_Ki =>
                         	PID_Config(PID_I_L1).Ki := Float'Value(SU.To_String(valueString));
	    	     	Put_Line ("[ OK ] PID_I_L1 Ki set to " & Float'Image (PID_Config (PID_I_L1).Ki));
	    	  when PID_I_L1_Kd =>
                           	PID_Config(PID_I_L1).Kd := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L1 Kd set to " & Float'Image (PID_Config (PID_I_L1).Kd));
	    	  when PID_I_L1_Sp =>
                           	PID_Config(PID_I_L1).Sp := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L1 Sp set to " & Float'Image (PID_Config (PID_I_L1).Sp));
	    	  when PID_I_L1_Sn =>
                           	PID_Config(PID_I_L1).Sn := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L1 Sn set to " & Float'Image (PID_Config (PID_I_L1).Sn));
	    	  when PID_I_L1_T =>
                           	PID_Config(PID_I_L1).T := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L1 T  set to " & Float'Image (PID_Config (PID_I_L1).T));
	          -- Parse L2
	    	  when PID_I_L2_Kp =>
                         	PID_Config(PID_I_L2).Kp := Float'Value(SU.To_String(valueString));
                         	Put_Line("[ OK ] PID_I_L2 Kp set to " & Float'Image(PID_Config(PID_I_L2).Kp));
                      when PID_I_L2_Ki =>
                         	PID_Config(PID_I_L2).Ki := Float'Value(SU.To_String(valueString));
	    	     	Put_Line ("[ OK ] PID_I_L2 Ki set to " & Float'Image (PID_Config (PID_I_L2).Ki));
	    	  when PID_I_L2_Kd =>
                           	PID_Config(PID_I_L2).Kd := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L2 Kd set to " & Float'Image (PID_Config (PID_I_L2).Kd));
	    	  when PID_I_L2_Sp =>
                           	PID_Config(PID_I_L2).Sp := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L2 Sp set to " & Float'Image (PID_Config (PID_I_L2).Sp));
	    	  when PID_I_L2_Sn =>
                           	PID_Config(PID_I_L2).Sn := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L2 Sn set to " & Float'Image (PID_Config (PID_I_L2).Sn));
	    	  when PID_I_L2_T =>
                           	PID_Config(PID_I_L2).T := Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] PID_I_L2 T  set to " & Float'Image (PID_Config (PID_I_L2).T));
	          -- Parse Simulation config
	    	  when Sim_L1 =>
                         	Sim_Config.L1:= Float'Value(SU.To_String(valueString));
                         	Put_Line("[ OK ] Sim      L1 set to " & Float'Image(Sim_Config.L1));
                      when Sim_C1 =>
                         	Sim_Config.C1:= Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] Sim      C1 set to " & Float'Image (Sim_Config.C1));
	    	  when Sim_L2 =>
                         	Sim_Config.L2:= Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] Sim      L2 set to " & Float'Image (Sim_Config.L2));
	    	  when Sim_C2 =>
                         	Sim_Config.C2:= Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] Sim      C2 set to " & Float'Image (Sim_Config.C2));
	    	  when Sim_f_V1 =>
                         	Sim_Config.f_V1:= Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] Sim    f_V1 set to " & Float'Image (Sim_Config.f_V1));
	    	  when Sim_Up_V1 =>
                         	Sim_Config.Up_V1:= Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] Sim   Up_V1 set to " & Float'Image (Sim_Config.Up_V1));
	    	  when Sim_T =>
                         	Sim_Config.T:= Float'Value(SU.To_String(valueString));
	    		Put_Line ("[ OK ] Sim      T  set to " & Float'Image (Sim_Config.T));
	              when others =>
	    		Put_Line ("ERROR: unknown config item: " & SU.To_String (paramString));
                   end case;
	        paramConfigured (ConfigParams'Value (SU.To_String (paramString))) := true;
              end if;
            end if;
	end if;
	end;
      end loop;
      exception
         when End_Error =>
      		for i in paramConfigured'Range loop
      			if (paramConfigured(i) = false) then
	  Put_Line ("ERROR: parameter not configured: " & ConfigParams (i)'Image);
	  configValid := false;
	     		end if;
      		end loop;
  end parseConfig;

  procedure parseLoad (fileHandle : in File_Type; loadArray : out loadArray_T; loadValid : out Boolean) is
   i : Integer;
    timeString : SU.Unbounded_String;
    valueString : SU.Unbounded_String;
    paramValid  : Boolean;

  begin
    numLoadValues := 0; -- global, from psu_simulation
    loadValid := true;
    loop
         declare
            Line : String := Get_Line (fileHandle);
         begin
	i := Index (Line, " ");
	if (Line'Length = 0) then
	  null;
	else
	    if ((Line(1) /= '-') and (i > 1 and i < Line'Last)) then
              timeString := SU.To_Unbounded_String(Line(Line'First..i-1));
              valueString := SU.To_Unbounded_String(Line(i+1..Line'Last));
	      paramValid := true;
	      begin
	      	if(Float'Value (SU.To_String (timeString)) = 0.0) then -- do some useless stuff, but needed to check if valid. Provokes exception if invalid
	          		null;
	          	end if;
	      exception
	      	when Constraint_Error =>
	          		Put_Line (Standard_Error, "ERROR: Float expected, but found: " & SU.To_String (timeString));
	          		paramValid := false;
	      end;
	      begin
	       	if(Float'Value(SU.To_String(valueString)) > 0.0) then -- do some useless stuff, but needed to check if valid. Provokes exception if invalid
	          		null;
	          	end if;
	      exception
	      	when Constraint_Error =>
	          		Put_Line (Standard_Error,  "ERROR: timestamp " & SU.To_String (timeString) & " load value invalid: " & SU.To_String (valueString));
	          		paramValid := false;
	      end;

	    if (paramValid = true) then
	      numLoadValues := numLoadValues + 1;
	      if (numLoadValues > maxLoadValues) then
		Put_Line ("Simulation currently has hard-coded limit of " & maxLoadValues'image & " load values, but load definition file has more than " & maxLoadValues'Image & " valid entries. Will simulate the first " & maxLoadValues'Image & " load values only!");
		numLoadValues := maxLoadValues;
		return;
	      end if;
	      loadArray (numLoadValues, 1) := Float'Value (SU.To_String (timeString));
	      loadArray (numLoadValues, 2) := Float'Value (SU.To_String (valueString));
	      Put_Line ("[ OK ] Load at time " & loadArray (numLoadValues, 1)'Image & " set to " & loadArray(numLoadValues, 2)'Image);
	    else loadValid := false;
	      return;
	    end if;
	  end if;
	end if;
	end;
    end loop;

      exception
         when End_Error =>
      null; --Put_Line ("Reached end of valid load definition file.");
  end parseLoad;

end CONFIG_Parser;
