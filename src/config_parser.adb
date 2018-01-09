pragma Profile (Ravenscar);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with PSU_Simulation; use PSU_Simulation;
with PSU_Control; use PSU_Control;

package body CONFIG_Parser is

   package SU   renames Ada.Strings.Unbounded;

   procedure parseConfig (fileHandle : in File_Type; Ctrl_Conf : in out Ctrl_Config_T) is
   -- Unbounded Strings handling inspired by https://en.wikibooks.org/wiki/Ada_Programming/Strings

   i : Integer := 0;
   paramString : SU.Unbounded_String;
   valueString : SU.Unbounded_String;

   begin
      loop
         declare
            Line : String := Get_Line (fileHandle);
         begin
            i := Index(Line," ");
            if(i > 1 and i < Line'Last) then
               paramString := SU.To_Unbounded_String(Line(Line'First..i-1));
               valueString := SU.To_Unbounded_String(Line(i+1..Line'Last));
               if(SU.To_String(paramString) = "Kp") then
                  Ctrl_Conf.PFC_Config.Kp := Float'Value(SU.To_String(valueString));
                  Put_Line("Kp set to " & Float'Image(Ctrl_Conf.PFC_Config.Kp));
               end if;
               if(SU.To_String(paramString) = "Tn") then
                  Ctrl_Conf.PFC_Config.Tn := Float'Value(SU.To_String(valueString));
                  Put_Line("Tn set to " & Float'Image(Ctrl_Conf.PFC_Config.Tn));
               end if;
               if(SU.To_String(paramString) = "Tv") then
                  Ctrl_Conf.PFC_Config.Tv := Float'Value(SU.To_String(valueString));
                  Put_Line("Tv set to " & Float'Image(Ctrl_Conf.PFC_Config.Tv));
               end if;
               if(SU.To_String(paramString) = "T") then
                  Ctrl_Conf.PFC_Config.T := Float'Value(SU.To_String(valueString));
                  Put_Line("T set to " & Float'Image(Ctrl_Conf.PFC_Config.T));
               end if;
            end if;
         end;
      end loop;
      exception
         when End_Error =>
            Put_Line("parseConfig finished!");
   end parseConfig;

end CONFIG_Parser;
