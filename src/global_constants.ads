package global_constants is

   subtype Time_Dilation_Factor_T is Float range 1.0 .. 1.0e6;

   --  Enable vt100 definitions to use colors in prompt
   vt100_RESET        : String :=  ASCII.ESC & "[0m";
   vt100_RED          : String :=   ASCII.ESC & "[31m";
   vt100_GREEN        : String :=  ASCII.ESC & "[32m";
   vt100_YELLOW       : String :=  ASCII.ESC & "[33m";
   vt100_CYAN       : String :=  ASCII.ESC & "[36m";
   --  vt100_BLACK        : String :=  ASCII.ESC & "[30m";
   --  vt100_BLUE         : String :=   ASCII.ESC & "[34m";
   --  vt100_MAGENTA      : String := ASCII.ESC & "[35m";
   --  vt100_CYAN         : String :=   ASCII.ESC & "[36m";
   --  vt100_WHITE        : String :=  ASCII.ESC & "[37m";

   --  Factor used to slow down tasking
   --  The simulation is not able to run in in real time (would be a few ns)
   RT_MUL             : Time_Dilation_Factor_T := 1.0e2;
   RT_MUL_S2MS        : Float := 1.0e3 * RT_MUL;
   RT_MUL_S2US        : Float := 1.0e3 * RT_MUL_S2MS;

end global_constants;
