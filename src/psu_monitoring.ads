--  The powersupply has multiple controlled state variables; two currents and two voltages.
--  Its controller is not failsafe and especially capacitors are prone to damage from overvoltage.
--  This package provides a SPARK proven, independent monitor which deactivates the power stages
--  when certain limits on the state variables are violated.
--  The monitoring fuction is implemented with two finite state machines. Four monitors that observe one
--  state variable each and report undesirable behaviour to a supervisor that handles activation and shutdown of the power stages.
--  The package operates like so:
--    1. the package starts a task independent from the controller and awaits configuration via the monitoring interface
--    2. supervisor and monitors are configurated and ready to start operation
--    3. the supervisor FSM leaves reset state and activates all monitors and the power stages
--    4. the monitors wait for the state variables to settle
--    5. the monitors report to the supervisor if a state variable violates the specified limits for a given time
--    6. the supervisor goes into shutdown and deactivates the power stages
--    7. the supervisor waits for a certain time and restarts the power supply
--    8. the supervisor leaves shutdown state and activates all monitors and the power stages
--    9. continue with 4.

--  @M. Becker - I was not able to find an elegant way to satisfy the preconditions for is_within_limits and is_within_expanded_limits.
--  The condition can't be fulfilled by constraining the datatypes for lower_threshold < upper_threshold further because that would limit
--  the threshold_based mode too much. I'm currently relying on the runtime checks (-gnata) of the preconditions. Thanks also for your help
--  on stackoverflow, I wouldn't have gotten so far without your advice. ~ Simon

pragma Profile (Ravenscar);
pragma SPARK_Mode;

--  See https://goo.gl/QQ8HAA
pragma Partition_Elaboration_Policy (Sequential);
pragma Assertion_Policy (Pre => Check);

with Ada.Real_Time; use Ada.Real_Time;
with System;

with PSU_Control; use PSU_Control;
with PSU_Simulation; use PSU_Simulation;

package PSU_Monitoring is

   type Threshold_T is (lower, upper);
   type Monitoring_Mode_T is (mean_based, threshold_based);
   type Monitor_State_T is (reset, startup, settling, active, alert, shutdown);
   type Supervisor_State_T is (reset, active, shutdown);

   subtype Expansion_Factor_T is Float range 1.1 .. 2.0;
   subtype Maximum_Deviation_T is Float range 50.0e-3 .. 10.0;
   subtype Float_Signed1000 is Float range -1_000.0 .. 1_000.0;
   subtype Float_Signed10000 is Float range -10_000.0 .. 10_000.0;

   type Monitor_Config_T is record
      --  The monitor can work based on two kinds of limit specification:
      --    * mean_based: You specify a desired value for the monitored signal (typically equal to the controllers setpoint)
      --      and a maximum allowed deviation from that value. The monitor will signal an alert if the maximum deviation is exceeded.
      --    * threshold_based: You specify minimum and maximum values for the monitored signal. The monitor will signal an alert
      --      if either of the limts is exceeded.
      monitoring_mode : Monitoring_Mode_T := mean_based;

      mean : Float_Signed1000 := 0.0;
      maximum_deviation : Maximum_Deviation_T := 100.0e-3;
      lower_threshold : Float_Signed1000 := -100.0e-3;
      upper_threshold : Float_Signed1000 := 100.0e-3;
      --  Typical controller designs allow for some overshoot to make the controller faster. This is typical behaviour during controller
      --  startup an must not trip the monitor. The monitor will allow higher deviation, by widening the tolerance band spanned by mean
      --  and deviation or lower and upper threshold, from the desired value by this factor in either of both monitoring modes.
      settling_tolerance_expansion : Expansion_Factor_T := 1.4;
      --  Maximum time the monitored signal is allowed to be outside the nominal tolerance band during monitors startup state
      startup_time   : Time_Span := Milliseconds (15);
      --  The monitor remains in state settling for this duration
      settling_time  : Time_Span := Milliseconds (5);
      --  Maximum time the monitored signal is allowed be outside of the nominal tolerance band before shutdown is enforced
      violation_time : Time_Span := Milliseconds (5);
   end record;

   type Supervisor_Config_T is record
      --  After enforcing a shutdown, the supervisor waits for this time before trying a restart
      retry_time     : Time_Span := Milliseconds (100);
   end record;

   type Monitor_T is record
      --  The monitors configuration is passed by a call to the monitoring interface
      config        : Monitor_Config_T;
      --  The FSM has some time controlled transitions  and needs a timer to keep track of the time spend in the current state. The timer
      --  is only incremented in states with outgoing time controlled transitions.
      timer         : Time_Span := Milliseconds (0);
      --  FSM states; current and next state are separated to make the code easier to understand
      current_state : Monitor_State_T := reset;
      next_state    : Monitor_State_T := reset;
   end record;

   type Supervisor_T is record
      --  The supervisors configuration is passed by a call to the monitoring interface
      config : Supervisor_Config_T;
      --  The FSM has some time controlled transitions  and needs a timer to keep track of the time spend in the current state. The timer
      --  is only incremented in states with outgoing time controlled transitions.
      timer : Time_Span := Milliseconds (0);
      --  FSM states; current and next state are separated to make the code easier to understand
      current_state : Supervisor_State_T := reset;
      next_state : Supervisor_State_T := reset;
   end record;

   --  This protected interface is used for inter task safe configuration of supervisor and monitors. The user must call each setter function
   --  at least once or the monitoring task won't run. The getter functions are used by the monitoring task to retrieve the user defined
   --  configuration and is not meant to be used by other tasks.
   protected type Monitoring_Interface_T is
      --  The monitoring task is only allowed to run after supervisor and all monitors have been configured properly. This function is used
      --  to determine whether all configurations were written at least once.
      function is_all_config_set return Boolean;
      --  Was one of the monitors not configured with a valid configuration structure?
      function is_config_erroneous return Boolean;

      procedure set_supervisor_config (new_supervisor_config : in Supervisor_Config_T);
      function get_supervisor_config return Supervisor_Config_T;

      procedure set_monitor_pfc_voltage_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_pfc_voltage_config return Monitor_Config_T;

      procedure set_monitor_pfc_current_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_pfc_current_config return Monitor_Config_T;

      procedure set_monitor_output_voltage_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_output_voltage_config return Monitor_Config_T;

      procedure set_monitor_output_current_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_output_current_config return Monitor_Config_T;
   private
      --  Erroneous configuration
      --  One of the monitors was not configured correctly
      config_error : Boolean := False;
      --  Configuration for supervisor
      supervisor_config : Supervisor_Config_T;
      supervisor_config_set : Boolean := False;
      --  Configuration for PFC intermediate voltage
      monitor_pfc_voltage_config        : Monitor_Config_T;
      monitor_pfc_voltage_config_set    : Boolean := False;
      --  Configuration for PFC inductor current
      monitor_pfc_current_config        : Monitor_Config_T;
      monitor_pfc_current_config_set    : Boolean := False;
      --  Configuration for output voltage
      monitor_output_voltage_config     : Monitor_Config_T;
      monitor_output_voltage_config_set : Boolean := False;
      --  Configuration for output inductor current
      monitor_output_current_config     : Monitor_Config_T;
      monitor_output_current_config_set : Boolean := False;
   end Monitoring_Interface_T;

   monitoring_interface : Monitoring_Interface_T;

private
   task monitoring_task is
      pragma Priority (System.Priority'Last);
   end monitoring_task;

   TASK_PERIOD : constant Time_Span := Microseconds (100);

   supervisor : Supervisor_T;
   monitor_pfc_voltage    : Monitor_T;
   monitor_pfc_current    : Monitor_T;
   monitor_output_voltage : Monitor_T;
   monitor_output_current : Monitor_T;

   --  Check if monitor configuration is valid
   --  This function is required to solve the precondition problem with is_within_limits and is_within_expanded_limits.
   function is_monitor_config_valid (monitor_config : in Monitor_Config_T) return Boolean
      with Global => null,
         Depends => (is_monitor_config_valid'Result => monitor_config);

   --  Execute one step of the supervisor:
   --    1. Update current state with old next state
   --    2. Execute the actions associated with the current state
   --    3. Determine the next state
   procedure do_supervision
      with Global => (Input => (monitoring_interface, Sim),
                      In_Out => (supervisor, monitor_pfc_voltage, monitor_pfc_current, monitor_output_voltage, monitor_output_current),
                      Output =>  Ctrl),
         Depends => (supervisor =>+ (monitor_pfc_voltage, monitor_pfc_current, monitor_output_voltage, monitor_output_current, Sim),
                     (monitor_pfc_voltage, monitor_pfc_current, monitor_output_voltage, monitor_output_current) =>+ (supervisor, Sim),
                     Ctrl => supervisor,
                     null => monitoring_interface);

   --  Execute one step of all monitors
   procedure do_monitoring
      with Global => (Input => Sim,
                      In_Out => (monitor_pfc_voltage, monitor_pfc_current, monitor_output_voltage, monitor_output_current)),
         Depends => (monitor_pfc_voltage =>+ Sim,
                     monitor_pfc_current =>+ Sim,
                     monitor_output_voltage =>+ Sim,
                     monitor_output_current =>+ Sim);

   --  Execute one step of a monitor:
   --    1. Update current state with old next state
   --    2. Execute the actions associated with the current state
   --    3. Determine the next state
   procedure monitor_signal (monitor : in out Monitor_T; signal_value : in Float_Signed1000)
      with Global => null,
         Depends => (monitor =>+ signal_value);

   --  Checks whether a signal is within the nominal tolerance band of its associated monitor
   function is_within_limits (monitor : in Monitor_T; signal_value : in Float_Signed1000) return Boolean
     with Pre => ((if monitor.config.monitoring_mode = mean_based then monitor.config.maximum_deviation > 0.0)
                  and then (if monitor.config.monitoring_mode = threshold_based then monitor.config.lower_threshold < monitor.config.upper_threshold)),
         Global => null,
         Depends => (is_within_limits'Result => (monitor, signal_value));

   --  Calculate in which value a nominal threshold results for the expanded tolerance band.
   --  This function was solely introduced to be able to add postconditions to tackle this problem https://goo.gl/TWWQ86. It did not solve the problem,
   --  since the postconditions could not be proved, but made adding a proper annotation sensible.
   function expand_threshold (threshold : in Float_Signed1000; expansion_factor : Expansion_Factor_T; T : Threshold_T) return Float_Signed10000
      with Pre => (expansion_factor > 1.0),
         Contract_Cases => ((T = lower) => (expand_threshold'Result <= threshold),
                            (T = upper) => (expand_threshold'Result >= threshold)),
         Global => null,
         Depends => (expand_threshold'Result => (threshold, expansion_factor, T));
      pragma Annotate (GNATprove, False_Positive, "contract case might fail", "checked by Simon");

   --  Checks whether a signal is within the expanded tolerance band of its associated monitor
   function is_within_expanded_limits (monitor : in Monitor_T; signal_value : in Float_Signed1000) return Boolean
     with Pre => ((if monitor.config.monitoring_mode = mean_based then monitor.config.maximum_deviation > 0.0)
                  and then (if monitor.config.monitoring_mode = threshold_based then monitor.config.lower_threshold < monitor.config.upper_threshold)
                  and then monitor.config.settling_tolerance_expansion > 1.0),
         Global => null,
         Depends => (is_within_expanded_limits'Result => (monitor, signal_value));

end PSU_Monitoring;
