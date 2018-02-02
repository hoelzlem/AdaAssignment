--  The powersupply has multiple controlled state variables. Two currents and two voltages. The controller is not failsafe and especially capacitors are prone to damage from overvoltage.
--  This package provides a SPARK proven, independent monitor which deactivates the power stages when certain limits on the state variables are violated.
--  The monitoring fuction is implemented with two finite state machines. Four monitors that observe one state variable each and report undesirable behaviour to a supervisor that handles activation and shutdown of the power stages.
--  The package operates like so:
--  1. the package startes a controller independent task and awaits configuration via the monitoring interface
--  2. supervisor and monitors are configurated and ready to start operation
--  3. the supervisor FSM leaves reset state and activates all monitors and the power stages
--  4. the monitors wait for the state variables to settle
--  5. the monitors report to the supervisor if a state variable violates the specified limits for a given time
--  6. the supervisor goes into shutdown and deactivates the power stages
--  7. the supervisor waits for a certain time and restarts the power supply

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

   type Monitoring_Mode_T is (mean_based, threshold_based);
   type Monitor_State_T is (reset, startup, settling, active, alert, shutdown);
   type Supervisor_State_T is (reset, active, shutdown);

   subtype Float_Signed1000 is Float range -1_000.0 .. 1_000.0;
   subtype Float_Signed10000 is Float range -10_000.0 .. 10_000.0;

   type Monitor_Config_T is record
      monitoring_mode : Monitoring_Mode_T := mean_based;

      mean : Float_Signed1000 := 0.0;
      maximum_deviation : Float range Float'Small .. 1_000.0 := 100.0e-3;
      lower_threshold : Float_Signed1000 := -100.0e-3;
      upper_threshold : Float_Signed1000 := 100.0e-3;

      settling_tolerance_expansion : Float range (1.0 + Float'Small) .. 2.0 := 1.2;

      startup_time   : Time_Span := Milliseconds (5);
      settling_time  : Time_Span := Milliseconds (2);
      violation_time : Time_Span := Milliseconds (5);
   end record;

   type Supervisor_Config_T is record
      retry_time     : Time_Span := Milliseconds (1000);
   end record;

   type Monitor_T is record
      config        : Monitor_Config_T;
      timer         : Time_Span := Milliseconds (0);
      current_state : Monitor_State_T := reset;
      next_state    : Monitor_State_T := reset;
   end record;

   type Supervisor_T is record
      config : Supervisor_Config_T;
      timer : Time_Span := Milliseconds (0);
      current_state : Supervisor_State_T := reset;
      next_state : Supervisor_State_T := reset;
   end record;

   protected type Monitoring_Interface_T is
      function is_all_config_set return Boolean;

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

   TASK_PERIOD : constant Time_Span := Milliseconds (100);

   supervisor : Supervisor_T;
   monitor_pfc_voltage    : Monitor_T;
   monitor_pfc_current    : Monitor_T;
   monitor_output_voltage : Monitor_T;
   monitor_output_current : Monitor_T;

   --  @TODO all of the current contracts are proved as "might fail". solve that!

   procedure do_supervision
      with Global => (Input => (monitoring_interface, Sim),
                      In_Out => (supervisor, monitor_pfc_voltage, monitor_pfc_current, monitor_output_voltage, monitor_output_current),
                      Output =>  Ctrl),
         Depends => (supervisor =>+ (monitor_pfc_voltage, monitor_pfc_current, monitor_output_voltage, monitor_output_current, Sim),
                     monitor_pfc_voltage =>+ (supervisor, Sim),
                     monitor_pfc_current =>+ (supervisor, Sim),
                     monitor_output_voltage =>+ (supervisor, Sim),
                     monitor_output_current =>+ (supervisor, Sim),
                     Ctrl => supervisor,
                     null => monitoring_interface);

   procedure do_monitoring
      with Global => (Input => Sim,
                      In_Out => (monitor_pfc_voltage, monitor_pfc_current, monitor_output_voltage, monitor_output_current)),
         Depends => (monitor_pfc_voltage =>+ Sim,
                     monitor_pfc_current =>+ Sim,
                     monitor_output_voltage =>+ Sim,
                     monitor_output_current =>+ Sim);

   procedure monitor_signal (monitor : in out Monitor_T; signal_value : in Float_Signed1000)
      with Global => null,
         Depends => (monitor =>+ signal_value);

   function is_within_limits (monitor : in Monitor_T; signal_value : in Float_Signed1000) return Boolean
      with Pre => ((if monitor.config.monitoring_mode = mean_based then monitor.config.maximum_deviation > 0.0) and then (if monitor.config.monitoring_mode = threshold_based then monitor.config.lower_threshold < monitor.config.upper_threshold)),
         Global => null,
         Depends => (is_within_limits'Result => (monitor, signal_value)) ;

   function is_within_expanded_limits (monitor : in Monitor_T; signal_value : in Float_Signed1000) return Boolean
      with Pre => ((if monitor.config.monitoring_mode = mean_based then monitor.config.maximum_deviation > 0.0) and then (if monitor.config.monitoring_mode = threshold_based then monitor.config.lower_threshold < monitor.config.upper_threshold) and then monitor.config.settling_tolerance_expansion > 1.0),
         Global => null,
         Depends => (is_within_expanded_limits'Result => (monitor, signal_value));

end PSU_Monitoring;
