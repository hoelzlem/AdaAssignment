pragma Profile (Ravenscar);
pragma SPARK_Mode;

--  See https://goo.gl/QQ8HAA
pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time; use Ada.Real_Time;
with System;

package PSU_Monitoring is

   type Monitoring_Mode_T is (mean_based, threshold_based);
   type Monitor_State_T is (reset, startup, settling, active, alert, shutdown);

   subtype Float_Natural1000 is Float range 0.0 .. 1_000.0;
   subtype Float_Positive10 is Float range 1.0 .. 10.0;
   subtype Float_Signed1000 is Float range -1_000.0 .. 1_000.0;

   type Monitor_Config_T is record
      monitoring_mode : Monitoring_Mode_T := mean_based;

      mean : Float_Signed1000 := 0.0;
      maximum_deviation : Float_Natural1000 := 0.0;
      lower_threshold : Float_Signed1000 := 0.0;
      upper_threshold : Float_Signed1000 := 0.0;

      settling_tolerance_expansion : Float_Positive10 := 1.2;

      startup_time : Time_Span := Milliseconds (5);
      settling_time : Time_Span := Milliseconds (2);
      violation_time : Time_Span := Milliseconds (5);
      retry_time : Time_Span := Milliseconds (100);
   end record;

   type Monitor_T is record
      config : Monitor_Config_T;
      timer : Time_Span := Milliseconds (0);
      current_state : Monitor_State_T := reset;
      next_state : Monitor_State_T := reset;
   end record;

   protected type Monitoring_Interface_T is
      function is_all_config_set return Boolean;

      procedure set_monitor_pfc_voltage_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_pfc_voltage_config return Monitor_Config_T;

      procedure set_monitor_pfc_current_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_pfc_current_config return Monitor_Config_T;

      procedure set_monitor_output_voltage_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_output_voltage_config return Monitor_Config_T;

      procedure set_monitor_output_current_config (new_monitor_config : in Monitor_Config_T);
      function get_monitor_output_current_config return Monitor_Config_T;
   private
      --  Configuration for PFC intermediate voltage
      monitor_pfc_voltage_config : Monitor_Config_T;
      monitor_pfc_voltage_config_set : Boolean := False;
      --  Configuration for PFC inductor current
      monitor_pfc_current_config : Monitor_Config_T;
      monitor_pfc_current_config_set : Boolean := False;
      --  Configuration for output voltage
      monitor_output_voltage_config : Monitor_Config_T;
      monitor_output_voltage_config_set : Boolean := False;
      --  Configuration for output inductor current
      monitor_output_current_config : Monitor_Config_T;
      monitor_output_current_config_set : Boolean := False;
   end Monitoring_Interface_T;

   monitoring_interface : Monitoring_Interface_T;

private
   task monitoring_task is
      pragma Priority (System.Priority'First);
   end monitoring_task;

   TASK_PERIOD : constant Time_Span := Milliseconds (100);

   monitor_pfc_voltage : Monitor_T;
   monitor_pfc_current : Monitor_T;
   monitor_output_voltage : Monitor_T;
   monitor_output_current : Monitor_T;

   --  @TODO remove this when the version with contracts works
   procedure do_monitoring;

   --  @TODO get this precondition to work
   --  the problem here is that the prover is neither able to evaluate private members of monitoring_interface nor its getter functions
   --procedure do_monitoring with
   --   Pre => monitoring_interface.monitor_pfc_voltage_config_set and then monitoring_interface.monitor_pfc_current_config_set and then monitoring_interface.monitor_output_voltage_config_set and then monitoring_interface.monitor_output_current_config_set;

   --  @TODO all of the following contracts are not reported as proved. Why?!

   procedure monitor_signal (monitor : in out Monitor_T; signal_value : in Float) with
      Post => monitor.next_state = reset and monitor.timer < Milliseconds (200) and monitor.timer /= monitor.timer'Old;

   function is_within_limits (monitor : in Monitor_T; signal_value : in Float) return Boolean with
      Pre => (if monitor.config.monitoring_mode = mean_based then monitor.config.maximum_deviation /= 0.0) and then (if monitor.config.monitoring_mode = threshold_based then monitor.config.lower_threshold < monitor.config.upper_threshold),
      Contract_Cases => (monitor.config.monitoring_mode = mean_based => (if is_within_limits'Result then (abs (monitor.config.mean - signal_value) <= monitor.config.maximum_deviation)),
                         monitor.config.monitoring_mode = threshold_based => (if is_within_limits'Result then (signal_value >= monitor.config.lower_threshold) and then (signal_value <= monitor.config.upper_threshold)));

   --  @TODO add case for monitoring_mode = threshold_based
   --  I didn't add that yet because it is really complicated and terrible to read. I put an assertion into the function until then.
   function is_within_expanded_limits (monitor : in Monitor_T; signal_value : in Float) return Boolean with
      Pre => (if monitor.config.monitoring_mode = mean_based then monitor.config.maximum_deviation /= 0.0) and then (if monitor.config.monitoring_mode = threshold_based then monitor.config.lower_threshold < monitor.config.upper_threshold) and then monitor.config.settling_tolerance_expansion > 1.1,
      Contract_Cases => (monitor.config.monitoring_mode = mean_based => (if is_within_expanded_limits'Result then (abs (monitor.config.mean - signal_value) <= (monitor.config.maximum_deviation * monitor.config.settling_tolerance_expansion))));

end PSU_Monitoring;
