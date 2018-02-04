pragma Profile (Ravenscar);
pragma SPARK_Mode;

with global_constants;

package body PSU_Monitoring is

   protected body Monitoring_Interface_T is

      function is_all_config_set return Boolean is
      begin
         return supervisor_config_set and monitor_pfc_voltage_config_set
           and monitor_pfc_current_config_set and monitor_output_voltage_config_set
           and monitor_output_current_config_set;
      end is_all_config_set;

      procedure set_supervisor_config (new_supervisor_config : in Supervisor_Config_T) is
      begin
         supervisor_config := new_supervisor_config;
         supervisor_config_set := True;
      end set_supervisor_config;

      function get_supervisor_config return Supervisor_Config_T is
      begin
         return supervisor_config;
      end get_supervisor_config;

      procedure set_monitor_pfc_voltage_config (new_monitor_config : in Monitor_Config_T) is
      begin
         monitor_pfc_voltage_config := new_monitor_config;
         monitor_pfc_voltage_config_set := True;
      end set_monitor_pfc_voltage_config;

      function get_monitor_pfc_voltage_config return Monitor_Config_T is
      begin
         return monitor_pfc_voltage_config;
      end get_monitor_pfc_voltage_config;

      procedure set_monitor_pfc_current_config (new_monitor_config : in Monitor_Config_T) is
      begin
         monitor_pfc_current_config := new_monitor_config;
         monitor_pfc_current_config_set := True;
      end set_monitor_pfc_current_config;

      function get_monitor_pfc_current_config return Monitor_Config_T is
      begin
         return monitor_pfc_current_config;
      end get_monitor_pfc_current_config;

      procedure set_monitor_output_voltage_config (new_monitor_config : in Monitor_Config_T) is
      begin
         monitor_output_voltage_config := new_monitor_config;
         monitor_output_voltage_config_set := True;
      end set_monitor_output_voltage_config;

      function get_monitor_output_voltage_config return Monitor_Config_T is
      begin
         return monitor_output_voltage_config;
      end get_monitor_output_voltage_config;

      procedure set_monitor_output_current_config (new_monitor_config : in Monitor_Config_T) is
      begin
         monitor_output_current_config := new_monitor_config;
         monitor_output_current_config_set := True;
      end set_monitor_output_current_config;

      function get_monitor_output_current_config return Monitor_Config_T is
      begin
         return monitor_output_current_config;
      end get_monitor_output_current_config;

   end Monitoring_Interface_T;

   function is_within_limits (monitor : in Monitor_T; signal_value : in Float_Signed1000) return Boolean is
      within_limits : Boolean := False;
   begin
      case monitor.config.monitoring_mode is
         when mean_based =>
            if abs (monitor.config.mean - signal_value) <= monitor.config.maximum_deviation then
               within_limits := True;
            end if;

         when threshold_based =>
            if signal_value in monitor.config.lower_threshold .. monitor.config.upper_threshold then
                  within_limits := True;
            end if;
      end case;

      return within_limits;

   end is_within_limits;

   function expand_threshold (threshold : in Float_Signed1000; expansion_factor : Expansion_Factor_T; T : Threshold_T) return Float_Signed10000 is
      expanded_threshold : Float_Signed10000;
   begin
      case T is
         when lower =>
            if threshold >= 0.0 then
               expanded_threshold := threshold / expansion_factor;
            else
               expanded_threshold := threshold * expansion_factor;
            end if;

         when upper =>
            if threshold >= 0.0 then
               expanded_threshold := threshold * expansion_factor;
            else
               expanded_threshold := threshold / expansion_factor;
            end if;
      end case;

      return expanded_threshold;

   end expand_threshold;

   function is_within_expanded_limits (monitor : in Monitor_T; signal_value : in Float_Signed1000) return Boolean is
      within_expanded_limits : Boolean := False;

      expanded_lower_threshold : Float_Signed10000;
      expanded_upper_threshold : Float_Signed10000;
   begin
      case monitor.config.monitoring_mode is
         when mean_based =>
            if abs (monitor.config.mean - signal_value) <= (monitor.config.maximum_deviation * monitor.config.settling_tolerance_expansion) then
               within_expanded_limits := True;
            end if;

         when threshold_based =>
            expanded_lower_threshold := expand_threshold (monitor.config.lower_threshold, monitor.config.settling_tolerance_expansion, lower);
            expanded_upper_threshold := expand_threshold (monitor.config.upper_threshold, monitor.config.settling_tolerance_expansion, upper);

            --  @M. Becker - This assertion is troublesome and couldn't be proved without using annotations yet.
            --  The lemma library might help but time was short in the end.
            pragma Assert (expanded_lower_threshold < expanded_upper_threshold);

            --  Check limits with expanded thresholds
            if signal_value >= expanded_lower_threshold and signal_value <= expanded_upper_threshold then
               within_expanded_limits := True;
            end if;
      end case;

      return within_expanded_limits;

   end is_within_expanded_limits;

   procedure monitor_signal (monitor : in out Monitor_T; signal_value : in Float_Signed1000) is
   begin
      --  Update FSM state
      monitor.current_state := monitor.next_state;
      --  Perform FSM actions and determine next state
      case monitor.current_state is
         when reset =>
            monitor.next_state := startup;
            monitor.timer := Milliseconds (0);

         when startup =>
            if is_within_limits (monitor, signal_value) then
               monitor.next_state := settling;
               monitor.timer := Milliseconds (0);
            elsif monitor.timer >= monitor.config.startup_time then
               monitor.next_state := shutdown;
               monitor.timer := Milliseconds (0);
            else
               monitor.next_state := startup;
               monitor.timer := monitor.timer + TASK_PERIOD;
            end if;

         when settling =>
            if not is_within_expanded_limits (monitor, signal_value) then
               monitor.next_state := shutdown;
               monitor.timer := Milliseconds (0);
            elsif monitor.timer >= monitor.config.settling_time then
               monitor.next_state := active;
            else
               monitor.next_state := settling;
               monitor.timer := monitor.timer + TASK_PERIOD;
            end if;

         when active =>
            if not is_within_limits (monitor, signal_value) then
               monitor.next_state := alert;
               monitor.timer := Milliseconds (0);
            else
               monitor.next_state := active;
            end if;

         when alert =>
            if is_within_limits (monitor, signal_value) then
               monitor.next_state := settling;
               monitor.timer := Milliseconds (0);
            elsif monitor.timer >= monitor.config.violation_time then
               monitor.next_state := shutdown;
               monitor.timer := Milliseconds (0);
            else
               monitor.next_state := alert;
               monitor.timer := monitor.timer + TASK_PERIOD;
            end if;

         when shutdown =>
            monitor.next_state := shutdown;
      end case;
   end monitor_signal;

   procedure do_monitoring is
      U_C1 : constant Float := Sim.Get_U_C1;
      I_L1 : constant Float := Sim.Get_I_L1;
      U_C2 : constant Float := Sim.Get_U_C2;
      I_L2 : constant Float := Sim.Get_I_L2;
   begin
      pragma Assert (U_C1 in Float_Signed1000);
      pragma Assert (I_L1 in Float_Signed1000);
      pragma Assert (U_C2 in Float_Signed1000);
      pragma Assert (I_L2 in Float_Signed1000);
      --  Monitor PFC intermediate voltage
      monitor_signal (monitor_pfc_voltage, U_C1);
      --  Monitor PFC inductor current
      monitor_signal (monitor_pfc_current, I_L1);
      --  Monitor output voltage
      monitor_signal (monitor_output_voltage, U_C2);
      --  Monitor output inductor current
      monitor_signal (monitor_output_current, I_L2);
   end do_monitoring;

   procedure do_supervision is
   begin
      --  Check if monitors and supervisor are configured and ready to use. This procedure can't be executed properly otherwise.
      declare
         all_config_is_set : constant Boolean := monitoring_interface.is_all_config_set;
      begin
         pragma Assert (all_config_is_set);
         pragma Annotate (GNATprove, False_Positive, "assertion might fail",
                         "The assertion can't fail because this procedure is only called by the task if is_all_config_set returns True");
      end;

      --  Update FSM state
      supervisor.current_state := supervisor.next_state;
      --  Perform FSM actions and determine next state
      case supervisor.current_state is
         when reset =>
            --  Deactivate power stages
            Ctrl.Set_Safety_State (False);
            pragma Annotate (GNATprove, False_Positive, "input value of ""Ctrl"" will be used", "Ctrl is only used as an output here");

            supervisor.next_state := active;

         when active =>
            --  Activate power stages
            Ctrl.Set_Safety_State (True);
            pragma Annotate (GNATprove, False_Positive, "input value of ""Ctrl"" will be used", "Ctrl is only used as an output here");
            --  Perform monitoring
            do_monitoring;

            --  Check if either of the monitors is in state shutdown
            if monitor_pfc_voltage.current_state = shutdown or monitor_pfc_current.current_state = shutdown
              or monitor_output_voltage.current_state = shutdown or monitor_output_current.current_state = shutdown
            then
               --  At least one state variable violated its limits => shutdown required
               supervisor.next_state := shutdown;
               supervisor.timer := Milliseconds (0);
            else
               supervisor.next_state := active;
            end if;

         when shutdown =>
            --  Deactivate power stages
            Ctrl.Set_Safety_State (False);
            pragma Annotate (GNATprove, False_Positive, "input value of ""Ctrl"" will be used", "Ctrl is only used as an output here");
            --  Reset monitors
            monitor_pfc_voltage.current_state := reset;
            monitor_pfc_voltage.next_state := reset;
            monitor_pfc_current.current_state := reset;
            monitor_pfc_current.next_state := reset;
            monitor_output_voltage.current_state := reset;
            monitor_output_voltage.next_state := reset;
            monitor_output_current.current_state := reset;
            monitor_output_current.next_state := reset;

            if supervisor.timer >= supervisor.config.retry_time then
               supervisor.next_state := active;
               supervisor.timer := Milliseconds (0);
            else
               supervisor.next_state := shutdown;
               supervisor.timer := supervisor.timer + TASK_PERIOD;
            end if;
      end case;
   end do_supervision;

   task body monitoring_task is
      STRECHED_TASK_PERIOD : constant Time_Span := TASK_PERIOD * Integer (global_constants.RT_MUL);
      next_time : Time := Clock;
   begin
      loop
         --  Fetch supervisor configuration
         supervisor.config := monitoring_interface.get_supervisor_config;
         --  Fetch monitor configuration
         monitor_pfc_voltage.config := monitoring_interface.get_monitor_pfc_voltage_config;
         monitor_pfc_current.config := monitoring_interface.get_monitor_pfc_current_config;
         monitor_output_voltage.config := monitoring_interface.get_monitor_output_voltage_config;
         monitor_output_current.config := monitoring_interface.get_monitor_output_current_config;

         declare
            --  See https://goo.gl/NBXa7y
            all_config_is_set : constant Boolean := monitoring_interface.is_all_config_set;
         begin
            if all_config_is_set then
               do_supervision;
            end if;
         end;

         next_time := next_time + STRECHED_TASK_PERIOD;
         delay until next_time;
      end loop;
   end monitoring_task;

end PSU_Monitoring;
