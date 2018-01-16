pragma Profile(Ravenscar);
pragma SPARK_Mode;

with PSU_Simulation; use PSU_Simulation;

package body PSU_Monitoring is

protected body Monitoring_Interface_T is

    function is_all_monitors_set return Boolean is
    begin
        return monitor_pfc_voltage_set and
               monitor_pfc_current_set and
               monitor_output_voltage_set and
               monitor_output_current_set;
    end is_all_monitors_set;

    procedure set_monitor_pfc_voltage(new_monitor : in Monitor_T) is
    begin
        monitor_pfc_voltage := new_monitor;
        monitor_pfc_voltage_set := True;
    end set_monitor_pfc_voltage;

    function get_monitor_pfc_voltage return Monitor_T is
    begin
        return monitor_pfc_voltage;
    end get_monitor_pfc_voltage;

    procedure set_monitor_pfc_current(new_monitor : in Monitor_T) is
    begin
        monitor_pfc_current := new_monitor;
        monitor_pfc_current_set := True;
    end set_monitor_pfc_current;

    function get_monitor_pfc_current return Monitor_T is
    begin
        return monitor_pfc_current;
    end get_monitor_pfc_current;

    procedure set_monitor_output_voltage(new_monitor : in Monitor_T) is
    begin
        monitor_output_voltage := new_monitor;
        monitor_output_voltage_set := True;
    end set_monitor_output_voltage;

    function get_monitor_output_voltage return Monitor_T is
    begin
        return monitor_output_voltage;
    end get_monitor_output_voltage;

    procedure set_monitor_output_current(new_monitor : in Monitor_T) is
    begin
        monitor_output_current := new_monitor;
        monitor_output_current_set := True;
    end set_monitor_output_current;

    function get_monitor_output_current return Monitor_T is
    begin
        return monitor_output_current;
    end get_monitor_output_current;

end Monitoring_Interface_T;

task body Monitoring_Task_T is
    TASK_PERIOD : constant Time_Span := Milliseconds(1);
    next_time : Time;
begin
    -- Initialisation of next execution time
    next_time := Clock;
    -- Superloop
    loop
        -- Check if module has been configured correctly
        -- Don't do anything otherwise
        if monitoring_interface.is_all_monitors_set = True then
            do_monitoring;
        end if;

        next_time := next_time + TASK_PERIOD;
        delay until next_Time;
    end loop;
end Monitoring_Task_T;

monitoring_task : Monitoring_Task_T;

procedure do_monitoring is
begin
    -- Check monitored signals against configured values
    -- @TODO add some code that leaves some error margin for controller startup! Otherwise the controller would be overriden as soon as the monitoring tasks starts since the signals will always violate the limits during startup. Maybe some kind of timer that is reset when the signals stabilise within the limits. After shutting down the controller the monitoring system should allow at least one retry.
    -- Check PFC intermediate voltage
    if is_within_limits(monitoring_interface.get_monitor_pfc_voltage, Sim.Get_U_C1) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
    -- Check PFC inductor current
    if is_within_limits(monitoring_interface.get_monitor_pfc_current, Sim.Get_I_L1) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
    -- Check output voltage
    if is_within_limits(monitoring_interface.get_monitor_output_voltage, Sim.Get_U_C2) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
    -- Check output inductor current
    if is_within_limits(monitoring_interface.get_monitor_output_current, Sim.Get_I_L2) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
end do_monitoring;

function is_within_limits(monitor : in Monitor_T; signal_value : in Float) return Boolean is
    within_limits : Boolean := False;
begin
    case monitor.monitoring_mode is
        when mean_based =>
            if abs(monitor.mean - signal_value) < monitor.maximum_deviation then
                within_limits := True;
            end if;
        when threshold_based =>
            if signal_value >= monitor.lower_threshold or signal_value <= monitor.upper_threshold then
                within_limits := True;
            end if;
    end case;

    return within_limits;
end is_within_limits;
   
end PSU_Monitoring;
