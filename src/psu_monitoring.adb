pragma Profile(Ravenscar);
pragma SPARK_Mode;

with Ada.Real_Time; use Ada.Real_Time;

with PSU_Simulation; use PSU_Simulation;

package body PSU_Monitoring is

protected body Monitoring_Interface_T is

    function is_all_config_set return Boolean is
    begin
        return monitor_pfc_voltage_config_set and monitor_pfc_current_config_set and monitor_output_voltage_config_set and monitor_output_current_config_set;
    end is_all_config_set;

    procedure set_monitor_pfc_voltage_config(new_config : in Monitor_Config_T) is
    begin
        monitor_pfc_voltage_config := new_config;
        monitor_pfc_voltage_config_set := True;
    end set_monitor_pfc_voltage_config;

    function get_monitor_pfc_voltage_config return Monitor_Config_T is
    begin
        return monitor_pfc_voltage_config;
    end get_monitor_pfc_voltage_config;

    procedure set_monitor_pfc_current_config(new_config : in Monitor_Config_T) is
    begin
        monitor_pfc_current_config := new_config;
        monitor_pfc_current_config_set := True;
    end set_monitor_pfc_current_config;

    function get_monitor_pfc_current_config return Monitor_Config_T is
    begin
        return monitor_pfc_current_config;
    end get_monitor_pfc_current_config;

    procedure set_monitor_output_voltage_config(new_config : in Monitor_Config_T) is
    begin
        monitor_output_voltage_config := new_config;
        monitor_output_voltage_config_set := True;
    end set_monitor_output_voltage_config;

    function get_monitor_output_voltage_config return Monitor_Config_T is
    begin
        return monitor_output_voltage_config;
    end get_monitor_output_voltage_config;

    procedure set_monitor_output_current_config(new_config : in Monitor_Config_T) is
    begin
        monitor_output_current_config := new_config;
        monitor_output_current_config_set := True;
    end set_monitor_output_current_config;

    function get_monitor_output_current_config return Monitor_Config_T is
    begin
        return monitor_output_current_config;
    end get_monitor_output_current_config;

end Monitoring_Interface_T;

task body Monitoring_Task_T is
    PERIOD : constant Time_Span := Milliseconds(1);
    next_time : Time;
begin
    -- Initialisation of next execution time
    next_time := Clock;
    -- Superloop
    loop
        -- Check if module has been configured correctly
        -- Don't do anything otherwise
        if monitoring_interface.is_all_config_set = True then
            do_monitoring;
        end if;

        next_time := next_time + PERIOD;
        delay until next_Time;
    end loop;
end Monitoring_Task_T;

monitoring_task : Monitoring_Task_T;

procedure do_monitoring is
begin
    -- Check monitored signals against configured values
    -- @TODO add some code that leaves some error margin for controller startup! Otherwise the controller would be overriden as soon as the monitoring tasks starts since the signals will always violate the limits during startup. Maybe some kind of timer that is reset when the signals stabilise within the limits. After shutting down the controller the monitoring system should allow at least one retry.
    -- Check PFC intermediate voltage
    if is_within_limits(monitoring_interface.get_monitor_pfc_voltage_config, Sim.Get_U_C1) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
    -- Check PFC inductor current
    if is_within_limits(monitoring_interface.get_monitor_pfc_current_config, Sim.Get_I_L1) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
    -- Check output voltage
    if is_within_limits(monitoring_interface.get_monitor_output_voltage_config, Sim.Get_U_C2) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
    -- Check output inductor current
    if is_within_limits(monitoring_interface.get_monitor_output_current_config, Sim.Get_I_L2) = False then
        -- @TODO call a function from controller module that disables the power stage
        null;
    end if;
end do_monitoring;

function is_within_limits(monitor_config : in Monitor_Config_T; signal_value : in Float) return Boolean is
    within_limits : Boolean := False;
begin
    case monitor_config.monitoring_mode is
        when mean_based =>
            if abs(monitor_config.mean - signal_value) < monitor_config.maximum_deviation then
                within_limits := True;
            end if;
        when threshold_based =>
            if signal_value >= monitor_config.lower_threshold or signal_value <= monitor_config.upper_threshold then
                within_limits := True;
            end if;
    end case;

    return within_limits;
end is_within_limits;
   
end PSU_Monitoring;
