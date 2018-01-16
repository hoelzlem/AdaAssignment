pragma Profile(Ravenscar);
pragma SPARK_Mode;

with Ada.Text_IO; use Ada.Text_IO;

with PSU_Simulation; use PSU_Simulation;

package body PSU_Monitoring is

protected body Monitoring_Interface_T is

    function is_all_config_set return Boolean is
    begin
        return monitor_pfc_voltage_config_set and
               monitor_pfc_current_config_set and
               monitor_output_voltage_config_set and
               monitor_output_current_config_set;
    end is_all_config_set;

    procedure set_monitor_pfc_voltage_config(new_monitor_config : in Monitor_Config_T) is
    begin
        monitor_pfc_voltage_config := new_monitor_config;
        monitor_pfc_voltage_config_set := True;
    end set_monitor_pfc_voltage_config;

    function get_monitor_pfc_voltage_config return Monitor_Config_T is
    begin
        return monitor_pfc_voltage_config;
    end get_monitor_pfc_voltage_config;

    procedure set_monitor_pfc_current_config(new_monitor_config : in Monitor_Config_T) is
    begin
        monitor_pfc_current_config := new_monitor_config;
        monitor_pfc_current_config_set := True;
    end set_monitor_pfc_current_config;

    function get_monitor_pfc_current_config return Monitor_Config_T is
    begin
        return monitor_pfc_current_config;
    end get_monitor_pfc_current_config;

    procedure set_monitor_output_voltage_config(new_monitor_config : in Monitor_Config_T) is
    begin
        monitor_output_voltage_config := new_monitor_config;
        monitor_output_voltage_config_set := True;
    end set_monitor_output_voltage_config;

    function get_monitor_output_voltage_config return Monitor_Config_T is
    begin
        return monitor_output_voltage_config;
    end get_monitor_output_voltage_config;

    procedure set_monitor_output_current_config(new_monitor_config : in Monitor_Config_T) is
    begin
        monitor_output_current_config := new_monitor_config;
        monitor_output_current_config_set := True;
    end set_monitor_output_current_config;

    function get_monitor_output_current_config return Monitor_Config_T is
    begin
        return monitor_output_current_config;
    end get_monitor_output_current_config;

end Monitoring_Interface_T;

task body Monitoring_Task_T is
    next_time : Time;

begin
    -- Initialisation of next execution time
    next_time := Clock;
    -- Superloop
    loop
        -- Load monitor configuration
        monitor_pfc_voltage.config := monitoring_interface.get_monitor_pfc_voltage_config;
        monitor_pfc_current.config := monitoring_interface.get_monitor_pfc_current_config;
        monitor_output_voltage.config := monitoring_interface.get_monitor_output_voltage_config;
        monitor_output_current.config := monitoring_interface.get_monitor_output_current_config;
        -- Check if module has been configured correctly
        -- Don't do anything otherwise
        if monitoring_interface.is_all_config_set = True then
            do_monitoring;
        end if;

        next_time := next_time + TASK_PERIOD;
        delay until next_Time;
    end loop;
end Monitoring_Task_T;

procedure do_monitoring is
begin    
    -- Monitor PFC intermediate voltage
    monitor_signal(monitor_pfc_voltage, Sim.Get_U_C1);
    -- Monitor PFC inductor current
    monitor_signal(monitor_pfc_current, Sim.Get_I_L1);
    -- Monitor output voltage
    monitor_signal(monitor_output_voltage, Sim.Get_U_C2);
    -- Monitor output inductor current
    monitor_signal(monitor_output_current, Sim.Get_I_L2);    
end do_monitoring;

procedure monitor_signal(monitor : in out Monitor_T; signal_value : in Float) is
begin
    -- @TODO add some code that leaves some error margin for controller startup! Otherwise the controller would be overriden as soon as the monitoring tasks starts since the signals will always violate the limits during startup. Maybe some kind of timer that is reset when the signals stabilise within the limits. After shutting down the controller the monitoring system should allow at least one retry.

    -- Update monitor state
    monitor.current_state := monitor.next_state;

    case monitor.current_state is
        when startup =>
            -- @TODO
            null;
        when settling =>
            -- @TODO
            null;
        when active =>
            if is_within_limits(monitor, signal_value) = False then
                -- @TODO call a function from controller module that disables the power stage
                monitor.next_state := alert;
            else
                monitor.next_state := active;
            end if;
        when alert =>
            -- @TODO
            null;
        when shutdown =>
            -- @TODO
            null;
    end case;
end monitor_signal;

function is_within_limits(monitor : in Monitor_T; signal_value : in Float) return Boolean is
    within_limits : Boolean := False;
begin
    case monitor.config.monitoring_mode is
        when mean_based =>
            if abs(monitor.config.mean - signal_value) < monitor.config.maximum_deviation then
                within_limits := True;
            end if;
        when threshold_based =>
            if signal_value >= monitor.config.lower_threshold or signal_value <= monitor.config.upper_threshold then
                within_limits := True;
            end if;
    end case;

    return within_limits;
end is_within_limits;
   
end PSU_Monitoring;
