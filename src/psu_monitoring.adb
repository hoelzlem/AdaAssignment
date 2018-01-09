pragma SPARK_Mode;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;

with PSU_Simulation;

package body PSU_Monitoring is

protected body Monitoring_Interface_T is

    function is_all_config_set return Boolean is
    begin
        return monitor_pfc_voltage_config_set and monitor_pfc_current_config_set and monitor_output_voltage_config_set and monitor_output_current_config_set;
    end is_all_config_set;

    procedure set_monitor_pfc_voltage_config(new_config : in Monitor_Config_T) is
    begin
        monitor_pfc_voltage_config := new_config;
    end set_monitor_pfc_voltage_config;

    function get_monitor_pfc_voltage_config return Monitor_Config_T is
    begin
        return monitor_pfc_voltage_config;
    end get_monitor_pfc_voltage_config;

    procedure set_monitor_pfc_current_config(new_config : in Monitor_Config_T) is
    begin
        monitor_pfc_current_config := new_config;
    end set_monitor_pfc_current_config;

    function get_monitor_pfc_current_config return Monitor_Config_T is
    begin
        return monitor_pfc_current_config;
    end get_monitor_pfc_current_config;

    procedure set_monitor_output_voltage_config(new_config : in Monitor_Config_T) is
    begin
        monitor_output_voltage_config := new_config;
    end set_monitor_output_voltage_config;

    function get_monitor_output_voltage_config return Monitor_Config_T is
    begin
        return monitor_output_voltage_config;
    end get_monitor_output_voltage_config;

    procedure set_monitor_output_current_config(new_config : in Monitor_Config_T) is
    begin
        monitor_output_current_config := new_config;
    end set_monitor_output_current_config;

    function get_monitor_output_current_config return Monitor_Config_T is
    begin
        return monitor_output_current_config;
    end get_monitor_output_current_config;

end Monitoring_Interface_T;

task body Monitoring_Task_T is
    Period : constant Time_Span := Milliseconds(1);
    Next_Time : Time;
   begin
    -- Busy wait until configuration for monitor is set
    while monitoring_interface.is_all_config_set = False loop
        null;
    end loop;
    -- Set initial next time
    Next_Time := Clock + Period;
    -- Check monitored signals against configured values
    loop
        delay until Next_Time;
        Next_Time := Next_Time + Period;
        -- Check PFC intermediate voltage

        -- Check PFC inductor current

        -- Check output voltage

        -- Check output inductor current
    end loop;
end Monitoring_Task_T;

monitoring_task : Monitoring_Task_T;
   
end PSU_Monitoring;
