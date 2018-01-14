pragma Profile(Ravenscar);
pragma SPARK_Mode;

package PSU_Monitoring is

    type Monitoring_Mode_T is (mean_based, threshold_based);
    
    subtype Float_Natural1000 is Float range 0.0..1000.0;
    subtype Float_Signed1000 is Float range -1000.0..1000.0;

    type Monitor_Config_T is record
        monitoring_mode : Monitoring_Mode_T := mean_based;
        mean : Float_Signed1000 := 0.0;
        maximum_deviation : Float_Natural1000 := 0.0;
        lower_threshold : Float_Signed1000 := 0.0;
        upper_threshold : Float_Signed1000 := 0.0;
    end record;

    protected type Monitoring_Interface_T is
        function is_all_config_set return Boolean;

        procedure set_monitor_pfc_voltage_config(new_config : in Monitor_Config_T);
        function get_monitor_pfc_voltage_config return Monitor_Config_T;
        
        procedure set_monitor_pfc_current_config(new_config : in Monitor_Config_T);
        function get_monitor_pfc_current_config return Monitor_Config_T;
        
        procedure set_monitor_output_voltage_config(new_config : in Monitor_Config_T);
        function get_monitor_output_voltage_config return Monitor_Config_T;
        
        procedure set_monitor_output_current_config(new_config : in Monitor_Config_T);
        function get_monitor_output_current_config return Monitor_Config_T;
    private
        -- Configuration for PFC intermediate voltage
        monitor_pfc_voltage_config : Monitor_Config_T;
        monitor_pfc_voltage_config_set : Boolean := False;
        -- Configuration for PFC inductor current
        monitor_pfc_current_config : Monitor_Config_T;
        monitor_pfc_current_config_set : Boolean := False;
        -- Configuration for output voltage
        monitor_output_voltage_config : Monitor_Config_T;
        monitor_output_voltage_config_set : Boolean := False;
        -- Configuration for output inductor current
        monitor_output_current_config : Monitor_Config_T;
        monitor_output_current_config_set : Boolean := False;
    end Monitoring_Interface_T;

    monitoring_interface : Monitoring_Interface_T;
   
private
    task type Monitoring_Task_T;

    function is_within_limits(config : in Monitor_Config_T; signal_value : in Float) return Boolean;

end PSU_Monitoring;
