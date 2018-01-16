pragma Profile(Ravenscar);
pragma SPARK_Mode;

with Ada.Real_Time; use Ada.Real_Time;

package PSU_Monitoring is

    type Monitoring_Mode_T is (mean_based, threshold_based);
    type Monitor_State_T is (startup, settling, active, shutdown);
    
    subtype Float_Natural1000 is Float range 0.0..1000.0;
    subtype Float_Signed1000 is Float range -1000.0..1000.0;

    type Monitor_T is record
        monitoring_mode : Monitoring_Mode_T := mean_based;
        mean : Float_Signed1000 := 0.0;
        maximum_deviation : Float_Natural1000 := 0.0;
        lower_threshold : Float_Signed1000 := 0.0;
        upper_threshold : Float_Signed1000 := 0.0;
        startup_time : Time_Span := Milliseconds(5);
        settling_time : Time_Span := Milliseconds(2);
        violation_time : Time_Span := Milliseconds(5);
        retry_time : Time_Span := Milliseconds(100);
        timer : Time_Span := Milliseconds(0);
        state : Monitor_State_T := startup;
    end record;

    protected type Monitoring_Interface_T is
        function is_all_monitors_set return Boolean;

        procedure set_monitor_pfc_voltage(new_monitor : in Monitor_T);
        function get_monitor_pfc_voltage return Monitor_T;
        
        procedure set_monitor_pfc_current(new_monitor : in Monitor_T);
        function get_monitor_pfc_current return Monitor_T;
        
        procedure set_monitor_output_voltage(new_monitor : in Monitor_T);
        function get_monitor_output_voltage return Monitor_T;
        
        procedure set_monitor_output_current(new_monitor : in Monitor_T);
        function get_monitor_output_current return Monitor_T;
    private
        -- Configuration for PFC intermediate voltage
        monitor_pfc_voltage : Monitor_T;
        monitor_pfc_voltage_set : Boolean := False;
        -- Configuration for PFC inductor current
        monitor_pfc_current : Monitor_T;
        monitor_pfc_current_set : Boolean := False;
        -- Configuration for output voltage
        monitor_output_voltage : Monitor_T;
        monitor_output_voltage_set : Boolean := False;
        -- Configuration for output inductor current
        monitor_output_current : Monitor_T;
        monitor_output_current_set : Boolean := False;
    end Monitoring_Interface_T;

    monitoring_interface : Monitoring_Interface_T;
   
private
    task type Monitoring_Task_T;

    procedure do_monitoring;
    procedure monitor_signal(monitor : in Monitor_T; signal_value : in Float);
    function is_within_limits(monitor : in Monitor_T; signal_value : in Float) return Boolean;

end PSU_Monitoring;
