package PSU_Simulation is

  type Config_T is record 
    pfcInductance  : Float := 1.0;
    pfcCapacity    : Float := 1.0;
    buckInductance : Float := 1.0;
    buckCapacity   : Float := 1.0;
    mainsFrequency : Float := 1.0;
    mainsAmplitude : Float := 1.0;
  end record;
   
  type SimOutput_T is record
    outputVoltage : Float := 0.0;
    outputCurrent : Float := 0.0;
    inputCurrent  : Float := 0.0;
    buckCurrent   : Float := 0.0;
    dcBusVoltage  : Float := 0.0;
    mainsVoltage  : Float := 0.0;
  end record;
   
  protected type Simulation_I_T is
    entry getConfig (Conf : in out Config_T);
    procedure setConfig (Conf : in Config_T);
    function getMainsVoltage return Float;
    function getDcBusVoltage return Float;    
    function getOutputVoltage return Float;
    function getOutputCurrent return Float;
    function getPfcDutycycle return Float;
    function getBuckDutycycle return Float;
    procedure setPfcDutycycle (Val : in Float);
    procedure setOutputDutycycle (Val : in Float);
    procedure setSimulationOutput (Val : in SimOutput_T);
  private
    simulationOutput : SimOutput_T;
    buckDutycycle    : Float := 0.0;
    pfcDutycycle     : Float := 0.0;
    configuration    : Config_T;
    configured       : Boolean;
  end Simulation_I_T;
  sim : Simulation_I_T;
   
  task type Simulation_T is
  end Simulation_T;
      
private
  config : Config_T;
  values : SimOutput_T;
   
end PSU_Simulation;
