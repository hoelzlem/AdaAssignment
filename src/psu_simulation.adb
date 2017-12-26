with Ada.Real_Time;
use Ada.Real_Time;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics;
--with System.Dim.Mks - Would be nice to use
package body PSU_Simulation is

  protected body Simulation_I_T is

    entry getConfig (Conf : in out Config_T) when configured is
    begin
      Conf := configuration;
    end getConfig;

    procedure setConfig (Conf : in Config_T) is
    begin
      configuration := Conf;
    end setConfig;

    function getMainsVoltage return Float is
    begin
      return simulationOutput.mainsVoltage;
    end getMainsVoltage;

    function getDcBusVoltage return Float is
    begin
      return simulationOutput.dcBusVoltage;
    end getDcBusVoltage;

    function getOutputVoltage return Float is
    begin
      return simulationOutput.outputVoltage;
    end getOutputVoltage;

    function getOutputCurrent return Float is
    begin
      return simulationOutput.outputCurrent;
    end getOutputCurrent;

    function getPfcDutycycle return Float is
    begin
      return pfcDutycycle;
    end getPfcDutycycle;

    function getBuckDutycycle return Float is
    begin
      return buckDutycycle;
    end getBuckDutycycle;

    procedure setPfcDutycycle (Val : in Float) is
    begin
      pfcDutycycle := Val;
    end setPfcDutycycle;

    procedure setOutputDutycycle (Val : in Float) is
    begin
      buckDutycycle := Val;
    end setOutputDutycycle;

    procedure setSimulationOutput (Val : in SimOutput_T) is
    begin
      simulationOutput := Val;
    end setSimulationOutput;

  end Simulation_I_T;

  SimuationTask : Simulation_T;

  task body Simulation_T is
    span_ms   : Integer := 1;
    span_s    : Float := Float (span_ms) * 1.0e-3;
    time_s    : Float := 0.0;
    load      : Float := 1.0;
    Next_Time : Time := Clock + Milliseconds (span_ms);
  begin
    loop
      values.mainsVoltage := config.mainsAmplitude *  Ada.Numerics.Elementary_Functions.Sin (time_s / config.mainsFrequency * 2.0 * Ada.Numerics.Pi);
      values.inputCurrent := values.inputCurrent + (sim.getPfcDutycycle * values.mainsVoltage - (1.0 - sim.getPfcDutycycle) * values.dcBusVoltage) * span_s / config.pfcInductance;
      values.outputCurrent := values.outputVoltage / load;
      values.buckCurrent := values.buckCurrent+(sim.getBuckDutycycle * values.dcBusVoltage -(1.0 - sim.getBuckDutycycle) * values.outputVoltage) * span_s / config.buckInductance;
      values.outputVoltage := (values.outputCurrent - values.buckCurrent) * span_s / config.buckCapacity;
      values.dcBusVoltage := (values.inputCurrent - sim.getBuckDutycycle * values.buckCurrent) * span_s / config.pfcCapacity;
      delay until Next_Time;
      -- increment time_s here
      Next_Time := Next_Time + Milliseconds (span_ms);
    end loop;
  end Simulation_T;


end PSU_Simulation;
