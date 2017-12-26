with PSU_Simulation;
use PSU_Simulation;
package body PSU_Control is

  task body PSU_Control_T is
  begin
    sim.setPfcDutycycle (0.5);
  end PSU_Control_T;

end PSU_Control;
