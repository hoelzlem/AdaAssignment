# RTPL Lab WS 2017/18 Group 15
**Power supply simulation**
* Markus Hölzle, 3693070
* Simon Diehl, 3692790
* Alexander Lange, 3637632

Welcome to the introduction for the power supply simulation written in Ada/SPARK for the RTPL Lab.
The code runs from the terminal either on Linux or Windows, and has no extra dependencies.


# Idea behind the project

Our group decided to implement a simulation of a power supply, which would typically be connected to an AC power supply, for example 230V, 50Hz. The output voltage and load can be specified in configuration files, and the program simulates the power-up sequence as well as the steady state behavior. 
SPARK mode is used to implement a monitor, which checks whether all voltages and currents are within a tolerated range.
Additionally, a logger module stores all voltages and currents in an output file for visualization in MATLAB, to see the stability of the control loop and the maximum voltages/currents that may be experienced by the respective components.

# Contributions
The application contains the following modules:
* main - starts all sub-processes, handles the user inputs - written in colaboration
* config_parser - parses the configuration files for the simulation, load and monitor - written by Alexander
* psu_control - 4 control loops set the duty cycles within the circuit - written by Markus
* psu_simulation - computes all voltages and currents, similar to LTSpice - written by Markus
* psu_monitoring - SPARK, monitors the thresholds of the control loops - written by Simon
* psu_logging - periodically samples all voltages and currents, and writes them to a log file - written by Simon

Additional files:
* Circuit.png shows the circuit and component names - designed by Markus 
* MATLAB script to view output log - written by Alexander
* This documentation - written by Alexander

# Running the application
The application runs purely from the terminal, without any graphical interface. Since there are many configuration options for the circuit, its simulation and the monitoring thresholds, different configuration files are read by the program during start.
The simulation automatically starts when all configurations are valid. When the last timestamp of the load has been reached, the load is disconnected, irrespective of the provided load value. This simulates a power supply without load until the application is aborted with Ctrl+C.

## User inputs

The main loop asks the user for 4 file paths. Either the default values can be taken by entering 'y' or an individual path can be specified for each file. The default paths are as follows:
* ./simConfig.txt
* ./monitorConfig.txt
* ./loadConfig.txt
* ./output.txt

All values given in the example files must be set. When using a different configuration file, make sure to set all values, otherwise the simulation will abort after parsing the file!
The load configuration has a float timestamp in seconds, and resistance values that are set at the respective time after program start. The timestamps can be set to any time, though there is a hard-coded limit of 1000 entries.

The parser supports comment lines in all configuration files. Any line starting with a '-' is skipped. Configuration lines start with the respective parameter, a space, and the respective float value. Scientific notation is supported. Only exception is the 'mode' parameter of the monitor, which accepts the strings "mean_based" or "threshold_based". 
> -- this is a comment line
> parameter value

## Output file

The logger module stores data to the specified output file for visualization with MATLAB in the last step. It is a .txt file with CSV format, where the first line includes the header. Usage of the output file is shown with the MATLAB script.

# Visualization in MATLAB

The supplied script visualizePowerSupply.m plots all parameters, and can be used to analyze the stability of the control loops in the power supply by inspecting the output voltage.