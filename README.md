# Power Supply Simulation

This project is our entry for the second programming assignment for the course *Real-Time-Programming-Languages* at TU Munich for the winter semester 2017 / 18.

**Participants:**
* Simon Diehl (3692790)
* Markus Hölzle (3693070)
* Alexander Lange (3637632)

# Idea behind the project

Part of the course are two assignments. One on the synchronous programming language *Esterel* and one on the general purpose programming language *Ada*. We were asked to choose a software project that we should implement using Ada.

Being electrical engineers, our group decided to implement some of the software required for a simple, computer controlled power supply.

The following figure shows the schematic of a generic power supply.

![Schematic](Schematic.png)

The purpose of the power supply is to take a high AC voltage from the grid (source V1; eg. 230 V~ with 50 Hz) and convert it to a more usable, stable DC voltage. It consists of two power stages. The first stage is a power factor correction unit (bridge rectifier, PFC choke L1, switch M1 and diode D5) with an intermediate DC voltage circuit on its output (capacitor C1). The second stage is a forward converter (switches M2 .. M5, transformer build from L3 and L4, output bridge rectifier, buck choke L4) that takes the voltage from the intermediate circuit and converts it into a much lower, isolated and stable DC voltage (capacitor C2). A resistive load is connected to the power supply.

Imagine a microcomputer that measures voltages and currents, does some computations and generates the PWM signals required by the five switches. We implemented the hardware independent software aspects namely controllers, voltage and current monitoring and data logging. A simple model of the power supply is used to simulate its behaviour. Our programm uses Ada's language features and the Ravenscar profile to start multiple tasks in parallel. The tasks are controllers, monitoring, logging and simulation. SPARK mode was also required for the assignment. We used it to verify absence of runtime errors within the monitoring module which could be condidered crucial for safety of machinery.

 Output voltage and load sequence can be specified in configuration files.

# Project structure

Description of relevant files:
* ./ - Base directory
   * simConfig.txt - example configuration file for simulation and controllers
   * monitorConfig.txt - example configuration file for monitoring
   * loadConfig.txt - example definition of load sequence
   * log_visualisation.m - Matlab script that plots the content of the log file
* ./src - Ada source files
   * main - handles user's inputs and configures other modules
   * global constants - some constants that are required by multiple modules
   * config_parser - parses the configuration files
   * psu_control - control loops that compute the required duty cycles
   * psu_simulation - simulates the power supply; computes all voltages and currents based on an averaged model for all switched signals
   * psu_monitoring - monitors state variables; proven with SPARK
   * psu_logging - periodically samples all voltages and currents; writes them to a log file

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

## Visualisation in Matlab

The supplied script plots all simulation variables. The simulations ouptut is not really human readable otherwise.