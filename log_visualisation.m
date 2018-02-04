logfile = 'sim_output.csv';

logdata = readtable(logfile);

figure()
subplot(3,1,1)
plot(logdata.T, logdata.U_V1, logdata.T, logdata.U_C1, logdata.T, logdata.U_C2, 'LineWidth', 2)
legend('Input voltage', 'DC bus voltage', 'Load voltage');
title('Voltages');
grid();
xlabel('Time in s', 'fontweight','bold','fontsize',16);
ylabel('Voltage in V', 'fontweight','bold','fontsize',16);

subplot(3,1,2)
plot(logdata.T, logdata.I_L1, logdata.T, logdata.I_L2, logdata.T, logdata.I_LOAD, 'LineWidth', 2)
legend('Input current', 'Output choke current ', 'Load current');
title('Currents');
grid();
xlabel('Time in s', 'fontweight','bold','fontsize',16);
ylabel('Current in A', 'fontweight','bold','fontsize',16);

subplot(3,1,3)
plot(logdata.T, logdata.R_LOAD, 'LineWidth', 2)
legend('Load resistance');
title('Load');
grid();
xlabel('Time in s', 'fontweight','bold','fontsize',16);
ylabel('Resistance in \Omega', 'fontweight','bold','fontsize',16);