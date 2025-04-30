clearvars;
clc;
% Open Tethys batch file and set Matlab path
%% Initiate database amd set query handler
query_h = dbInit('Server', 'localhost');
%% Test connection to database - get deployments
DeploymentInfo = dbGetDeployments(query_h);
% should return a new item in the workspace called DeploymentInfo with a
% list of all of the deployments and associated metadata 

%% Run XQuery to update the database
dbRunQueryFile(query_h, "Z:/ANALYSIS/Tethys/MatlabClient/db/xqueries/swfsc_update.xq");