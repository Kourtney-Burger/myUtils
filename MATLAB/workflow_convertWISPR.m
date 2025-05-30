% WORKFLOW_CONVERTWISPR.M
%	Workflow for converting raw WISPR data into .flac files
%
%	Description:
%		This script provides the minimum steps to convert a directory (or
%		directories) of raw WISPR .dat files into a more readable format,
%		in this case .flac. The script can be modified to also convert to
%		.wav files. 
%
%       The user will need to update the path to agate and specify the
%       name/location of the configuration file. The configuration file
%       should include CONFIG.ws.inDir and CONFIG.ws.outDir to specify the
%       paths to input raw data and the target output location,
%       respectively. But, if it does not, the function will prompt to
%       select the correct folder.  
%
%       Currently, the input directory must either be a directory named
%       with the date or a directory full of subdirectories where each
%       subdirectory is named with the date using the format 'YYMMDD',
%       (e.g., 230504 for 4 May 2023). 
%
%	Notes
%
%	See also
%
%
%	Authors:
%		S. Fregosi <selene.fregosi@gmail.com> <https://github.com/sfregosi>
%
%	Updated:      06 February 2025
%
%	Created with MATLAB ver.: 24.2.0.2740171 (R2024b) Update 1
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% add agate to the path
addpath(genpath('C:\Users\kourtney.burger\Documents\MATLAB\agate\v1.1.20250423'))

% initialize agate
% this can be empty but setting CONFIG.ws.inDir and CONFIG.ws.outDir will
% streamline processing so the directories do not need to be manually
% selected
CONFIG = agate('E:\FLAC Conversion\config files\agate_config_SWFSC-WISPR-202504.cnf');

% process all the files!
convertWispr(CONFIG, 'showProgress', true, 'outExt', '.flac');
% This will print progress to the Command Window. Set 'showProgress' to
% false to skip printing. 
% This will convert files to .flac. Set 'outExt' to '.wav' to convert to WAV

% If the process is interrupted at any point, it is possible to restart at
% a specified subdirectory. WISPR typically saves raw .dat files in folders
% by date with the format 'YYMMDD', so enter the name of the dated 
% subdirectory as a 6 digit string to restart there
% convertWispr(CONFIG, 'showProgress', true, 'outExt', '.flac', ...
  %   'restartDir', '240919');