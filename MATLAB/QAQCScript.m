clearvars; close all; clc
set(0,'DefaultFigureWindowStyle','normal')

%==========================================================================
% user-defined input
DiskLabel = 'CalCurCEAS_027'; % user-defined deployment name, match data file name
XLIMITS = [datenum(2024,11,23,14,00,00) datenum(2024,12,03,8,00,00)];  % user-defined time period (only for plots!)
YourName = 'KB';    % user-defined, who ran this script
Nticks = 5; % user-defined, number of tick marks for plots

% data path
PathIn = 'D:\CalCurCEAS_027\RAW\SUD'; % user-defined

% voltage and temp limits/y-ticks
VoltSelect = 'Internal'; % user-defined, change to "External" if needed

YLIMITS_int = [0 5]; YLIMITS_int_ticks = linspace(YLIMITS_int(1),YLIMITS_int(2),Nticks);
YLIMITS_temp = [0 25]; YLIMITS_temp_ticks = linspace(YLIMITS_temp(1),YLIMITS_temp(2),Nticks);


% ---------- Shouldn't need to change these, derived from above -----------
PathOut = fullfile('D:\CalCurCEAS_027\QAQC'); % user-defined, change PathIn if you want the QAQC results stored somewhere other than the folder the data is stored in
mkdir(PathOut)
FileOut = fullfile(PathOut,DiskLabel);
%==========================================================================
%%
TIMEtick = linspace(XLIMITS(1), XLIMITS(2),Nticks);

clear XMLinfo

XMLinfo.DiskLabel = DiskLabel;      % disk label
XMLinfo.LastUpdate = datetime;      % when it was created (updated)
XMLinfo.RanBy = YourName;           % author

%%
XML_Path = PathIn;
SUD_Path = PathIn;
WAV_Path = PathIn;

%%
FilesIn = dir(fullfile(XML_Path, '*.xml'));
FilesSUD = dir(fullfile(SUD_Path, '*.sud'));
FilesWAV = dir(fullfile(WAV_Path, '*.wav')); 

DATESstart = nan(length(FilesIn),1);
DATESstop = nan(length(FilesIn),1);
VOLTS = nan(length(FilesIn),1);
TEMPS = nan(length(FilesIn),1);
CumSampGap = nan(length(FilesIn),1);
SampTimePeriod = nan(length(FilesIn),1);
SampleCount = nan(length(FilesIn),1);
SudSize = nan(length(FilesIn),1);
WavSize = nan(length(FilesIn),1);
XMLSize = nan(length(FilesIn),1);
SudName = cell(length(FilesIn),1);
XMLName = cell(length(FilesIn),1);
WavName = cell(length(FilesIn),1);

for Ifile = 1:length(FilesIn)
    fid = fopen( fullfile(XML_Path,FilesIn(Ifile).name) );
    while ~feof(fid)
        line = fgetl(fid);
        if ~isempty(line) && ischar(line)
            FindDateStart = strfind(line, 'SamplingStartTimeUTC');
            if ~isempty(FindDateStart)
                IJK = strfind(line,'"');
                if length(IJK)==2
                    DATESstart(Ifile) = datenum(datevec(line(IJK(1)+1:IJK(2)-1),'yyyy-mm-ddTHH:MM:SS'));
                end
            end
            FindDateStop = strfind(line, 'SamplingStopTimeUTC');
            if ~isempty(FindDateStop)
                IJK = strfind(line,'"');
                if length(IJK)==2
                    DATESstop(Ifile) = datenum(datevec(line(IJK(1)+1:IJK(2)-1),'yyyy-mm-ddTHH:MM:SS'));
                end
            end

            switch VoltSelect
                case 'Internal'
                    FindBatt = strfind(line, 'INT_BATT UNITS="mV"');
                case 'External'
                    FindBatt = strfind(line, 'EX_BATT UNITS="mV"');
                otherwise
                    disp('Wrong battery choice; program terminated')
                    return
            end

            if ~isempty(FindBatt)
                IJK = sort([strfind(line,'<') strfind(line,'>')]);
                IJK = IJK(2:3);
                VOLTS(Ifile) = str2double(line(IJK(1)+1:IJK(2)-1))*0.001;
            end

            FindTEMP = strfind(line, 'TEMPERATURE UNITS="DegCx100"');
            if ~isempty(FindTEMP)
                IJK = sort([strfind(line,'<') strfind(line,'>')]);
                IJK = IJK(2:3);
                TEMPS(Ifile) = str2double(line(IJK(1)+1:IJK(2)-1))*0.01;
            end
            FindCSG = strfind(line, 'CumulativeSamplingGap');
            if ~isempty(FindCSG)
                IJK = strfind(line,'"');
                if length(IJK)==2
                    CumSampGap(Ifile) = str2double(line(IJK(1)+1:IJK(2)-3));
                end
            end
            FindSTP = strfind(line, 'SamplingTimePeriod');
            if ~isempty(FindSTP)
                IJK = strfind(line,'"');
                if length(IJK)==2
                    SampTimePeriod(Ifile) = str2double(line(IJK(1)+1:IJK(2)-3));
                end
            end
            FindSC = strfind(line, 'SampleCount');
            if ~isempty(FindSC)
                IJK = strfind(line,'"');
                if length(IJK)==2
                    SampleCount(Ifile) = str2double(line(IJK(1)+1:IJK(2)-1));
                end
            end


            SudSize(Ifile) = FilesSUD(Ifile).bytes;
            XMLSize(Ifile) = FilesIn(Ifile).bytes;
            SudName{Ifile} = FilesSUD(Ifile).name;
            XMLName{Ifile} = FilesIn(Ifile).name;
            WavName{Ifile} = FilesWAV(Ifile).name;

            if isfile(fullfile(WAV_Path,WavName{Ifile}))
                wavinfo = dir(fullfile(WAV_Path,WavName{Ifile}));
                WavSize(Ifile) = wavinfo.bytes;
            else
                WavSize(Ifile) = NaN;
            end
        end
    end
    fclose(fid);
end

%%
WavName = cell2mat(WavName); SudName = cell2mat(SudName); XMLName = cell2mat(XMLName);
XMLinfo.SudFiles.name = SudName;
XMLinfo.SudFiles.size_bytes = SudSize;
XMLinfo.XMLFiles.name = XMLName;
XMLinfo.XMLFiles.size_bytes = XMLSize;
XMLinfo.WavFiles.names = WavName;
XMLinfo.WavFiles.size_bytes = WavSize;

XMLinfo.StartDate.parameter = 'Sampling Start Time UTC';
XMLinfo.StartDate.units = 'Date as Matlab Serial Number';
XMLinfo.StartDate.values = DATESstart;

XMLinfo.StopDate.parameter = 'Sampling Stop Time UTC';
XMLinfo.StopDate.units = 'Date as Matlab Serial Number';
XMLinfo.StopDate.values = DATESstop;

XMLinfo.Voltage.parameter = 'Internal Battery Voltage';
XMLinfo.Voltage.units = 'V';
XMLinfo.Voltage.values = VOLTS;

XMLinfo.Temperature.parameter = 'Internal Temperature';
XMLinfo.Temperature.units = '\degC';
XMLinfo.Temperature.values = TEMPS;

XMLinfo.CumSampGap.parameter = 'Cumulative Sampling Gap ';
XMLinfo.CumSampGap.units = 'micro-seconds';
XMLinfo.CumSampGap.values = CumSampGap;

XMLinfo.SampTimePeriod.parameter = 'Sampling Time Period';
XMLinfo.SampTimePeriod.units = 'micro-seconds';
XMLinfo.SampTimePeriod.values = SampTimePeriod;

XMLinfo.SampleCount.parameter = 'Number of Samples per file';
XMLinfo.SampleCount.units = 'SampleCount';
XMLinfo.SampleCount.values = SampleCount;

save(FileOut, 'XMLinfo','SudSize','WavSize','DATESstart','DATESstop','VOLTS','TEMPS','CumSampGap','SampTimePeriod','SampleCount','XMLName')

%%
% Battery and Temperature Plot
figure('Color','w','Position',[1 41 1920 963])
yyaxis left
plot(DATESstart,VOLTS)
axis tight
grid on
set(gca,'XLim',XLIMITS,'XTick',TIMEtick,'XMinorGrid','on')
set(gca,'YLim',YLIMITS_int,'YTick',YLIMITS_int_ticks)
set(get(gca,'YLabel'),'String',[VoltSelect ' Battery, V'])
datetick('x','keeplimits','keepticks')
xlabel('Time')


yyaxis right
plot(DATESstart,TEMPS);
axis tight
grid on
set(gca,'XLim',XLIMITS,'XTick',TIMEtick,'XMinorGrid','on')
set(gca,'YLim',YLIMITS_temp,'YTick',YLIMITS_temp_ticks)
set(get(gca,'YLabel'),'String','Internal Temperature, \circC')
datetick('x','keeplimits','keepticks')

Htitle = title(DiskLabel); set(Htitle,'Interpreter','none')
print('-dpng','-r300',[FileOut '_TV'])

%%
% Eight figure plot, for SAEL Lab, the focus is on the 
figure('Color','w','units','normalized','outerposition',[0 0 1 1])
tcl = tiledlayout(4,2);

%Size of compressed files plot
nexttile
plot(DATESstart,SudSize/1024.^3)
set(gca,'TickLength',[0.005 0])
grid on
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
title('Size of compressed files')
ylabel('Gb')
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)

%Size of wav files plot
nexttile
plot(DATESstart,WavSize/1024.^3)
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
grid on
title('Size of wav files')
ylabel('Gb')
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)

% Sampling Time Period plot
nexttile
plot(DATESstart,SampTimePeriod)
plot(DATESstart,SampTimePeriod*1e-6)
plot(DATESstart,SampTimePeriod*1e-6/3600)
title(XMLinfo.SampTimePeriod.parameter)
ylabel('hours')
ylabel('hrs')
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
grid on
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)

%Sample count / sample time period plot
nexttile
plot(DATESstart, SampleCount./(SampTimePeriod*1e-6))
ylabel('samples/sec')
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
grid on
title('SampleCount/SampleTimePeriod')
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)

% Number of Samples per file plot
nexttile
plot(DATESstart,SampleCount)
title(XMLinfo.SampleCount.parameter)
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
grid on
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)

%Sample time period plot (date/time stop - date/time start)
nexttile
DT = (DATESstop-DATESstart)*86400;
plot(DATESstart,SampTimePeriod*1e-6-DT)
ylabel('secs')
set(gca,'YLim',[-1 1])
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
grid on
title('SampTimePeriod - [DATESstop - DATESstart]')
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)

% Cumulative sampling gap plot
nexttile
plot(DATESstart,CumSampGap*1e-6)
title(XMLinfo.CumSampGap.parameter)
ylabel('secs')
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
grid on
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)
datetick('x','keepticks','keeplimits')

% Gaps between files plot
nexttile
GAP = DATESstart(2:end)-DATESstop(1:end-1);
plot(DATESstart(1:end-1),GAP*86400)
set(gca,'YLim',[-0.5 10])
set(gca,'TickLength',[0.005 0],'XTickLabel',[])
grid on
title('Gaps between files')
ylabel('secs')
GAPout = find(GAP*86400>1.0001);
if ~isempty(GAPout)
    Stitle = cell(length(GAPout),1);
    for k = 1:length(GAPout)
        Stitle{k} = sprintf('%7.1f sec (%7.1f hrs) between #%d (%s) and \n #%d (%s)\n',...
            GAP(GAPout(k))*86400, ...
            GAP(GAPout(k))*24, ...
            GAPout(k), WavName(GAPout(k),end-15:end-4),...
            GAPout(k)+1, WavName(GAPout(k)+1,end-15:end-4));
    end
    xlabel(Stitle) % displays the gaps on the x axis, not usefull if you have many gaps
%    disp(Stitle) % displays gaps in the command window, which you can then save if needed

end
set(gca,'XLim',XLIMITS)
set(gca,'XTick',TIMEtick)
datetick('x','keeplimits','keepticks')


Htitle = title(tcl,DiskLabel); set(Htitle,'Interpreter','none')

print('-dpng','-r300',[FileOut '_QC'])
