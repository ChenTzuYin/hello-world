%%  �쐬��
% Masa Yano (Tohoku University/JSPS)
% masayano@kyudai.jp

%% ���ϐU�����v�Z���Acsv�t�@�C���ɕۑ����܂��B

setBIN = [1:4];       % ���ϓd�ʗʂ��o�͂�����BIN�̔ԍ�
LAT_MIN = 300;                  % ���ϓd�ʗʂ̊J�n����
LAT_MAX = 500;                  % ���ϓd�ʗʂ̏I������

% �f�[�^���N���A
clear EPOCH_BINmatrix;
clear EPOCH_BINtrial;

%% Select Datasets
for n = 1:length(ALLEEG)
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, n,'retrieve', n);
    % ALLEEG = �ǂݍ��܂�Ă���EEG�f�[�^�̃��X�g, pop_newset�̂��Ƃ�1�ڂ�"n"�������w�肵�Ă��邩�s��
    % ���������ɂ��Ă�"retrieve"��value�Ɏw�肵�����̂��I�������B
    
%% initialize data file

currentfolder = pwd;
cd(currentfolder);
fileName = 0;

while fileName == 0;
    file = [EEG.setname,'.csv'];
    [fileName,pathName] = uiputfile(file, 'Save Data as');
end;

fid = fopen([pathName fileName],'a');    %open data file
      fprintf(fid, '%s\n\n', EEG.setname);
      % fprintf(fid,'bin number: %d\n\n',setBIN);
      fprintf(fid, 'Mean Amp range (ms)\n');
      fprintf(fid, 'Min,Max\n');
      fprintf(fid, '%d,%d\n\n', LAT_MIN, LAT_MAX);
fclose(fid);

%% �w�肵��BIN�̃f�[�^�𒊏o���A���ϓd�ʗʂ����߂�

%EEG.times;         % ms (e.g. -100:799)
%size(EEG.times,2); % number of data within an epoch
%EEG.pnts;          % number of data within an epoch

% �w�肵��BIN�̏��𒊏o
n=0;
a=0;
for n=1:size(EEG.epoch, 2);  % number of epochs  
    % if  find(cell2mat(EEG.epoch(n).eventbini(1,1)) == setBIN) >= 1 % find(): setBIN���ł̔ԍ���Ԃ�
    if  find(EEG.epoch(n).eventbini(1,1) == setBIN) >= 1
        a=a+1;
        EPOCH_BINmatrix(a).DATA = EEG.data(:,:,n);        % extract eegdata of setBIN
        EPOCH_BINmatrix(a).AD = EEG.reject.rejmanual(n);  % extract AD mark (0 = OK)
    %   EPOCH_BINmatrix(a).BINlabel = cell2mat(EEG.epoch(n).eventbini(1,1)); % extract set num
        EPOCH_BINmatrix(a).BINlabel = EEG.epoch(n).eventbini(1,1);
    end
end

% �w�肵�������̊J�n���_�ƏI�����_��������
fmp = 0;
findMINpoint = 0;
findMAXpoint = 0;
for fmp = 1:size(EEG.times,2);          % number of data point
    if EEG.times(fmp) == LAT_MIN;      % find start point as LatMin
        findMINpoint = fmp;
    elseif EEG.times(fmp) == LAT_MAX;   % find end point as LatMax
        findMAXpoint = fmp; break;
    end;
end

% �w�肵�������т̕��ϓd�ʗʂ����s���Ɍv�Z
ma = 0;
for ma = 1:size(EPOCH_BINmatrix,2);
   
    Baseline = mean((EPOCH_BINmatrix(ma).DATA(:,1:100)),2) ;                    % Baseline�̕��ϓd�ʗʂ̌v�Z
    MeamAmp = mean(EPOCH_BINmatrix(ma).DATA(:,findMINpoint:findMAXpoint),2);    % �֐S�̂�������т̕��ϓd�ʗʂ̌v�Z
    EPOCH_BINtrial(ma).MeanAmp = MeamAmp-Baseline;                              % �֐S�̂�������т̕��ϓd�ʗʂ���Baseline�̕��ϓd�ʗʂ������đ��
    EPOCH_BINtrial(ma).AD = EPOCH_BINmatrix(ma).AD;                             % ���̂܂ܑ��
    EPOCH_BINtrial(ma).BINlabel = EPOCH_BINmatrix(ma).BINlabel;                 % ���̂܂ܑ��

%% save trial data 
   fid=fopen([pathName fileName],'a');    %open data file
   if ma==1;
     fprintf(fid,'trial,');
     for b=1:size(EPOCH_BINtrial(ma).MeanAmp,1)
         CHname=['ch',num2str(b)];
         fprintf(fid,'%s,',CHname);      
     end   
     fprintf(fid,'Articfact Detection,');
     fprintf(fid,'Bin label\n');
   end
   
   fprintf(fid,'%d,',ma);
   for b=1:size(EPOCH_BINtrial(ma).MeanAmp,1)    
       fprintf(fid,'%.2f,',EPOCH_BINtrial(ma).MeanAmp(b));
   end   
   fprintf(fid,'%d,%d\n',EPOCH_BINtrial(ma).AD,EPOCH_BINtrial(ma).BINlabel);
   fclose(fid);
    
end
end