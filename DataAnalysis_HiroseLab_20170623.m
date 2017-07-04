%%  �쐬��
% Masa Yano (Tohoku University/JSPS)
% masayano@kyudai.jp

%% EEGLAB�̗����グ

eeglab

%% �t�@�C�������w�肵�Ă��������@%%

CommonCode = ['2017-'];        % �f�[�^�t�@�C�����̋��ʕ��������
IndivCode  = ['02-21-03'];     % �f�[�^�t�@�C�����ɈႤ������S�ē���(������������)
%IndivCode  =['02-21-01';'02-21-02';'02-21-03';'02-22-01';'02-22-02';'02-22-03';'02-23-01';'02-23-02';'02-23-03';'02-23-04';'02-23-05';'02-24-01';'02-24-03';'03-01-01';'03-01-03';'03-03-01';'03-03-02';'03-06-01';'03-06-02';'03-07-01';'03-07-02';'03-07-03';'03-07-04'];  

for FileNumber = 1:size(IndivCode,1); % �f�[�^�t�@�C���̐����擾

SubjName = [CommonCode, IndivCode(FileNumber,:)]; % �t�@�C�����擾

%% EEG�t�@�C��(.CNT)�̏ꏊ����͂��Ă��������B%%

DataLocation = '/Volumes/Transcend2/Main/Experiment_HiroseLabTaiwan/CNT/';

%% Epoch�i���Z�͈́j����͂��ĉ����� %%

StartEpoch = -100;
EndEpoch = 2000;
Epoch = [StartEpoch, EndEpoch];

%% bin�t�@�C���i.txt�j�̖��O����͂��ĉ����� %%

BinName = 'bin';

%% �d�Ɉʒu���t�@�C���̏ꏊ����͂��ĉ����� %%

ChannelLocationPath = '/Volumes/Transcend2/Main/Experiment_HiroseLabTaiwan/ChannelLocation_20170502.elp';
% Original File = /eeglab13_6_5b/plugins/dipfit2.3/standard_BESA/standard-10-5-cap385.elp

%% �ϐ��̐錾 %%
    
% �g���q
CntExtension = '.cnt';
TxtExtension = '.txt';
ErpExtension = '.erp';
SetExtension = '.set';

% 2.�f�[�^��
EEGFileName = [SubjName, CntExtension];         % �t�@�C�����i�g���q�t���j
DataLocationSubName = [DataLocation, SubjName]; % �t�@�C���̃t���p�X
EEGName = [SubjName, '-EEG', SetExtension];     % LME�p�̃t�@�C����
ERPName = [SubjName, '-ERP'];                   % ANOVA�p�̃t�@�C����

%% ���������GUI�ōs�������̂��X�N���v�g�Ŏ��s���Ă����܂� %% 

%% �f�[�^�̓ǂݍ��� %% 

EEG = pop_loadcnt([DataLocation, EEGFileName],'dataformat', 'int32', 'memmapfile', '');
EEG.setname = SubjName;
EEG.filename = SubjName;

%% �d�Ɉʒu�̓ǂݍ��� %% 

EEG.chanlocs(33).labels = 'A1';     % M1
EEG.chanlocs(43).labels = 'A2';     % M2
EEG.chanlocs(60).labels = 'OI1';    % CB1
EEG.chanlocs(64).labels = 'OI2';    % CB1
EEG.chanlocs(65).labels = 'HEOG';   % HEO
EEG.chanlocs(66).labels = 'VEOG';   % VEO

EEG = pop_chanedit(EEG,'load',{ChannelLocationPath, 'filetype' 'autodetect'});
EEG = eeg_checkset(EEG);

%% �Ŋ���i���莞�FCP��CPz�̊ԁj%% 

% EEG = pop_reref(EEG, [33 43] ,'keepref','on');  % A1/A2 reference
EEG = pop_reref( EEG, [],'exclude',[33 43 65 66] ); % Average reference, A1/2, V/HEOG�����O

%% FILTER

% EEGLAB > Tools > Filter the data > Basic FIR filter (new, default)
% EEG = pop_eegfiltnew(EEG, 0.1, 30, 33000, 0, [], 0);
% EEG = eeg_checkset(EEG);

% ERPLAB > FIlter & Frequency Tools > Filters for EEG data
EEG = pop_basicfilter(EEG, 1:66, 'Boundary', 'boundary', 'Cutoff', 10, 'Design', 'fir', 'Filter', 'lowpass', 'Order', 36);
EEG = eeg_checkset(EEG);

% ����̂�
EEG  = pop_basicfilter(EEG, [33 43] , 'Boundary', 'boundary', 'Cutoff',  10, 'Design', 'fir', 'Filter', 'lowpass', 'Order', 1000);

%% Create Basic Event List

EEG = pop_creabasiceventlist(EEG , 'AlphanumericCleaning', 'on', 'Eventlist', DataLocationSubName, 'Newboundary', { -99 }, 'Stringboundary', { 'boundary' }, 'Warning', 'off' );
EEG = eeg_checkset(EEG);

%% BINLISTER

BinLocation = [DataLocation, BinName, TxtExtension];
event_export = [DataLocation, SubjName, TxtExtension];
EEG = pop_binlister(EEG , 'BDF', BinLocation, 'ExportEL', event_export, 'IndexEL',  1, 'SendEL2', 'EEG&Text', 'Voutput', 'EEG' );
EEG = eeg_checkset(EEG);

%% BIN CREATION

EEG = pop_epochbin(EEG , Epoch, 'pre');
EEG = eeg_checkset(EEG);

%% ARTEFACT DETECTION & SAVE

EEG = pop_artextval(EEG , 'Channel', 1:66, 'Flag', 1, 'Threshold', [-100 100], 'Twindow', Epoch);

% Visual Inspection
% EEG = pop_eegplot( EEG, 1, 1, 0); % �����Ficacomp, superpose, reject, see pop_eegplot
% ERPLAB > Artifact detection in epoched data > Synchronize artifact info in EEG and EVENTLIST
% EEG = pop_syncroartifacts(EEG, 1);

EEG = pop_saveset(EEG, 'filename', EEGName,'filepath', DataLocation);
EEG = eeg_checkset(EEG);

%% ERPs

ERP = pop_averager(EEG, 'Criterion', 'good', 'SEM', 'off');

%% BIN OPERATION�i�g�|�O���t�B�[�p�̍��g�`�̍쐬�j

ERP = pop_binoperator(ERP, {'BIN5  = b2 - b1  label Exp1_OverEffect'});       % Over-generated minus baseline
ERP = pop_binoperator(ERP, {'BIN6 = b3 - b1  label Exp1_MismatchEffect'});   % Tone-mismatch minus baseline
ERP = pop_binoperator(ERP, {'BIN7 = b4 - b1  label Exp1_ImpossibleEffect'}); % Impssible minus baseline

%% SAVE ERP DATA

ERPNameErpExtension = [ERPName, ErpExtension];
ERP = pop_savemyerp(ERP, 'erpname', ERPName, 'filename', ERPNameErpExtension, 'filepath', DataLocation);

end

%% ���Z�񐔂̌v�Z

currentfolder = pwd;
cd(currentfolder);
filename = ['ERP.ntrials.csv'];

for FileNumber=1:size(IndivCode,1);

    SubjName = [CommonCode,IndivCode(FileNumber,:)];
    erp_filename = [SubjName, '-ERP.erp']; 
    ERP = pop_loaderp('filename', erp_filename, 'filepath', DataLocation);
    ntrials = [ERP.ntrials.accepted];
    
    if FileNumber == 1;
        ntrials_all =  ntrials;
    else
        ntrials_all = vertcat(ntrials_all, ntrials);
    end
        
end

csvwrite(filename,ntrials_all);
