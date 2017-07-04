%%  作成者
% Masa Yano (Tohoku University/JSPS)
% masayano@kyudai.jp

%% EEGLABの立ち上げ

eeglab

%% ファイル名を指定してください　%%

CommonCode = ['2017-'];        % データファイル名の共通部分を入力
IndivCode  = ['02-21-03'];     % データファイル毎に違う部分を全て入力(同じ文字数で)
%IndivCode  =['02-21-01';'02-21-02';'02-21-03';'02-22-01';'02-22-02';'02-22-03';'02-23-01';'02-23-02';'02-23-03';'02-23-04';'02-23-05';'02-24-01';'02-24-03';'03-01-01';'03-01-03';'03-03-01';'03-03-02';'03-06-01';'03-06-02';'03-07-01';'03-07-02';'03-07-03';'03-07-04'];  

for FileNumber = 1:size(IndivCode,1); % データファイルの数を取得

SubjName = [CommonCode, IndivCode(FileNumber,:)]; % ファイル名取得

%% EEGファイル(.CNT)の場所を入力してください。%%

DataLocation = '/Volumes/Transcend2/Main/Experiment_HiroseLabTaiwan/CNT/';

%% Epoch（加算範囲）を入力して下さい %%

StartEpoch = -100;
EndEpoch = 2000;
Epoch = [StartEpoch, EndEpoch];

%% binファイル（.txt）の名前を入力して下さい %%

BinName = 'bin';

%% 電極位置情報ファイルの場所を入力して下さい %%

ChannelLocationPath = '/Volumes/Transcend2/Main/Experiment_HiroseLabTaiwan/ChannelLocation_20170502.elp';
% Original File = /eeglab13_6_5b/plugins/dipfit2.3/standard_BESA/standard-10-5-cap385.elp

%% 変数の宣言 %%
    
% 拡張子
CntExtension = '.cnt';
TxtExtension = '.txt';
ErpExtension = '.erp';
SetExtension = '.set';

% 2.データ名
EEGFileName = [SubjName, CntExtension];         % ファイル名（拡張子付き）
DataLocationSubName = [DataLocation, SubjName]; % ファイルのフルパス
EEGName = [SubjName, '-EEG', SetExtension];     % LME用のファイル名
ERPName = [SubjName, '-ERP'];                   % ANOVA用のファイル名

%% ここからはGUIで行ったものをスクリプトで実行していきます %% 

%% データの読み込み %% 

EEG = pop_loadcnt([DataLocation, EEGFileName],'dataformat', 'int32', 'memmapfile', '');
EEG.setname = SubjName;
EEG.filename = SubjName;

%% 電極位置の読み込み %% 

EEG.chanlocs(33).labels = 'A1';     % M1
EEG.chanlocs(43).labels = 'A2';     % M2
EEG.chanlocs(60).labels = 'OI1';    % CB1
EEG.chanlocs(64).labels = 'OI2';    % CB1
EEG.chanlocs(65).labels = 'HEOG';   % HEO
EEG.chanlocs(66).labels = 'VEOG';   % VEO

EEG = pop_chanedit(EEG,'load',{ChannelLocationPath, 'filetype' 'autodetect'});
EEG = eeg_checkset(EEG);

%% 最基準化（測定時：CPとCPzの間）%% 

% EEG = pop_reref(EEG, [33 43] ,'keepref','on');  % A1/A2 reference
EEG = pop_reref( EEG, [],'exclude',[33 43 65 66] ); % Average reference, A1/2, V/HEOGを除外

%% FILTER

% EEGLAB > Tools > Filter the data > Basic FIR filter (new, default)
% EEG = pop_eegfiltnew(EEG, 0.1, 30, 33000, 0, [], 0);
% EEG = eeg_checkset(EEG);

% ERPLAB > FIlter & Frequency Tools > Filters for EEG data
EEG = pop_basicfilter(EEG, 1:66, 'Boundary', 'boundary', 'Cutoff', 10, 'Design', 'fir', 'Filter', 'lowpass', 'Order', 36);
EEG = eeg_checkset(EEG);

% 今回のみ
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
% EEG = pop_eegplot( EEG, 1, 1, 0); % 引数：icacomp, superpose, reject, see pop_eegplot
% ERPLAB > Artifact detection in epoched data > Synchronize artifact info in EEG and EVENTLIST
% EEG = pop_syncroartifacts(EEG, 1);

EEG = pop_saveset(EEG, 'filename', EEGName,'filepath', DataLocation);
EEG = eeg_checkset(EEG);

%% ERPs

ERP = pop_averager(EEG, 'Criterion', 'good', 'SEM', 'off');

%% BIN OPERATION（トポグラフィー用の差波形の作成）

ERP = pop_binoperator(ERP, {'BIN5  = b2 - b1  label Exp1_OverEffect'});       % Over-generated minus baseline
ERP = pop_binoperator(ERP, {'BIN6 = b3 - b1  label Exp1_MismatchEffect'});   % Tone-mismatch minus baseline
ERP = pop_binoperator(ERP, {'BIN7 = b4 - b1  label Exp1_ImpossibleEffect'}); % Impssible minus baseline

%% SAVE ERP DATA

ERPNameErpExtension = [ERPName, ErpExtension];
ERP = pop_savemyerp(ERP, 'erpname', ERPName, 'filename', ERPNameErpExtension, 'filepath', DataLocation);

end

%% 加算回数の計算

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
