%%  作成者
% Masa Yano (Tohoku University/JSPS)
% masayano@kyudai.jp

%% 平均振幅を計算し、csvファイルに保存します。

setBIN = [1:4];       % 平均電位量を出力したいBINの番号
LAT_MIN = 300;                  % 平均電位量の開始潜時
LAT_MAX = 500;                  % 平均電位量の終了潜時

% データをクリア
clear EPOCH_BINmatrix;
clear EPOCH_BINtrial;

%% Select Datasets
for n = 1:length(ALLEEG)
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, n,'retrieve', n);
    % ALLEEG = 読み込まれているEEGデータのリスト, pop_newsetのあとの1つ目の"n"が何を指定しているか不明
    % ここを何にしても"retrieve"のvalueに指定したものが選択される。
    
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

%% 指定したBINのデータを抽出し、平均電位量を求める

%EEG.times;         % ms (e.g. -100:799)
%size(EEG.times,2); % number of data within an epoch
%EEG.pnts;          % number of data within an epoch

% 指定したBINの情報を抽出
n=0;
a=0;
for n=1:size(EEG.epoch, 2);  % number of epochs  
    % if  find(cell2mat(EEG.epoch(n).eventbini(1,1)) == setBIN) >= 1 % find(): setBIN内での番号を返す
    if  find(EEG.epoch(n).eventbini(1,1) == setBIN) >= 1
        a=a+1;
        EPOCH_BINmatrix(a).DATA = EEG.data(:,:,n);        % extract eegdata of setBIN
        EPOCH_BINmatrix(a).AD = EEG.reject.rejmanual(n);  % extract AD mark (0 = OK)
    %   EPOCH_BINmatrix(a).BINlabel = cell2mat(EEG.epoch(n).eventbini(1,1)); % extract set num
        EPOCH_BINmatrix(a).BINlabel = EEG.epoch(n).eventbini(1,1);
    end
end

% 指定した潜時の開始時点と終了時点を見つける
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

% 指定した潜時帯の平均電位量を試行毎に計算
ma = 0;
for ma = 1:size(EPOCH_BINmatrix,2);
   
    Baseline = mean((EPOCH_BINmatrix(ma).DATA(:,1:100)),2) ;                    % Baselineの平均電位量の計算
    MeamAmp = mean(EPOCH_BINmatrix(ma).DATA(:,findMINpoint:findMAXpoint),2);    % 関心のある潜時帯の平均電位量の計算
    EPOCH_BINtrial(ma).MeanAmp = MeamAmp-Baseline;                              % 関心のある潜時帯の平均電位量からBaselineの平均電位量を引いて代入
    EPOCH_BINtrial(ma).AD = EPOCH_BINmatrix(ma).AD;                             % そのまま代入
    EPOCH_BINtrial(ma).BINlabel = EPOCH_BINmatrix(ma).BINlabel;                 % そのまま代入

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