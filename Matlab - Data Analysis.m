%Read cvs file and set table labels
file_path = 'data/2022-05-02-14-06-08-mouse-23ExampleDLC_resnet50_EyeTrackingMay04shuffle1_200000.csv';
T = readtable(file_path,'NumHeaderLines',1); 
header = T.Properties.VariableNames ;
data = readtable(file_path, 'NumHeaderLines',2) ; 
data.Properties.VariableNames = header; 

%Create time vector (1 dectection every frame, 24fps, so frame and detection every 1/24 ~ 0.0417 seconds)
time_lim = (size(data,1))/24;
time = 0:0.0417:time_lim; 
time = time(:); %transfor time in from row-vector to column
%Create x axis ticks for time (every 60 sec or every 1 sec if video time < 60 sec)
if (time_lim < 60)
    time_ticks = 0:1:time_lim;
else 
    time_ticks = 0:60:time_lim; 
end

%Create tables to save analized data
save_means = table();
save_data_D = table();

%Check accuracy light and tongue detection, if > 0.8 change value to 1, else to 0
idx = find(data.lightOn_2(:) >= 0.8);
idx2 = find(data.lightOn_2(:) <  0.8);
data.lightOn_2(idx) = 1;
data.lightOn_2(idx2) = 0;

idx = find(data.tongue_2(:) >= 0.8);
idx2 = find(data.tongue_2(:) <  0.8);
data.tongue_2(idx) = 1;
data.tongue_2(idx2) = 0;

%Calculate distances between pupil points and pupil dilation
index = ["1","2","3","4","5","6","7","8"];
for i = 1:numel(index)
    r = index(i);   
    delimiters = '';
    x = join(['p', r], delimiters);
    y = join([x, '_1'], delimiters);    
    new_label_PrPn = join(['d_PrP', r], delimiters); 
    save_data.(new_label_PrPn) = distance(data.p_ref,data.p_ref_1, data.(x), data.(y));  
    if(index(i) == "8")
        r2 = "1";
        x2 = join(['p', r2], delimiters);
        y2 = join([x2, '_1'], delimiters);
        new_label_D = join(['d_P', r,'P', r2], delimiters);
        save_data_D.(new_label_D) = distance(data.(x), data.(y), data.(x2), data.(y2));
    else
        r2 = string(str2double(r) + 1);
        x2 = join(['p', r2], delimiters);
        y2 = join([x2, '_1'], delimiters); 
        new_label_D = join(['d_P', r,'P', r2], delimiters);
        save_data_D.(new_label_D) = distance(data.(x), data.(y), data.(x2), data.(y2));
    end
end
save_means.Mean_PrPn = rowfun(@mean, save_data, 'SeparateInputs', false, "OutputFormat","uniform");
save_means.Mean_Dil = rowfun(@sum, save_data_D, 'SeparateInputs', false, "OutputFormat","uniform");


%Check positions variations over time
%[ave,stdev] = stat(data.p_ref);
m = mean(data.p_ref);
%[ave,stdev] = stat(save_means.Mean_PrPn);
%[ave,stdev] = stat(save_means.Mean_Dil);

%Normalize dilation and mean distance between pupil points and reference
%(light reflection) over column. Both z-score and minmaxnormalzation
Dilation_Norm = normalize(save_means.Mean_Dil,1);
PrPn_Norm = normalize(save_means.Mean_PrPn,1);

Dilation_NormMM = (save_means.Mean_Dil - min(save_means.Mean_Dil))/(max(save_means.Mean_Dil) - min(save_means.Mean_Dil));
PrPn_NormMM = (save_means.Mean_PrPn - min(save_means.Mean_PrPn))/(max(save_means.Mean_PrPn) - min(save_means.Mean_PrPn));

%%PLOTs
%Plot Mean distances and dilation
figure()
plot(time, save_means.Mean_PrPn)
hold on
plot(time, save_means.Mean_Dil)
title('Pupil dilation and mean sum Pref-Pn distances over time','FontSize',26, 'FontName','Arial', 'FontWeight','bold')
legend("Mean distance Pref - Pupil points", "Pupil dilation",'Location','eastoutside','FontName','Arial', 'FontSize', 17)
xticks(time_ticks)
xlabel('Time (s)', 'FontSize',20, 'FontName','Arial')
ylabel('Lenght (pixels)','FontSize',20, 'FontName','Arial')
grid on
hold off

%Plot licking and light on events
figure()
fill(time, data.tongue_2, "r")
hold on
fill(time, data.lightOn_2, "b")
title('Licking and LightOn events','FontSize',26, 'FontName','Arial', 'FontWeight','bold')
legend("Tongue", "LightOn",'Location','eastoutside','FontName','Arial', 'FontSize', 17)
xticks(time_ticks)
xlabel('Time (s)', 'FontSize',20, 'FontName','Arial')
ylabel('A.U.','FontSize',20, 'FontName','Arial')
grid on
hold off

%Plot Normalized Dilation and pupil movement (mean Pref-Pn)over time
figure()
plot(time, Dilation_Norm)
hold on
plot(time, PrPn_Norm)
title('Normalized (z-score) pupil dilation and movement over time','FontSize',26, 'FontName','Arial', 'FontWeight','bold')
legend("Normalized Pupil Dilation", "Normalized Mean distance Pupil-PRef",'Location','eastoutside','FontName','Arial', 'FontSize', 17)
xticks(time_ticks)
xlabel('Time (s)', 'FontSize',20, 'FontName','Arial')
ylabel('A.U.','FontSize',20, 'FontName','Arial')
grid on
hold off

%Plot all data together
figure()
licking = fill(time, data.tongue_2, "r");
set(licking,'facealpha',.4)
hold on
light  = fill(time, data.lightOn_2, "b");
set(light,'facealpha',.4) %set transparency
hold on
plot(time, Dilation_NormMM)
hold on
plot(time, PrPn_NormMM)
title('All data','FontSize',26, 'FontName','Arial', 'FontWeight','bold')
legend("Tongue", "LightOn", "Normalized Pupil Dilation (minmax)", "Normalized Mean distance Pupil-PRef (minmax)",'Location','eastoutside','FontName','Arial', 'FontSize', 17)
xticks(time_ticks)
xlabel('Time (s)', 'FontSize',20, 'FontName','Arial')
ylabel('A.U.','FontSize',20, 'FontName','Arial')
grid on
hold off

function d = distance(x1, y1, x2, y2)
    d = sqrt((x1-x2).^2+(y1-y2).^2); 
end

function mean = mean(x)
    n = length(x);
    mean = sum(x)/n;
    %s = sqrt(sum((x-m).^2/n));
end
