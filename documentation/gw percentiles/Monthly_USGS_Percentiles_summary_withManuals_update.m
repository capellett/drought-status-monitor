function Monthly_USGS_Percentiles_summary_withManuals;

% last updated on 3/25/09
% Computes monthly medians for each year and then comutes specified monthly
% percentiles based on the monthly median values. Methodology is same as
% method used by USGS on their water watch site for ground water data. There
% is no limit on number of days per month in which to include a monthly
% median value in percentile computations.
% Input file should be a txt file with two columns, column 1 ifs the date and column
% 2 is the flow values. The date column in the input file must be in the mm/dd/yyyy format. 

%% Open file 
% creates character string which identifies path and filename for desired stream, only have to 
% change 'filename'
filename='MCK-0052_1993-2019';
path = 'C:\Users\harders\Documents\MATLAB\Statprograms\GW-stats\Input-Files\';
extension = '.txt'; 

infile = [path filename extension]; 
Df=1;
nlines=0;  % used to count number of lines in file - probably a simpler way
fid=fopen(infile);

%% Reads file line by line and converts date string to number and flow value
% string to a number
while feof(fid)~=1   
    nrow=fgetl(fid);
    l=length(nrow);
    if l>11                                 %only selects lines of data that have a  value, i.e. missing flow values are skipped. Not %sure if this is robust enough filter                           
        dayslash=find(nrow=='/');           % Not sure if this is robust enough filter
        daydateStr = nrow((dayslash(1)-2:dayslash(2)+4));
        daydate(Df)=datenum(daydateStr,'mm/dd/yyyy');
        Qval(Df)=str2num(nrow((dayslash(2)+ 6: l)));
        Df=Df+1; 
    end
    nlines=nlines+1;  %counts number of lines in file
end




Ndays = length(Qval);  % counts number of days with flow values
% newout = [daydate' Qval'];  %creates new array with Matlab formatted number date and corresponding Q value
% 
[year, month, day] = datevec(daydate);
% newout2= [month' Qval'];
i=1;
k=1;
newout3 = [month' year' Qval'];
% Create monthly median data table - with format: year month median
for y = min(year):max(year)
    for m=1:12
        month_dat = 0;
        for j = 1:Ndays
            if newout3(j,1) == m
                if newout3(j,2)== y
                month_dat(i) = newout3(j,3);
                i=i+1;
                end
            end
        end
        if month_dat ~= 0 
           month_med(k) = median(month_dat);
           mon (k) = m;
           yr(k) = y;
           No_of_days (k) = length(month_dat);
           k=k+1;
        end
        i=1;
        j=1;
    end
end
median_table = [yr' mon' month_med' No_of_days'];

clear y;
clear m;
clear month_dat;
N_months = length(median_table);
clear m;
ii=1;
percentiles = [.10; .25; .50; .75; .90];

for m = min(mon):max(mon)
    month_dat = 0;
    for j=1:N_months
        
        if median_table(j,2) == m
            month_dat(i)= median_table(j,3);
            i=i+1;
        end
        
    end
    if month_dat ~= 0
        No_of_years(m) = length(month_dat);
        month_min(m) = min(month_dat);  % minimum here is highest or shallowest water level for ground water data
        month_max(m) = max(month_dat);  % maximum here is lowest or deepest water level for ground water data
        month_median(m) = median(month_dat);
        month_sort = sort(month_dat, 'descend');
        for p = [.02,.05,.10,.20,.25,.30,.50,0.70,.75,.90]
            t = p * (No_of_years(m)+1);
            k = floor(t);
            d = t - k;
            
            if k == 0
                p_value (m,ii) = month_sort(1);
            elseif k == No_of_years(m)
                p_value (m, ii) = month_sort(No_of_years(m));
            else
                p_value (m,ii) = month_sort(k) + d * (month_sort(k+1) - month_sort(k));
            end
                
            ii=ii+1;
        end
        p_summary = [month_max' p_value month_min' No_of_years'];
        
    end
    i=1;
    j=1;
    ii=1;
end


path_out = 'C:\Users\harders\Documents\MATLAB\Statprograms\GW-stats\Output\';
extension_out_long = 'USGS_Monthly_Percentiles.txt';
extension_out_long2 = 'USGS_Monthly_Medians.txt';

file_out_long = [path_out filename extension_out_long];  % creates path and filename string for writing to output
file_out_long2 = [path_out filename extension_out_long2];
dlmwrite(file_out_long, p_summary, 'delimiter', '\t');
dlmwrite(file_out_long2, median_table, 'delimiter', '\t');
end



    


        
