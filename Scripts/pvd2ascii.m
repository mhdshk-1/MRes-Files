%% Get directory and sub-directories that contain pvd files

par_dirinfo = dir();
par_dirinfo = par_dirinfo(~ismember({par_dirinfo.name},{'.','..'})); %remove home and up-directory . & ..
par_dirinfo(~[par_dirinfo.isdir]) = [];  %remove non-directories
subdirinfo = cell(length(par_dirinfo));
for i = 1 : length(par_dirinfo)    
  thisdir = par_dirinfo(i).name;
  subdirinfo{i} = dir(fullfile(thisdir, '*.pvd'));
end

%% For loop to extract pvd data

for i = 1 : length(par_dirinfo)
    if exist('table_data', 'var')
        clear table_data
    else
        if ~isempty(subdirinfo{i,1})
            cd(par_dirinfo(i).name)
            for j = 1 : length(subdirinfo{i,1})
                counter = 1 : length(subdirinfo{i,1});
                count = num2str(counter(j));
                filename = subdirinfo{i,1}(j).name;
                [pathstr,name,ext] = fileparts(filename);
                [Hz, Vel, rec] = GetPointData(subdirinfo{i,1}(j).name,'FFT','Vib','Velocity','Magnitude',1,0);
                if exist('table_data', 'var');                
                    temp_table = array2table([Hz' Vel'], 'VariableNames', {'freq', name});
                    temp_table = array2table(temp_table{1:length(Hz),2}, 'VariableNames', {name});
                    table_data = [table_data temp_table];
                else
                    table_data = array2table([Hz' Vel'], 'VariableNames', {'freq', name});
                end
            end
            writetable(table_data, strcat(par_dirinfo(i).name, 'out.txt'), 'Delimiter', '\t', 'WriteVariableNames', true)
            clear table_data
            cd('../')
        else
            disp(strcat('No pvd files found in: ', par_dirinfo(i).name));        
        end
    end
end