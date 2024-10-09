function process_FID_data()
clear all
dbstop if error;

% Define the base directory
base_dir = 'L:\NPC\studies\Ironside-2023-TCADPilot';
% Get a list of all subfolders starting with 'sub-'
subfolders = dir(fullfile(base_dir, 'sub-*'));

% table to contain everyone's data
all_data = table();

% Loop through each subject folder
for i = 1:length(subfolders)
    if ~subfolders(i).isdir
        continue;
    end
    % Get the subject folder name (e.g., 'sub-BC553')
    subject_folder = subfolders(i).name;
    subdat.subject = strrep(subject_folder, 'sub-', '');
    % flag for valid subject data
    valid_task_run = 1;
    % Loop through both ses-v1 and ses-v2 folders
    for ses = {'ses-v1', 'ses-v2'}
        func_folder = fullfile(base_dir, subject_folder, ses{1}, 'func');
        if ~isfolder(func_folder)
            continue;
        end
        all_tsv_files = [];
        % each task run was broken up into four parts
        for i = 1:4
            search_pattern = sprintf('%s_%s_task-flightinitiationdistance%d_events.tsv', subject_folder, ses{1}, i);
            tsv_files = dir(fullfile(func_folder, search_pattern));
            % skip any subject who doesn't have complete data for a task
            % run (or duplicate data)
            if length(tsv_files) ~= 1 
                valid_task_run = 0;
                fprintf('%s does not have valid data for %s\n', subdat.subject, ses{1});
                break;
            end
            all_tsv_files = [all_tsv_files; tsv_files];  % Append found files to the list
        end
        % if participant did not have complete data for this task run,
        % continue
        if ~valid_task_run
            continue;
        end
        % Loop through the found files and build full file
        tsv_data_full = table();
        for j = 1:length(all_tsv_files)
            tsv_file_path = fullfile(all_tsv_files(j).folder, all_tsv_files(j).name);
            tsv_data = readtable(tsv_file_path, 'FileType', 'text', 'Delimiter', '\t');
            tsv_data.trial = tsv_data.trial_number + ((j-1) * 30);
            tsv_data_full = [tsv_data_full; tsv_data];
        end
        
        % check to make sure contains all 120 trials
        if ~all(ismember(1:120, tsv_data_full.trial))
            continue;
        end
        
        
        % For trial_type, here is how to parse: <0,1,2,3,4>_<S,F> = Block Type (0=0_0, 1=1_1, 2=1_2, 3=2_1, 4=2_2) _ Predator Type (slow, fast) ; Note: block type code is <reward multiplier_shock multiplier>
        % i.e. S = slow, F = fast
        % 0 = 0 reward, 0 shock
        % 1 = 1 reward, 1 shock
        % 2 = 1 reward, 2 shock
        % 3 = 2 reward, 1 shock
        % 4 = 2 reward, 2 shock
        num_trials = 120;
        trial_data = zeros(num_trials, 6); % Preallocate for trial, trial_type, FID, AD, reward_level, shock_level
        for trial = 0:num_trials-1
            trial_rows = tsv_data_full(tsv_data_full.trial == trial,:);
            
            % flight initiation
            flight_initiation_idx = find(trial_rows.event_code == 10);
            if ~isempty(flight_initiation_idx)
                FID = trial_rows(flight_initiation_idx - 1,:).result;
            else
                FID = 0;
            end
            
            % attack distance
            attack_initiation_idx = find(trial_rows.event_code == 11);
            if ~isempty(attack_initiation_idx)
                AD = trial_rows(attack_initiation_idx,:).result;
            else
                error("I think there should always be an attack distance");
            end
            
            % color i.e. fast (2) or slow (1)
            trial_type = contains(trial_rows(trial_rows.event_code == 8,:).trial_type, 'F') + 1;
            
            % reward_level and shock level
            if contains(trial_rows(trial_rows.event_code == 8,:).trial_type, '0')
                reward_level = 0;
                shock_level = 0;
            elseif contains(trial_rows(trial_rows.event_code == 8,:).trial_type, '1')
                reward_level = 1;
                shock_level = 1;
            elseif contains(trial_rows(trial_rows.event_code == 8,:).trial_type, '2')
                reward_level = 1;
                shock_level = 2;
            elseif contains(trial_rows(trial_rows.event_code == 8,:).trial_type, '3')
                reward_level = 2;
                shock_level = 1;
            elseif contains(trial_rows(trial_rows.event_code == 8,:).trial_type, '4')
                reward_level = 2;
                shock_level = 2;               
            end
            
             % Store the results for the current trial
            trial_data(trial + 1, :) = [trial+1, trial_type, FID, AD, reward_level, shock_level];
        end
        trial_table = array2table(trial_data, 'VariableNames', {'Trial', 'TrialType', 'FID', 'AD', 'RewardLevel', 'ShockLevel'});
        trial_table.subject = repmat([subdat.subject '_' ses{1}], num_trials, 1);  % Assuming subject_id is a constant for all trials
        % Append the trial_table to the all_data table
        all_data = [all_data; trial_table];  % Accumulate all data
    end
end

% to look at unique strings
% unique(string(all_data.subject))
end
