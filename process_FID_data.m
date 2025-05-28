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

    % if ~strcmp(subdat.subject,'BV156')
    %     continue;
    % end
    
    
    % Loop through both ses-v1 and ses-v2 folders
    for ses = {'ses-v1', 'ses-v2'}
        % flag for valid subject data
        valid_task_run = 1;
        func_folder = fullfile(base_dir, subject_folder, ses{1}, 'func');
        if ~isfolder(func_folder)
            continue;
        end
        % Each task run was broken up into four runs of 30 trials each
        % Combine these parts to build a full table for the session
        tsv_data_full = table();
        % Keep track of the number of runs they completed
        has_runs = [];
        for run = 1:4
            search_pattern = sprintf('%s_%s_task-flightinitiationdistance%d_events.tsv', subject_folder, ses{1}, run);
            tsv_file = dir(fullfile(func_folder, search_pattern));
            % Check to see if there is a file for this run
            if ~isempty(tsv_file)
                tsv_data = readtable(fullfile(tsv_file.folder, tsv_file.name), 'FileType', 'text', 'Delimiter', '\t');
                % check to make sure this file is complete (i.e., it has 30
                % trials). This will appear to be 31 because it's zero indexed and
                % trial_number==30 isn't an actual trial. 
                if length(unique(tsv_data.trial_number(~isnan(tsv_data.trial_number)))) == 31
                    tsv_data.trial = tsv_data.trial_number + ((run-1) * 30);
                    tsv_data_full = [tsv_data_full; tsv_data];
                    has_runs = [has_runs; run];
                end
            end
        end
        
        % For trial_type, here is how to parse: <0,1,2,3,4>_<S,F> = Block Type (0=0_0, 1=1_1, 2=1_2, 3=2_1, 4=2_2) _ Predator Type (slow, fast) ; Note: block type code is <reward multiplier_shock multiplier>
        % i.e. S = slow, F = fast
        % 0 = 0 reward, 0 shock
        % 1 = 1 reward, 1 shock
        % 2 = 1 reward, 2 shock
        % 3 = 2 reward, 1 shock
        % 4 = 2 reward, 2 shock
        
        % "The runway has a total length of 90 units, where the
        % participant’s figure is placed 10 units to the safety exit." I
        % will therefore subtract from 80 the values in the table showing
        % how far the predator traveled to get relative distances

        % Get the number of trials
        num_trials = length(has_runs)*30;
        trial_data = zeros(num_trials, 6); % Preallocate for trial, trial_type, FID, AD, reward_level, shock_level
        trials_per_run = 30;
        % Get the trial numbers to iterate through
        trial_list = reshape(cell2mat(arrayfun(@(r) (r-1)*trials_per_run : r*trials_per_run - 1, has_runs(:)', 'UniformOutput', false)), 1, []);

        for trial = 0:num_trials-1
            game_trial = trial_list(trial+1); % Note that this for loop will always go from 0 to the number of trials-1, but someone may complete only the last two blocks so their game_number will go from 61 to 120 instead of 0 to 59.
            trial_rows = tsv_data_full(tsv_data_full.trial == game_trial,:);
            
            % flight initiation
            flight_initiation_idx = find(trial_rows.event_code == 10);
            if ~isempty(flight_initiation_idx)
                FID = 80 - trial_rows(flight_initiation_idx - 1,:).result;
            else
                FID = 0;
            end
            
            % attack distance
            attack_initiation_idx = find(trial_rows.event_code == 11);
            if ~isempty(attack_initiation_idx)
                AD = 80 - trial_rows(attack_initiation_idx,:).result;
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
            trial_data(trial + 1, :) = [game_trial+1, trial_type, FID, AD, reward_level, shock_level];
        end
        trial_table = array2table(trial_data, 'VariableNames', {'Trial', 'PredatorSpeed', 'FID', 'AD', 'RewardLevel', 'ShockLevel'});
        trial_table.subject = repmat([subdat.subject '_' ses{1}], num_trials, 1);  % Assuming subject_id is a constant for all trials
        % Append the trial_table to the all_data table
        all_data = [all_data; trial_table];  % Accumulate all data
    end
end

% remove Maria's data
all_data.subject = cellstr(all_data.subject); % Convert character array to cell array of strings
all_data(contains(all_data.subject, 'MI999'), :) = []; % Now this should work

% note that BV156 had no events file for Ses-V1 but had physio files
% BW460 and AA374 had only 3 out of 4 parts for Ses-V2

% to look at unique strings
% unique(string(all_data.subject))


% save results
%writetable(all_data, 'L:\rsmith\lab-members\cgoldman\ironside_FID\LIBR_FID_scripts_CMG\data\expanded_data_LIBR_5-28-25.csv');

end
