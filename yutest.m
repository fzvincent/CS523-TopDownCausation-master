% Add JIDT jar library to the path, and disable warnings that it's already there:
warning('off','MATLAB:Java:DuplicateClass');
javaaddpath('D:\Yu\Projects\logical\infodynamics\infodynamics.jar');
% Add utilities to the path
addpath('D:\Yu\Projects\logical\infodynamics\demos\octave');

% 0. Load/prepare the data:
data = load('D:\Yu\Projects\logical\infodynamics\demos\data\2coupledDiscreteCols-1.txt');
% Column indices start from 1 in Matlab:
source = octaveToJavaIntArray(data(:,1));
destination = octaveToJavaIntArray(data(:,2));

% 1. Construct the calculator:
calc = javaObject('infodynamics.measures.discrete.MutualInformationCalculatorDiscrete', 3, 3, 0);
% 2. No other properties to set for discrete calculators.
% 3. Initialise the calculator for (re-)use:
calc.initialise();
% 4. Supply the sample data:
calc.addObservations(source, destination);
% 5. Compute the estimate:
result = calc.computeAverageLocalOfObservations();

fprintf('MI_Discrete(col_0 -> col_1) = %.4f bits\n', ...
	result);
