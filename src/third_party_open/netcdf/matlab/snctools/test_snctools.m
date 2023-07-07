function test_snctools()
% Runs SNCTOOLS test suite.

testroot = fileparts(mfilename('fullpath'));

% Add test directory to the path, change to a temporary folder, and
% let'r rip!
addpath([testroot  '/tests']);
cdir = pwd;
cd(tempdir);
run_snctools_tests;

delete('*.nc');

cd (cdir);

