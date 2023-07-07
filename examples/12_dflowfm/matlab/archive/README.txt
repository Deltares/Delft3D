MATLAB Routines for Unstruc
---------------------------
More info/questions: Arthur van Dam <Arthur.vanDam@deltares.nl>

* Installation:
Add the path <UNSTRUCDIR>/matlab to your matlab path and copy all
files and the '+unstruc' directory into it.

Also install parseargs:
http://www.mathworks.com/matlabcentral/fileexchange/10670-parseargs-simplifies-input-processing-for-functions-with-multiple-options
And also add it to your path (Typically put some addpath commands in the file
$HOMEDIR\matlab\startup.m).

* Usage:
All Unstruc-MATLAB routines are in a package 'unstruc' (by means of
the directory '+unstruc'). Call all routines prefixed with 'unstruc.'.

Example:
matlab> udata = unstruc.readNet('index4_net.nc')
matlab> unstruc.plotNet(udata)

matlab> udata = unstruc.readMap('avdtest02_map.nc')
matlab> unstruc.plotNet(udata,'PlotType','flow')
matlab> unstruc.plotNet(udata, 'PlotType','flowcells','XYLim',[0 1200 2000 4000])

matlab> hisdata = unstruc.readHis('avdtest02_his.nc')
matlab> unstruc.plotHisStations(hisdata, 'testldbfile.ldb')

* Documentation:
matlab> help unstruc