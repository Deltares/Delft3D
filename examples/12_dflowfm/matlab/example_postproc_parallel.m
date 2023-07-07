clear all
close all

addpath('d:/pijl/openearthtools_svn/trunk/matlab\');
try
    dflowfm.readMap;
catch
    oetsettings;
end

% DIR= 'd:/pijl/unstruc_svn/cases/parallel/Waal_schematic/dflowfm/lisa/par32/output_dt0d6_24h';
% % MDUnam='par32_dt0d6_24h';
% MDUnam='par32_dt0d6_24h_whitecolebrook';


DIR= 'd:/pijl/unstruc_svn/cases/parallel/Andy/QLD_MPI/DFM_OUTPUT_GBR_DelftFM';
MDUnam='GBR_DelftFM'

% pol.x = [0 41000];
% pol.y = [10700 10700];

% use first paritition to locate files
FNAM=sprintf('%s/%s_%.4i_map.nc', DIR,MDUnam, 0);

% read partitions
[G,D,ndomains] = dflowfm.readparallel(FNAM,'vel',true);

% plot
figure(1)
dflowfm.plotparallel(G,D)

% get polygon
%pol = dflowfm.get_xy_from_figure(1);
pol.x = [10000 40000];
pol.y = [10900 10900];

% interpolate on polygon
crs = dflowfm.interpolate_on_polygon(G,D,pol);

% plot cross-section
figure(2)
for i=0:ndomains-1
    plot(crs{i+1}.x, crs{i+1}.cen.zwl-crs{i+1}.cen.z)
    hold all
end