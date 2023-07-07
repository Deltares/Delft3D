addpath('d:\pijl\bin')

clear all
close all

% binary locations
REFDIR = 'd:\pijl\unstruc_second_svn\';
NEWDIR = 'd:\pijl\unstruc_clean_svn\';

% executable location
EXEDIR = 'bin\x64\Release\';
% resource locatoin
RESDIR = 'res\';

% executable
EXE = 'dflowfm.exe';

% options
OPTS = '--autostartstop';

% resource files
RESFILES = {'unstruc.ini'; 'isocolour.hls'; 'interact.ini'};

% outputdir
OUTPUTDIR = 'dflowfmoutput\';

% testcase directories
F05 = 'f05_boundary_conditions';
F06 = 'f06_external_forcing';

CASEDIRS= { ...
    strcat(F05, '\', 'c060_riemann_tim_1d')
    strcat(F05, '\', 'c061_riemann_tim_2d')
    strcat(F05, '\', 'c062_riemann_cmp_1d')
    strcat(F05, '\', 'c063_riemann_cmp_2d')
    strcat(F05, '\', 'c064_riemann_bc_tim_1d')
    strcat(F05, '\', 'c065_riemann_bc_tim_2d')
    strcat(F05, '\', 'c066_riemann_bc_cmp_1d')
    strcat(F05, '\', 'c067_riemann_bc_cmp_2d')
    strcat(F05, '\', 'c068_riemann_tim_1d_outer')
    strcat(F05, '\', 'c069_riemann_tim_2d_outer')
    strcat(F06, '\', 'c030_flatbed_quadrilateral')
    strcat(F06, '\', 'c031_flatbed_quadrilateral_fine')
    strcat(F06, '\', 'c032_flatbed_quadrilateral_finest')
    strcat(F06, '\', 'c033_flatbed_triangular')
    strcat(F06, '\', 'c034_linearbed_quadrilateral')
    strcat(F06, '\', 'c035_linearbed_quadrilateral_piecewiselinear')
    strcat(F06, '\', 'c036_cosine_squares')
    strcat(F06, '\', 'c037_cosine_triangles')
    strcat(F06, '\', 'c040_riemannbndshift_linear')
    strcat(F06, '\', 'c041_riemannbndshift_piecewiselinear')
    strcat(F06, '\', 'c042_riemannbndshift_cosine')
};

numcases = length(CASEDIRS);

CURDIR = pwd;
        
% make plot dir
plotloc = 'diffplot';
mkdir(CURDIR,plotloc);
plotloc = [CURDIR, '\', plotloc];

for icase=1:numcases
    casedir = CASEDIRS{icase};
    
%   descend into case directory
    cd(strcat(CURDIR, '\', casedir));
    
%   get all *.mdu files
    D = dir('*.mdu');
    
    nummdu = length(D);
    
    if ( nummdu<1 )
        continue
    end
        
%   copy resource files
    for ires=1:length(RESFILES)
        resfile = strcat(NEWDIR,RESDIR,RESFILES{ires});
        cmdstr = ['if not exist ', RESFILES{ires}, ' copy ', resfile, '.'];
        system(cmdstr);
    end
    
%   run all mdu files
    for imdu = 1:nummdu
        mdu = D(imdu).name;
        
%       remove old his-files
        cmd = ['del ', OUTPUTDIR, '*his.nc'];
        system(cmd);
        
%       get modelname
        [pathstr,modelname,ext]=fileparts(mdu);
        
%       run reference        
        cmd = [REFDIR, EXEDIR, EXE, ' ', OPTS, ' ', mdu];
        system(cmd);
        
%       hisfiles
        Dhis = dir([OUTPUTDIR, '*_his.nc']);
        hisname = [OUTPUTDIR, '/', Dhis.name];
        
%       open hisfiles
        nc_id = netcdf.open(hisname);
        
%       read data
        time = nc_getVarByName(nc_id, 'time');
        
        sref = nc_getVarByName(nc_id, 'waterlevel');
        uxref = nc_getVarByName(nc_id, 'x_velocity');
        uyref = nc_getVarByName(nc_id, 'y_velocity');
        
%       close hisfiles
        netcdf.close(nc_id);
        
%       remove old his-files
        cmd = 'rem *his.nc';
        system(cmd);
        
%       run new    
        cmd = [NEWDIR, EXEDIR, EXE, ' ', OPTS, ' ', mdu];
        system(cmd);
        
%       hisfiles
        Dhis = dir([OUTPUTDIR, '*_his.nc']);
        hisname = [OUTPUTDIR, '\', Dhis.name];
        
%       open hisfiles
        nc_id = netcdf.open(hisname);
        
        snew = nc_getVarByName(nc_id, 'waterlevel');
        uxnew = nc_getVarByName(nc_id, 'x_velocity');
        uynew = nc_getVarByName(nc_id, 'y_velocity');
        
%       close hisfiles
        netcdf.close(nc_id);
        
%       output file basename        
        fbase = casedir;
        fbase(fbase=='\') = '_';
        
%       plot
%       loop over stations
        for istat=1:size(sref,1)
%           output file basename
            fnambase = sprintf('%s\\%s_stat_%i', plotloc, fbase, istat);
            
%           title
            stitle = sprintf('%s; station %i', casedir, istat);
            
            figure(1)
            subplot(2,1,1)
            plot(time, sref(istat,:), '-b', time, snew(istat,:), '--r');
            title(stitle,'interpreter','none')
            ylabel('s [m]')
            legend('ref','new')
            subplot(2,1,2)
            plot(time, snew(istat,:)-sref(istat,:));
            xlabel('time [s]')
            ylabel('\Delta s [m]')
%             saveas(gcf,[fnambase, '_s'],'pdf')
            saveas(gcf,[fnambase, '_s'],'png')
            
            figure(2)
            subplot(2,1,1)
            plot(time, uxref(istat,:), '-b', time, uxnew(istat,:), '--r');
            title(stitle,'interpreter','none')
            ylabel('u_x [m/s]')
            legend('ref','new')
            subplot(2,1,2)
            plot(time, uxnew(istat,:)-uxref(istat,:));
            xlabel('time [s]')
            ylabel('\Delta u_x [m/s]')
%             saveas(gcf,[fnambase, '_ux'],'pdf')
            saveas(gcf,[fnambase, '_ux'],'png')
            
            figure(3)
            subplot(2,1,1)
            plot(time, uyref(istat,:), '-b', time, uynew(istat,:), '--r');
            title(stitle,'interpreter','none')
            ylabel('u_y [m/s]')
            legend('ref','new')
            subplot(2,1,2)
            plot(time, uynew(istat,:)-uyref(istat,:));
            xlabel('time [s]')
            ylabel('\Delta u_y [m/s]')
%             saveas(gcf,[fnambase, '_uy'],'pdf')
            saveas(gcf,[fnambase, '_uy'],'png')
        end
        
%       remove his-file
%         cmd = 'rem *his.nc';
%         system(cmd);
        
    end
end

cd(CURDIR);