function valo=qp_settings(param,val)
%QP_SETTINGS Routine to store and retreive settings.
%   VAL = QP_SETTINGS(PARAM,DEFVAL) obtain the value of parameter
%   Options/PARAM. If no specific value has been set by the user/system
%   then the default value DEFVAL will be returned. The DEFVAL argument is
%   optional; if no DEFVAL is provided, the function will result in an
%   error if no value has been set by the user/system.
%
%   VAL = QP_SETTINGS({GRP PARAM},DEFVAL) uses the key in the group GRP
%   rather than the default group 'Options'.
%
%   QP_SETTINGS(PARAM,VAL) set value of parameter Options/PARAM to VAL.
%
%   QP_SETTINGS({GRP PARAM},VAL) set value of GRP/PARAM to VAL.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2023 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

persistent Settings qppref
if isempty(Settings)
    qppref=fullfile(qp_basedir('pref'),'Delft-QUICKPLOT.ini');
    Settings=qp_read_settings(qppref);
end

grp='Options';
if iscell(param)
    grp=param{1};
    param=param{2};
end
if nargout==1
    % retrieve value
    if length(param)>6 && strcmpi(param(end-5:end),'string')
        cmd='cgetstring';
    else
        cmd='cget';
    end
    if nargin>1
        dval = val;
    else
        dval = {};
    end
    dval = qp_settings_default(param, dval);
    valo = inifile(cmd, Settings, grp, param, dval);
    if length(valo)==1
        valo = valo{1};
    end
elseif isequal(param,'<SAVE>')
    Settings=qp_write_settings(Settings,qppref);
else
    Settings=inifile('set',Settings,grp,param,val);
end


function val=qp_settings_default(param,defval)
steelblue3 = [79 148 205]/255;
if isunix
   Set.UIActiveColor      = [1 1 1];           %steelblue3;
   Set.UIInActiveColor    = [235 233 237]/255; %steelblue3;
   Set.UIForeGroundColor  = [0 0 0];           %[1 1 1];
else
   Set.UIActiveColor      = [1 1 1];
   Set.UIInActiveColor    = get(0,'factoryuicontrolbackgroundcolor');
   if isequal(Set.UIInActiveColor,[0.94 0.94 0.94]) % HG2 in R2013b shows white for exactly this color in listboxes
       Set.UIInActiveColor    = [0.93 0.93 0.93];
   end
   Set.UIForeGroundColor  = get(0,'factoryuicontrolforegroundcolor');
end
Set.UIButtonMargin     = 0; %5;
Set.UIFontAngle        = get(0,'DefaultUicontrolFontAngle');
Set.UIFontName         = get(0,'DefaultUicontrolFontName');
Set.UIFontUnits        = get(0,'DefaultUicontrolFontUnits');
Set.UIFontSize         = get(0,'DefaultUicontrolFontSize');
Set.UIFontWeight       = get(0,'DefaultUicontrolFontWeight');
Set.figuredir          = '';
Set.graphicssmoothing  = 0;
Set.gridviewbackgroundcolor   = [230 230 230];
Set.gridviewgridcolor         = [0 153 153];
Set.gridviewselectioncolor    = [255 0 0];
Set.gridviewlandboundarycolor = [0 0 0];
Set.gridviewshowindices       = 1;
Set.defaultfigure             = '';
Set.defaultfigurepos          = 'auto';
Set.defaultfigurecolor        = get(0,'factoryuicontrolbackgroundcolor')*255;
Set.defaultaxescolor          = [255 255 255];
Set.boundingbox               = 0;
Set.v6zoombehavior            = 0;
Set.colorbar_ratio            = 25;
Set.print_ID                  = 'PDF file';
Set.print_method              = 2;
Set.print_DPI                 = 150;
Set.print_colour              = 1;
Set.print_inverthardcopy      = 1;
Set.print_pagelabels          = 1;
Set.organizationname          = 'Deltares';
Set.filefilterselection       = '"ARC/INFO Ascii Grid Files","Delft3D Grid Files","Delft3D Output Files","Delft3D-FLOW Bound. Cond. Files","Delft3D/SOBEK Meteo Files","Delwaq Binary Files","Delwaq Time Series Input Files","NetCDF Files","Sample Files","Simona SDS Files","Sobek Networks","Tekal Data Files"';
Set.debugging                 = 0;
Set.showinactiveopt           = 0;
Set.showversion               = 'off';
Set.stopruniferror            = 1;
Set.timezone                  = 'Ignored';
Set.export_max_ntimes         = 10;
%
Set.netcdf_use_fillvalue      = 'valid_range';
%
Set.delwaq_names              = 'expanded';
Set.delwaq_procdef            = 'auto';
%
Set.shipma_distance_along_desired_track = 1;
Set.shipma_spacestep          = 500; %m
Set.shipma_tickwidth          = 200; %m
Set.shipma_timestep           = 300; %s
Set.shipma_figa               = 1;
Set.shipma_figa_fairway       = 1;
Set.shipma_figa_banksuction   = 0;
Set.shipma_figa_depth         = 1;
Set.shipma_figa_contourstep   = 5;
Set.shipma_figa_contourmax    = 20;
Set.shipma_figa1              = 0;
Set.shipma_figa1_zoombox      = [NaN NaN NaN NaN];
Set.shipma_figa2               = 1;
Set.shipma_figa2_quantity      = 'waves';
Set.shipma_figa2_contourstep   = 0.05;
Set.shipma_figa2_contourmax    = 0.25;
Set.shipma_figb               = 1;
Set.shipma_figc               = 1;
Set.shipma_figd               = 1;
Set.shipma_figd_wind          = 1;
Set.shipma_figd_waves         = 1;
Set.shipma_figd_swell         = 1;
Set.shipma_figd_banksuction   = 1;
Set.shipma_fige               = 1;
Set.shipma_fige_tugs          = 1;
Set.shipma_fige_thrusters     = 1;
Set.shipma_bordertext1_string = '%caption%';
Set.shipma_bordertext2_string = '%project%';
Set.shipma_bordertext3_string = '%case%';
Set.shipma_bordertext4_string = '%shipma%';
Set.shipma_bordertext5_string = '';
Set.shipma_bordertext6_string = '%fignr%';
Set.shipma_bordertext7_string = '%organization%';
%
if isfield(Set,param)
    val=Set.(param);
elseif ~iscell(defval)
    val=defval;
else
    error('Unknown parameter: %s',param)
end


function Prefs=qp_read_settings(qppref)
try
    Prefs=inifile('open',qppref);
catch
    %Try old directory
    c = computer;
    if c(1:2) == 'PC'
        qppref = strrep(qppref,'Deltares','Deltares');
    else % Unix
        qppref = strrep(qppref,'Deltares','DelftHydraulics');
    end
    try
        Prefs=inifile('open',qppref);
    catch
        Prefs=inifile('new');
    end
end


function Prefs=qp_write_settings(Prefs,qppref)
try
    Prefs=inifile('write',qppref,Prefs);
catch
end
