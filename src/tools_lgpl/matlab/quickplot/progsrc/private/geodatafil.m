function varargout=geodatafil(FI,domain,field,cmd,varargin)
%GEODATAFIL QP support for geodata items.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [TZshift   ,TZstr  ]    = XXXFIL(FI,Domain,DataFld,'timezone')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%
%   The DataFld can only be either an element of the DataProps structure.

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

%========================= GENERAL CODE =======================================
persistent root
T_=1; ST_=2; M_=3; N_=4; K_=5;

GSHHS_types = {'gshhs', 'border', 'river', 'shore lines', 'country and state borders', 'rivers'};
if isequal(FI,'file_exists') % domain is proxy for type
    switch domain
        case GSHHS_types
            type = gshhg('type','type',domain);
            if ~isfield(root,type)
                root.(type) = search_file(type);
            end
            varargout = {~isequal(root.(type),'')};
        otherwise
            varargout = {1};
    end
    return
elseif nargin<2
    error('Not enough input arguments')
elseif nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'getparams'
            varargout={[]};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,Props,0)};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        Parent = varargin{1};
        Ops = varargin{2};
        % hOld=varargin{3};
        %
        switch Props.Name
            case GSHHS_types
                type = gshhg('type','type',Props.Name);
                %
                if ~isfield(root,type)
                    root.(type) = search_file(type);
                end
                %
                hNew = gshhg('plot','rootfolder',root.(type), ...
                    'type',Props.Name, ...
                    'parent',Parent, ...
                    'color','k');
            otherwise
                if ~strncmpi(Props.Name,'wms/',4)
                    error('Unknown geodata plot type: %s',Props.Name)
                end
                [IMG,lon,lat] = wms('image',wms('tms',Props.Name(5:end)),'',get(gca,'xlim'),get(gca,'ylim'));
                hNew = surface(lon,lat,zeros(length(lat),length(lon)), ...
                    'cdata',IMG, ...
                    'facecolor','texturemap', ...
                    'edgecolor','none', ...
                    'cLimInclude','off');
        end
        %
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;


function root = search_file(type)
file = ['binned_' type '_i.nc'];
roots = {qp_basedir('exe') % same folder
    [qp_basedir('exe') filesep 'GSHHG'] % subfolder
    [qp_basedir('exe') filesep '..' filesep 'quickplot' filesep 'bin' filesep 'GSHHG'] % subfolder next to QP (from D3D-MATLAB interface)
    [getenv('D3D_HOME') filesep getenv('ARCH') filesep 'quickplot' filesep 'bin' filesep 'GSHHG'] % subfolder next to QP (from anywhere if environment variables have been defined)
    fileparts(which(file))}; % on the MATLAB search path
for i = 1:length(roots)
    root = roots{i};
    if exist([root filesep file],'file')
        return
    end
end
% can't find the file
root = '';