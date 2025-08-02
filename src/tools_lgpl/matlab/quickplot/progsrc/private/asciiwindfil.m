function varargout=asciiwindfil(FI,domain,field,cmd,varargin)
%ASCIIWINDFIL QP support for Delft3D meteo files.
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
%   Copyright (C) 2011-2025 Stichting Deltares.                                     
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

T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
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
        return;
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
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

subf=getsubfields(FI,Props);
fidx=find(DimFlag);
if isempty(subf)
    % initialize and read indices ...
    idx={[] [] 0 0 0};
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    Props.SubFld=varargin{1};
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

% select appropriate timestep ...
sz=getsize(FI,Props);
if DimFlag(T_)
    if isempty(idx{T_})
        idx{T_}=sz(T_);
    end
    if isequal(idx{T_},0)
        idx{T_}=1:sz(T_);
    end
end

%========================= GENERAL CODE =======================================
allidx=zeros(size(sz));
for i=[M_ N_ K_]
    if DimFlag(i)
        if isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        elseif ~isequal(idx{i},idx{i}(1):idx{i}(end))
            error('Only scalars or ranges allowed for index %i',i)
        end
    end
end

if ~isempty(idx{T_}) && max(idx{T_})>sz(T_) && ~(isequal(idx{T_},1) && sz(T_)==0)
    error('Selected timestep (%i) larger than number of timesteps (%i) in file.',max(idx{T_}),sz(T_))
end

% read grid ...
x=[];
y=[];
if XYRead
    switch FI.Header.filetype
        case {'meteo_on_equidistant_grid','field_on_equidistant_grid', ...
                'meteo_on_computational_grid','field_on_computational_grid'}
            gidx{M_} = [idx{M_} max(idx{M_})+1];
            gidx{N_} = [idx{N_} max(idx{N_})+1];
        otherwise
            gidx = idx;
    end
    [x,y,grid_unit] = asciiwind('grid',FI,idx{T_},gidx{[M_ N_]});
    switch FI.Header.filetype
        case {'meteo_on_equidistant_grid','field_on_equidistant_grid','meteo_on_computational_grid','field_on_computational_grid'}
            if ~DataInCell && Props.NVal>0
                [x,y] = corner2center(x,y);
            end
    end
end

val2 = [];
if DataRead && Props.NVal>0
    switch FI.Header.filetype
        case {'meteo_on_computational_grid','field_on_computational_grid'}
            idx{M_} = idx{M_}+1;
            idx{N_} = idx{N_}+1;
        otherwise
            %nothing to do
    end
    val1 = asciiwind('read',FI,Props.Q(1),idx{T_},idx{[M_ N_]});
    if Props.NVal==2
        if length(Props.Q)==2
            val2 = asciiwind('read',FI,Props.Q(2),idx{T_},idx{[M_ N_]});
        elseif FI.Header.quantity{1}(1)=='x'
            val2 = asciiwind('read',FI.Vector,1,idx{T_},idx{[M_ N_]});
        else
            val2 = val1;
            val1 = asciiwind('read',FI.Vector,1,idx{T_},idx{[M_ N_]});
        end
        if strcmp(FI.Header.quantity{Props.Q(1)},'wind_speed') % and direction
            d2r  = pi/180;
            val2 = val2*d2r;
            valx = -val1.*sin(val2);
            val2 = -val1.*cos(val2);
            val1 = valx;
            valx = [];
        end
    end
else
    Props.NVal=0;
end

if length(idx{T_}) <= 1
    if XYRead
        szx = [size(x) 1];
        x = reshape(x,szx(2:end));
        y = reshape(y,szx(2:end));
    end
    if DataRead && Props.NVal>0
        szx = [size(val1) 1];
        val1 = reshape(val1,szx(2:end));
        if ~isempty(val2)
            val2 = reshape(val2,szx(2:end));
        end
    end
end

% generate output ...
if XYRead
    Ans.X=x;
    Ans.Y=y;
    if ~isempty(grid_unit)
        Ans.XUnits = grid_unit;
        Ans.YUnits = grid_unit;
    end
end
if Props.NVal==0
elseif isempty(val2)
    Ans.Val=val1;
else
    Ans.XComp=val1;
    Ans.YComp=val2;
end

if Props.DimFlag(T_)
    % read time ...
    Ans.Time=readtim(FI,Props,idx{T_});
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'               'Units'  'DimFlag' 'DataInCell' 'NVal'   'Sign'  'Q' };
DataProps={'grid'               ''        [0 0 1 1 0]  0         0         0      0
    '-------'                   ''        [0 0 0 0 0]  0         0         0      0
    'data'                      ''        [1 0 1 1 0]  0         1         1      1  };
%------------------------------------------------------------------------------
Out = cell2struct(DataProps,PropNames,2);
%------------------------------------------------------------------------------
switch FI.Header.filetype
    case {'meteo_on_equidistant_grid','field_on_equidistant_grid'}
        Out(3).DataInCell = 1;
    case {'meteo_on_computational_grid','field_on_computational_grid'}
        Out(3).DataInCell = 1;
    case {'meteo_on_spiderweb_grid','field_on_spiderweb_grid'}
        Out(1).DimFlag(1) = 1;
end
%
Out(2+(1:FI.Header.n_quantity)) = Out(3);
ntimes = length(FI.Data);
processed = zeros(1,FI.Header.n_quantity);
for q = 1:FI.Header.n_quantity
    if processed(q)
        continue
    end
    Out(2+q).Name  = FI.Header.quantity{q};
    Out(2+q).Units = FI.Header.unit{q};
    Out(2+q).Q     = q;
    if ntimes==0
        Out(2+q).DimFlag(1) = 0;
    end
    if isfield(FI,'Vector')
        Out(2+q).Name = 'wind velocity';
        Out(2+q).NVal = 2;
    else
        switch Out(2+q).Name
            case 'x_wind'
                name2 = 'y_wind';
            case 'y_wind'
                name2 = 'x_wind';
            case 'wind_speed'
                name2 = 'wind_from_direction';
            otherwise
                continue
        end
        q2 = find(strncmp(name2,FI.Header.quantity,length(name2)));
        if ~isempty(q2)
            Out(2+q).Name = 'wind velocity';
            Out(2+q).NVal = 2;
            if strcmp(name2,'x_wind')
                Out(2+q).Q    = [q2 q];
            else
                Out(2+q).Q    = [q q2];
            end
            processed(q2) = 1;
        end
    end
end
if any(processed)
    Out(2+find(processed)) = [];
end
%--- set UseGrid options ...
[Out(:).UseGrid]=deal(1);
%switch FI.Header.filetype
%    case {'meteo_on_equidistant_grid','field_on_equidistant_grid'}
%        [Out(:).MName]=deal('row');
%        [Out(:).NName]=deal('column');
%    case {'meteo_on_spiderweb_grid','field_on_spiderweb_grid'}
%        [Out(:).MName]=deal('radius');
%        [Out(:).NName]=deal('direction');
%    case {'meteo_on_curvilinear_grid','field_on_curvilinear_grid', ...
%          'meteo_on_flow_grid','field_on_flow_grid'}
%end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];

%======================== SPECIFIC CODE =======================================
if Props.DimFlag(M_) && Props.DimFlag(N_)
    switch lower(FI.Header.filetype)
        case {'meteo_on_equidistant_grid','field_on_equidistant_grid'}
           sz([M_ N_])=[FI.Header.n_rows FI.Header.n_cols];
        case {'meteo_on_spiderweb_grid','field_on_spiderweb_grid'}
           sz([M_ N_])=[FI.Header.n_rows FI.Header.n_cols]+1;
        case {'meteo_on_curvilinear_grid','field_on_curvilinear_grid'}
           sz([M_ N_])=size(FI.Header.grid_file.X);
        case {'meteo_on_computational_grid','field_on_computational_grid'}
           sz([M_ N_])=size(FI.Header.grid_file.X)-1;
    end
end
if Props.DimFlag(T_)
    sz(T_)=length(FI.Data);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
if t==0
    T = [FI.Data.time];
else
    T = [FI.Data(t).time];
end
% -----------------------------------------------------------------------------

