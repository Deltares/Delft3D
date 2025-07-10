function varargout = analytical_solution(FI,idom,field,cmd,varargin)
%ANALYTICAL_SOLUTION QP support for an analytical solution.
%   Domains                 = XXXXXX(FI,[],'domains')
%   DataProps               = XXXXXX(FI,Domain)
%   Size                    = XXXXXX(FI,Domain,DataFld,'size')
%   Times                   = XXXXXX(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXXXX(FI,Domain,DataFld,'stations')
%   SubFields               = XXXXXX(FI,Domain,DataFld,'subfields')
%   [TZshift   ,TZstr  ]    = XXXXXX(FI,Domain,DataFld,'timezone')
%   [Data      ,NewFI]      = XXXXXX(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXXXX(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXXXX(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXXXX(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXXXX(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXXXX(FI,[],'options',OptionsFigure,OptionsCommand, ...)
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

if nargin==1
    varargout{1} = select_analytical_solution(FI);
    return
elseif nargin<2
    error('Not enough input arguments')
end

if nargin==2
    varargout={infile(FI,idom)};
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
        varargout={getsize(FI,idom,Props)};
        return
    case 'times'
        varargout={readtim(FI,idom,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,idom,Props);
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

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);
subf=getsubfields(FI,Props);
if isempty(subf)    
    idx_subf = [];
    idx(fidx(1:length(varargin))) = varargin;
else
    idx_subf = varargin{1};
    idx(fidx(1:(length(varargin)-1))) = varargin(2:end);
end

sz = getsize(FI,idom,Props);
allidx=zeros(size(sz));
for i=1:length(sz)
    if DimFlag(i)
        if i == T_ && isempty(idx{i})
            idx{T_}=sz(T_);
        elseif i == ST_ && isempty(idx{i})
            % skip --> nothing
        elseif  isempty(idx{i}) || isequal(idx{i},0) || isequal(idx{i},1:sz(i))
            idx{i}=1:sz(i);
            allidx(i)=1;
        end
    end
end

par = FI.Args;
switch FI.Type
    case 'Dam break'
        x = par.xLeft + (0:par.nGridSteps-1)' * (par.xRight - par.xLeft)/par.nGridSteps;
        x = x(idx{M_});
        Ans = evaluate_dambreak(Props.Name,x,par,idx{T_});
        Ans.Time = readtim(FI,idom,Props,idx{T_});
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,idom)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'SubFld' 'MNK' 'varid'  'DimName' 'hasCoords' 'VectorDef' 'ClosedPoly' 'UseGrid'};
DataProps={'-------'                ''      ''     ''      [0 0 0 0 0]  0           0      []       0     []          {}          0         0          0          0};
Out=cell2struct(DataProps,PropNames,2);
switch FI.Type
    case 'Dam break'
        Out(1).Geom = 'sQUAD';
        Out(1).Coords = 'x';
        Out(2:3) = Out(1);
        
        Out(1).Name = 'water level';
        Out(1).Units = 'm';
        Out(1).NVal = 1;
        Out(1).DimFlag([T_,M_]) = 1;
        
        Out(2).Name = 'water depth';
        Out(2).Units = 'm';
        Out(2).NVal = 1;
        Out(2).DimFlag([T_,M_]) = 1;
        
        Out(3).Name = 'velocity';
        Out(3).Units = 'm/s';
        Out(3).NVal = 1;
        Out(3).DimFlag([T_,M_]) = 1;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz = getsize(FI,idom,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
ndims = length(Props.DimFlag);
sz = zeros(1,ndims);
%
par = FI.Args;
switch FI.Type
    case 'Dam break'
        sz(M_) = par.nGridSteps;
        sz(T_) = par.nTimeSteps;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T = readtim(FI, idom, Props, t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
if nargin <4 || isequal(t,0)
    sz = getsize(FI,idom,Props);
    t = 1:sz(T_);
end
%
par = FI.Args;
switch FI.Type
    case 'Dam break'
        T = datenum(par.refDate) + (t-1) * par.dt * timeStep(par.dtUnitStr) / 86400;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function dtUnit = timeStep(dtUnitString)
switch dtUnitString
    case 'seconds'
        dtUnit = 1;
    case 'minutes'
        dtUnit = 60;
    case 'hours'
        dtUnit = 3600;
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Data = select_analytical_solution(defaults)      
cases = {'Dam break'};
[SelectedType,SelectedName,SelectedNr] = ui_typeandname(cases);
Data = [];
if SelectedNr > 0
    switch SelectedType
        case 'Dam break'
            fields = get_dambreak_parameters;
    end
    if ~isempty(defaults)
        for i = 1:size(fields,1)
            if isfield(defaults,fields{i,1})
                fields{i,5} = defaults.(fields{i,1});
            end
        end
    end
    SelectedArgs = md_dialog([SelectedName ' settings...'], ...
        fields(:,2), ...
        fields(:,3), ...
        fields(:,4), ...
        fields(:,5));
    if ~isempty(SelectedArgs)
        Data.Type = SelectedType;
        Data.Name = SelectedName;
        Data.Args = cell2struct(SelectedArgs,fields(:,1));
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function fields = get_dambreak_parameters
tUnits = {'seconds','minutes','hours'};
fields = {
    'xLeft'     , 'X Left [m]'               , 'editreal'    , [-inf inf], 0
    'nGridSteps', 'Number of Grid Points [-]', 'editint'     , [2 inf]   , 500
    'xRight'    , 'X Right [m]'              , 'editreal'    , [-inf inf], 1000 % range should be >= xLeft
    'zb'        , 'Bed Level [m]'            , 'editreal'    , [-inf inf], 0
    'xDamBreak' , 'X Dam Break [m]'          , 'editreal'    , [-inf inf], 500 % range should be xLeft < xDamBreak < xRight
    'hLeft'     , 'Water Depth Left [m]'     , 'editreal'    , [0 inf]   , 2
    'hRight'    , 'Water Depth Right [m]'    , 'editreal'    , [0 inf]   , 0.01 % hLeft and hRight should not both be zero
    'g'         , 'Gravity [m/s2]'           , 'editreal'    , [0 inf]   , 9.81
    'dt'        , 'Time Step'                , 'editreal'    , [0 inf]   , 1
    'dtUnitStr' , 'Time Step Unit'           , 'popupmenu'   , tUnits    , tUnits{1}
    'refDate'   , 'Since / Reference Date'   , 'editdatetime', 'datetime' , '01-Jan-2020 00:00:00'
    'nTimeSteps', 'Number of Time Steps [-]' , 'editint'     , [1 inf]   , 100                   };
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Ans = evaluate_dambreak(qnt,x,par,it)
[h,u] = dambreak(x,par.xDamBreak,par.hLeft,par.hRight,par.g,(it-1)*par.dt*timeStep(par.dtUnitStr));
%
Ans.X = x;
switch qnt
    case 'water level'
        Ans.Val = par.zb+h;
    case 'water depth'
        Ans.Val = h;
    otherwise
        Ans.Val = u;
end
if numel(it)>1
    Ans.Val = Ans.Val';
end