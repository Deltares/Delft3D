function varargout=ecomsedfil(FI,domain,field,cmd,varargin)
%ECOMSEDFIL QP support for binary ECOM/ECOMSED files.
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
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,Props,varargin{:})};
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
    % initialize and read indices ...
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    Props.SubFld=varargin{1};
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

sz = getsize(FI,Props);
for i=1:length(idx)
   if isequal(idx{i},0) | strcmp(idx{i},':')
      idx{i} = 1:sz(i);
   end
end

% generate output ...
switch FI.SubType
   case 'GCMPLT'
      Mextended=0;
      Nextended=0;
      if strcmp(Props.Name,'grid')
         gidx{M_} = idx{M_};
         gidx{N_} = idx{N_};
      else
         if idx{M_}(end)==sz(M_)
            Mextended = 1;
         end
         gidx{M_} = [idx{M_} idx{M_}(end)+1-Mextended];
         if idx{N_}(end)==sz(N_)
            Nextended = 1;
         end
         gidx{N_} = [idx{N_} idx{N_}(end)+1-Nextended];
      end
      
      if XYRead
         Ans.X = FI.Grid.X(gidx{M_},gidx{N_});
         Ans.Y = FI.Grid.Y(gidx{M_},gidx{N_});
         if Mextended
            Ans.X(end,:) = NaN;
            Ans.Y(end,:) = NaN;
         end
         if Nextended
            Ans.X(:,end) = NaN;
            Ans.Y(:,end) = NaN;
         end
         
         if ~DataInCell & Props.NVal>0
            Ans.X = (Ans.X(1:end-1,1:end-1) + Ans.X(2:end,1:end-1) + Ans.X(1:end-1,2:end) + Ans.X(2:end,2:end))/4;
            Ans.Y = (Ans.Y(1:end-1,1:end-1) + Ans.Y(2:end,1:end-1) + Ans.Y(1:end-1,2:end) + Ans.Y(2:end,2:end))/4;
         end
      end
      
      if DataRead & Props.NVal>0
         if DimFlag(T_)
            if DimFlag(K_)
               Val = ecomsed('read',FI,Props.Field1,idx{T_},idx{M_},idx{N_},idx{K_});
            else
               Val = ecomsed('read',FI,Props.Field1,idx{T_},idx{M_},idx{N_});
            end
         else
            Val = FI.(Props.Field1)(idx{M_},idx{N_});
            Val = reshape(Val, [1 size(Val)]);
            if strcmp(Props.Name,'bed level')
               Val = -Val;
            end
         end
         switch Props.NVal
             case 2
                 if DimFlag(K_)
                     XComp = ecomsed('read',FI,Props.Field1,idx{T_},gidx{M_},idx{N_},idx{K_});
                     YComp = ecomsed('read',FI,Props.Field2,idx{T_},idx{M_},gidx{N_},idx{K_});
                 else
                     XComp = ecomsed('read',FI,Props.Field1,idx{T_},gidx{M_},idx{N_});
                     YComp = ecomsed('read',FI,Props.Field2,idx{T_},idx{M_},gidx{N_});
                 end
                 XComp = (XComp(:,1:end-1,:,:) + XComp(:,2:end,:,:))/2;
                 YComp = (YComp(:,:,1:end-1,:) + YComp(:,:,2:end,:))/2;
                 Ang = reshape(FI.ANG(idx{M_},idx{N_}),[1 length(idx{M_}) length(idx{N_})]);
                 cAng = cos(Ang);
                 sAng = sin(Ang);
                 for t = size(Val,1):-1:1
                     for k = size(Val,4):-1:1
                         Ans.XComp(t,:,:,k) = XComp(t,:,:,k).*cAng - YComp(t,:,:,k).*sAng;
                         Ans.YComp(t,:,:,k) = XComp(t,:,:,k).*sAng + YComp(t,:,:,k).*cAng;
                     end
                 end
             case 1
                 if DimFlag(T_)
                     if DimFlag(K_)
                         Val = ecomsed('read',FI,Props.Field1,idx{T_},idx{M_},idx{N_},idx{K_});
                     else
                         Val = ecomsed('read',FI,Props.Field1,idx{T_},idx{M_},idx{N_});
                     end
                 else
                     Val = FI.(Props.Field1)(idx{M_},idx{N_});
                     Val = reshape(Val, [1 size(Val)]);
                     if strcmp(Props.Name,'bed level')
                         Val = -Val;
                     end
                 end
                 Ans.Val = Val;
         end
         inactive = FI.FSM(idx{M_},idx{N_})==0;
         if isfield(Ans,'Val')
            if ~strcmp(Props.Name,'inactive points')
               Ans.Val(:,inactive) = NaN;
            else
               Ans.Val(:,~inactive) = NaN;
            end
         else
            Ans.XComp(:,inactive) = NaN;
            Ans.YComp(:,inactive) = NaN;
         end
         if isfield(Ans,'XComp') & size(Ans.XComp,1) == 1
            szA = size(Ans.XComp);
            Ans.XComp = reshape(Ans.XComp,[szA(2:end) 1]);
            Ans.YComp = reshape(Ans.YComp,[szA(2:end) 1]);
         elseif isfield(Ans,'Val') & size(Ans.Val,1) == 1
            szA = size(Ans.Val);
            Ans.Val = reshape(Ans.Val,[szA(2:end) 1]);
         end
      end
   case 'GCMTSR'
      if XYRead
         Ans.X = [];
         Ans.Y = [];
      end
      
      if DataRead
         if DimFlag(K_)
            Val = ecomsed('read',FI,Props.Field1,idx{T_},Props.Field2,idx{ST_},idx{K_});
         else
            Val = ecomsed('read',FI,Props.Field1,idx{T_},Props.Field2,idx{ST_});
         end
         if length(Props.Field2)==1
            Ans.Val = Val;
         else
            Ans.XComp = Val(:,1,:);
            Ans.YComp = Val(:,2,:);
         end
      end
end

% read time ...
T=readtim(FI,Props,idx{T_});
Ans.Time=T;

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)

%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units'  'Geom'  'Coords' 'DimFlag' 'DataInCell' 'NVal' 'VecType' 'Loc' 'ReqLoc' 'Field1'   'Field2'     'UseGrid' 'SubFld'};
switch FI.SubType
   case 'GCMPLT'
      DataProps={'grid'             ''       'sQUAD' 'xy'     [0 0 1 1 0]  0          0     ''        ''    ''       ''         ''           1         ''
         'inactive points'          '-'      'sQUAD' 'xy'     [0 0 1 1 0]  1          1     ''        ''    ''       'FSM'      ''           1         ''
         '-------'                  ''       'sQUAD' 'xy'     [0 0 0 0 0]  0          0     ''        ''    ''       ''         ''           1         ''
         'water level'              'm'      'sQUAD' 'xy'     [3 0 1 1 0]  1          1     ''        ''    ''       'ARCET'    ''           1         ''
         'velocity'                 'm/s'    'sQUAD' 'xy'     [3 0 1 1 5]  1          2     ''        ''    ''       'ARCU'     'ARCV'       1         ''
         'unit discharge per layer' 'm^2/s'  'sQUAD' 'xy'     [3 0 1 1 5]  1          2     ''        ''    ''       'ARCUX'    'ARCVX'      1         ''
         '-------'                  ''       'sQUAD' 'xy'     [0 0 0 0 0]  0          0     ''        ''    ''       ''         ''           1         ''
         'tracer concentration'     ''       'sQUAD' 'xy'     [3 0 1 1 0]  1          1     ''        ''    ''       'ARCC'     ''           1         ''
         'cohesive sediment concentration' ...
            'mg/l'                           'sQUAD' 'xy'     [3 0 1 1 0]  1          1     ''        ''    ''       'ARCSED1'  ''           1         ''
         'non-cohesive sediment concentration' ...
            'mg/l'                           'sQUAD' 'xy'     [3 0 1 1 0]  1          1     ''        ''    ''       'ARCSED2'  ''           1         ''
         '-------'                  ''       'sQUAD' 'xy'     [0 0 0 0 0]  0          0     ''        ''    ''       ''         ''           1         ''
         'bed level'                'm'      'sQUAD' 'xy'     [0 0 1 1 0]  1          1     ''        ''    ''       'H'        ''           1         ''
         'bed elevation change'     'cm'     'sQUAD' 'xy'     [3 0 1 1 0]  1          1     ''        ''    ''       'ARCTHIK'  ''           1         ''
         'cohesive sediment-bound tracer concentration' ...
            'ug/l'                           'sQUAD' 'xy'     [3 0 1 1 1]  1          1     ''        ''    ''       'ARCCHEM1' ''           1         ''
         'non-cohesive sediment-bound tracer concentration' ...
            'ug/l'                           'sQUAD' 'xy'     [3 0 1 1 1]  1          1     ''        ''    ''       'ARCCHEM1' ''           1         ''
         'bed concentration sediment-bound tracer'  ...
            'ppm'                            'sQUAD' 'xy'     [3 0 1 1 0]  1          1     ''        ''    ''       'ARCPBED'  ''           1         ''
         '-------'                  ''       'sQUAD' 'xy'     [0 0 0 0 0]  0          0     ''        ''    ''       ''         ''           1         ''
         'distance first dimension' 'm'      'sQUAD' 'xy'     [0 0 1 1 0]  1          1     ''        ''    ''       'DX1'      ''           1         ''
         'distance second dimension' 'm'     'sQUAD' 'xy'     [0 0 1 1 0]  1          1     ''        ''    ''       'DX2'      ''           1         ''
         'grid angle'               'rad'    'sQUAD' 'xy'     [0 0 1 1 0]  1          1     ''        ''    ''       'ANG'      ''           1         ''};
   case 'GCMTSR'
      DataProps={'water level'      'm'      'sQUAD'   'xy'     [3 3 0 0 0]  0          1     ''        ''    ''       'ESAVE'     1           1         ''
         'water depth'              'm'      'sQUAD'   'xy'     [3 3 0 0 0]  0          1     ''        ''    ''       'DZSAVE'    1           1         ''
         'velocity'                 'm/s'    'sQUAD'   'xy'     [3 3 0 0 5]  0          2     ''        ''    ''       'UVSTZSAVE' [1 2]       2         ''
         '-------'                  ''       'sQUAD'   'xy'     [0 0 0 0 0]  0          0     ''        ''    ''       ''         ''           1         ''
         'temperature'              '°C'     'sQUAD'   'xy'     [3 3 0 0 5]  0          1     ''        ''    ''       'UVSTZSAVE' 3           2         ''
         'salinity'                 'psu'    'sQUAD'   'xy'     [3 3 0 0 5]  0          1     ''        ''    ''       'UVSTZSAVE' 4           2         ''
         'tracer concentration'     ''       'sQUAD'   'xy'     [3 3 0 0 0]  0          1     ''        ''    ''       'C1ZSAVE'  ''           2         ''
         'sediment concentration'   ''       'sQUAD'   'xy'     [3 3 0 0 0]  0          1     ''        ''    ''       'CSAVE'    ''           2         ''
         'bed elevation change'     'cm'     'sQUAD'   'xy'     [3 3 0 0 0]  0          1     ''        ''    ''       'THSAVE'   ''           2         ''
         'bed shear stress'         'dynes/cm' 'sQUAD'   'xy'   [3 3 0 0 0]  0          1     ''        ''    ''       'TAUSAVE'  ''           2         ''};
end
%======================== SPECIFIC CODE DIMENSIONS ============================
%======================== DataProps conversion ================================
Out=cell2struct(DataProps,PropNames,2);
%======================== SPECIFIC CODE REMOVE ================================
for i=length(Out):-1:1
   if strcmp(Out(i).Name,'-------')
      % don't remove separators
   elseif strcmp(Out(i).Name,'grid')
      % don't remove grid
   elseif isfield(FI.TimeDepData,Out(i).Field1)
      % don't remove time dependent data fields
   elseif isfield(FI,Out(i).Field1)
      % don't remove constant data fields
   else
      Out(i) = [];
   end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
%======================== SPECIFIC CODE =======================================
switch FI.SubType
   case 'GCMPLT'
      if Props.DimFlag(T_)
         sz(T_) = length(FI.Times);
      end
      sz(M_) = FI.IM;
      sz(N_) = FI.JM;
      if Props.DimFlag(K_)
         sz(K_) = FI.KB;
      end
   case 'GCMTSR'
      if Props.DimFlag(T_)
         sz(T_) = length(FI.Times);
      end
      switch Props.UseGrid
         case 1
            sz(ST_) = FI.EPTS;
         case 2
            sz(ST_) = FI.VPTS;
         case 3
            sz(ST_) = FI.FPTS;
      end
      if Props.DimFlag(K_)
         sz(K_) = FI.TimeDepData.(Props.Field1).size(end);
      end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
T = 1+FI.Times;
if ~isequal(t,0)
   T = T(t);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,s)
%======================== SPECIFIC CODE =======================================
switch FI.SubType
   case 'GCMTSR'
      switch Props.UseGrid
         case 1
            S = multiline(sprintf('(%i,%i)\n',[FI.INXIE;FI.INXJE]),'cell');
         case 2
            S = multiline(sprintf('(%i,%i)\n',[FI.INXIV;FI.INXJV]),'cell');
      end
      S(end) = [];
end
if nargin>2 & ~isequal(s,0)
   S = S(s);
end
% -----------------------------------------------------------------------------
