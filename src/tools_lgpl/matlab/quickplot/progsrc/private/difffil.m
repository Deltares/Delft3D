function varargout=difffil(FI,domain,field,cmd,varargin)
%DIFFFIL QP support for file differences.
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
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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

if ~isfield(FI,'Files')
    FIo = FI;
    FI = [];
    FI.Files = FIo;
    FI.DiffType = 'index';
end

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
        varargout={getsize(FI,domain,Props)};
        return
    case 'times'
        varargout={readtim(FI,domain,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,domain,Props,varargin{:})};
        return
    case 'subfields'
        varargout={getsubfields(FI,domain,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

if ~isfield(FI,'DiffDomain') || isempty(FI.DiffDomain)
    domain1 = 1;
    domain2 = 1;
else
    domain1 = FI.DiffDomain(1);
    domain2 = FI.DiffDomain(2);
end

check_renumbering = strcmp(FI.DiffType,'renum');
check_shared_locs = strcmp(FI.DiffType,'shared');
check_grids = check_renumbering || check_shared_locs;
JRI1 = [];
JRI2 = [];
if isempty(Props.SubFld)
    subf = {};
    nSubf = 0;
else % subfld index is the first index of varargin, so pass that one ...
    subf = varargin{1};
    nSubf = 1;
end
if Props.NVal>0 && check_grids
    [~,Grid1,FI.Files(1)] = qp_getdata(FI.Files(1),domain1,Props.Q1,'grid',subf{:});
    [~,Grid2,FI.Files(2)] = qp_getdata(FI.Files(2),domain2,Props.Q2,'grid',subf{:});
    if isfield(Grid1,'ValLocation')
        if check_renumbering
            JRI2 = determine_renum(Grid1, Grid2);
        elseif check_shared_locs
            [JRI1, JRI2] = determine_shared(Grid1, Grid2);
        end
    end
end

% determine M index in varargin/args array ...
iM = nSubf + sum(Props.DimFlag(1:M_)~=0);
args = apply_renum(varargin, JRI1, iM);
[success,Ans,FI.Files(1)] = qp_getdata(FI.Files(1),domain1,Props.Q1,cmd,args{:});
if ~success
    error(lasterr)
end

if Props.NVal>0
    FI = guarantee_options(FI);
    diffval = qp_option(FI,'diffval');
    
    cmd = strrep(cmd,'grid','');
    args = apply_renum(varargin, JRI2, iM);
    [success,Data2,FI.Files(2)] = qp_getdata(FI.Files(2),domain2,Props.Q2,cmd,args{:});
    if ~success
        error(lasterr)
    end
    %
    cflds = {'Val','XComp','YComp','ZComp','XDamVal','YDamVal'};
    for cfld = cflds
        fld = cfld{1};
        if isfield(Data2,fld)
            v1 = Ans.(fld);
            v2 = Data2.(fld);
            switch diffval
                case 'val1-val2'
                    v1 = v1 - v2;
                case 'only-val1'
                    v1(~isnan(v2)) = NaN;
                case 'only-val2'
                    v2(~isnan(v1)) = NaN;
                    v1 = v2;
            end
            Ans.(fld) = v1;
        end
    end
    %
    % temperature units are always relative or unspecified
    %
    if isfield(Ans,'AbsoluteUnits')
        Ans = rmfield(Ans,'AbsoluteUnits');
        Ans.TemperatureType = 'relative';
    elseif isfield(Ans,'TemperatureType')
        % The difference of two temperature quantities will always be relative
        % however only if the base quantities are simple temperatures and not
        % if the quantity is e.g. a temperature flux or square of temperature
        % So, keep TemperatureType unspecified unchanged.
        if ~strcmp(Ans.TemperatureType,'unspecified')
            Ans.TemperatureType = 'relative';
        end
    end
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


function args = apply_renum(args, JRI, i)
if ~isempty(JRI)
    if isequal(args{i},0)
        args{i} = JRI;
    else
        valid = args{i}<=numel(JRI);
        args{i} = JRI(args{i}(valid));
    end
end


function JRI = determine_renum(Grid1, Grid2)
X1 = Grid1.X;
Y1 = Grid1.Y;
%
X2 = Grid2.X;
Y2 = Grid2.Y;
%
[~,I1] = sort(Y1);
[~,I2] = sort(X1(I1));
I = I1(I2);
%
[~,J1] = sort(Y2);
[~,J2] = sort(X2(J1));
J = J1(J2);
%
if ~isequal(X1(I),X2(J)) || ~isequal(Y1(I),Y2(J))
    error('The node coordinates of the two meshes are not equal.')
end
%
switch Grid1.ValLocation
    case 'NODE'
        [~,RI] = sort(I);
        JRI = J(RI);
    case 'EDGE'
        [~,RI]=sort(I);
        [~,RJ]=sort(J);
        %
        ENC1 = RI(Grid1.EdgeNodeConnect);
        [ENC1,eI] = sortrows(ENC1);
        %
        ENC2 = RJ(Grid2.EdgeNodeConnect);
        [ENC2,eJ] = sortrows(ENC2);
        %
        if ~isequal(ENC1,ENC2)
            dENC = ~all(ENC1==ENC2,2);
            ei = eI(dENC);
            ej = eJ(dENC);
            error('The edge-node connectivity of the two meshes are not equal.\nCheck edge %i of mesh 1, and face %i of mesh 2.',ei(1),ej(1))
        end
        %
        [~,eRI] = sort(eI);
        JRI = eJ(eRI);
    case 'FACE'
        [~,RI] = sort(I);
        [~,RJ] = sort(J);
        %
        FNC1 = Grid1.FaceNodeConnect;
        Mask = isnan(FNC1);
        FNC1(Mask) = 1;
        FNC1 = RI(FNC1);
        FNC1(Mask) = 0;
        [FNC1,fI] = sortrows(FNC1);
        %
        FNC2 = Grid2.FaceNodeConnect;
        Mask = isnan(FNC2);
        FNC2(Mask) = 1;
        FNC2 = RJ(FNC2);
        FNC2(Mask) = 0;
        [FNC2,fJ] = sortrows(FNC2);
        %
        if ~isequal(FNC1,FNC2)
            nPnt = min(size(FNC1,2),size(FNC2,2));
            if isequal(FNC1(:,1:nPnt),FNC2(:,1:nPnt)) && ...
                    all(all(FNC1(:,nPnt+1:end)==0)) && ...
                    all(all(FNC2(:,nPnt+1:end)==0))
                % still okay
            else
                nPnt = max(size(FNC1,2),size(FNC2,2));
                if nPnt > size(FNC1,2)
                    FNC1(:,end+1:nPnt) = 0;
                else
                    FNC2(:,end+1:nPnt) = 0;
                end
                dFNC = ~all(FNC1==FNC2,2);
                fi = fI(dFNC);
                fj = fJ(dFNC);
                error('The face-node connectivity of the two meshes are not equal.\nCheck face %i of mesh 1, and face %i of mesh 2.',fi(1),fj(1))
            end
        end
        %
        [~,fRI] = sort(fI);
        JRI = fJ(fRI);
end


function [JRI1, JRI2] = determine_shared(Grid1, Grid2)
XY1 = Grid1.X + 1i * Grid1.Y;
XY2 = Grid2.X + 1i * Grid2.Y;
[~, JRI1_node, JRI2_node] = intersect(XY1, XY2);
%
switch Grid1.ValLocation
    case 'NODE'
        JRI1 = JRI1_node;
        JRI2 = JRI2_node;
        
    case 'EDGE'
        reverse_JRI1 = NaN(size(XY1));
        reverse_JRI1(JRI1_node) = 1:length(JRI1_node);
        ENC1 = reverse_JRI1(Grid1.EdgeNodeConnect);
        %
        reverse_JRI2 = NaN(size(XY2));
        reverse_JRI2(JRI2_node) = 1:length(JRI2_node);
        ENC2 = reverse_JRI2(Grid2.EdgeNodeConnect);
        %
        [~,JRI1,JRI2] = intersect(ENC1,ENC2,'rows');
        
    case 'FACE'
        reverse_JRI1 = NaN(size(XY1));
        reverse_JRI1(JRI1_node) = 1:length(JRI1_node);
        %
        FNC1 = Grid1.FaceNodeConnect;
        Mask = isnan(FNC1);
        FNC1(Mask) = 1;
        FNC1 = reverse_JRI1(FNC1);
        FNC1(Mask) = 0;
        %
        reverse_JRI2 = NaN(size(XY2));
        reverse_JRI2(JRI2_node) = 1:length(JRI2_node);
        %
        FNC2 = Grid2.FaceNodeConnect;
        Mask = isnan(FNC2);
        FNC2(Mask) = 1;
        FNC2 = reverse_JRI2(FNC2);
        FNC2(Mask) = 0;
        %
        [~,JRI1,JRI2] = intersect(FNC1,FNC2,'rows');

end


% -----------------------------------------------------------------------------
function Domains=domains(FI)
[success,D1] = qp_getdata(FI.Files(1),'domains');
[success,D2] = qp_getdata(FI.Files(2),'domains');

if ~isfield(FI,'DiffDomain') || isempty(FI.DiffDomain)
    if isempty(D1) && isempty(D2)
        Domains = {};
    elseif isequal(D1,D2)
        Domains = D1;
    elseif length(D1)<=1 && length(D2)<=1
        if length(D1)==1 && isempty(D2)
            Domains = D1;
        elseif isempty(D1) && length(D2)==1
            Domains = D2;
        else % length(D1)==1 && length(D2)==1
            Domains = { sprintf('%s/%s',D1{1},D2{1}) };
        end
    else
        error('Multiple different domains not supported.')
    end
else
    if isempty(D1) && isempty(D2)
        Domains = {};
    elseif isempty(D1)
        Domains = D2(FI.DiffDomain(2));
    else
        D1 = D1(FI.DiffDomain(1));
        if isempty(D2)
            Domains = D1;
        else
            D2 = D2(FI.DiffDomain(2));
            if isequal(D1,D2)
                Domains = D1;
            else
                Domains = { sprintf('%s/%s',D1{1},D2{1}) };
            end
        end
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
warnings = {};
if ~isfield(FI,'DiffDomain') || isempty(FI.DiffDomain)
    domain1 = 1;
    domain2 = 1;
else
    domain1 = FI.DiffDomain(1);
    domain2 = FI.DiffDomain(2);
end
[success,Q1] = qp_getdata(FI.Files(1),domain1);
if ~success
    error(lasterr)
end
[success,Q2] = qp_getdata(FI.Files(2),domain2);
if ~success
    error(lasterr)
end
%
Out = cell2struct(cell(12,length(Q1)),{'Name','Units','Geom','Coords','DimFlag','DataInCell','NVal','Q1','Q2','Size','SubFld','ClosedPoly'});
q2 = {Q2.Name};
j = 0;
for i=1:length(Q1)
    if strcmp(Q1(i).Name,'-------')
        j=j+1;
        Out(j).Name = Q1(i).Name;
        Out(j).Units = '';
        Out(j).Geom = '';
        Out(j).Coords = '';
        Out(j).DimFlag = [0 0 0 0 0];
        Out(j).DataInCell = 0;
        Out(j).NVal = 0;
        Out(j).Q1 = [];
        Out(j).Q2 = [];
        Out(j).Size = [];
        Out(j).SubFld = {};
        Out(j).ClosedPoly = 0;
        continue
    end
    if Q1(i).NVal<0
        % cannot diff dedicated plots
        continue
    end
    i2 = strmatch(Q1(i).Name,q2,'exact');
    if isempty(i2)
        % no match found
        continue
    end
    [success,sz] = qp_getdata(FI.Files(1),domain1,Q1(i),'size');
    if ~success
        error(lasterr)
    end
    [success,sf] = qp_getdata(FI.Files(1),domain1,Q1(i),'subfields');
    if ~success
        error(lasterr)
    end
    for k=length(i2):-1:1
        if ~isequal(Q1(i).NVal,Q2(i2(k)).NVal)
            % cannot take difference of scalar and vector or similar
            % mismatches.
            i2(k)=[];
        elseif ~isequal(Q1(i).DimFlag,Q2(i2(k)).DimFlag)
            % the dimension should match, although a mismatch in time
            % dependence and stations might be acceptable in the future.
            i2(k)=[];
        else
            [success,sz2] = qp_getdata(FI.Files(2),domain2,Q2(i2(k)),'size');
            if ~success
                error(lasterr)
            end
            if ~isequal(sz(T_),sz2(T_))
                % allow for a different number of time steps, but warn user
                % about it.
                szt = min(sz(T_),sz2(T_));
                warnings{end+1} = sprintf('File 1 contains %i time steps, file 2 contains %i time steps. Diff supported for first %i time steps.',sz(T_),sz2(T_),szt);
                sz(T_) = szt;
                sz2(T_) = szt;
            end
            if strcmp(FI.DiffType,'shared')
                sz = min(sz,sz2);
                sz2 = sz;
            end
            if ~isequal(sz,sz2)
                % allow for different number of stations in the future?
                i2(k)=[];
            else
                [success,sf2] = qp_getdata(FI.Files(2),domain2,Q2(i2(k)),'subfields');
                if ~success
                    error(lasterr)
                end
                if ~isequal(sf,sf2)
                    % might be acceptable in the future to select matching
                    % subfields, e.g. corresponding sediment fractions.
                    i2(k)=[];
                end
            end
        end
    end
    if length(i2)~=1
        % unable to uniquely identify the corresponding quantity
        continue
    end
    %
    NewFld.Name = Q1(i).Name;
    NewFld.Units = Q1(i).Units;
    if isfield(Q1,'Geom')
        NewFld.Geom = Q1(i).Geom;
    else
        NewFld.Geom = '';
    end
    if isfield(Q1,'Coords')
        NewFld.Coords = Q1(i).Coords;
    else
        NewFld.Coords = '';
    end
    if ~isequal(NewFld.Units,Q2(i2).Units)
        % might be able to relax this to compatible units later.
        continue
    end
    NewFld.DimFlag = Q1(i).DimFlag;
    NewFld.DataInCell = Q1(i).DataInCell;
    if ~isequal(NewFld.DataInCell,Q2(i2).DataInCell)
        % should not be too restrictive
        continue
    end
    NewFld.NVal = Q1(i).NVal;
    if NewFld.NVal~=0
        NewFld.Name = [NewFld.Name ' difference'];
    end
    NewFld.Q1 = Q1(i);
    NewFld.Q2 = Q2(i2);
    NewFld.Size = sz;
    NewFld.SubFld = sf;
    if isfield(Q1,'ClosedPoly')
        NewFld.ClosedPoly = Q1(i).ClosedPoly;
    else
        NewFld.ClosedPoly = 0;
    end
    %
    j = j+1;
    Out(j) = NewFld;
end
Out(j+1:end)=[];
if ~isempty(warnings)
    warnings = unique(warnings);
    ui_message('warning',warnings)
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function subf=getsubfields(FI,domain,Props,f)
subf = Props.SubFld;
if nargin>3
    subf = subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,domain,Props)
sz = Props.Size;
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function T=readtim(FI,domain,Props,t)
if ~isfield(FI,'DiffDomain') || isempty(FI.DiffDomain)
    domain1 = 1;
else
    domain1 = FI.DiffDomain(1);
end
if nargin>3
    TimeNr = {t};
else % limit the number of time steps to the number presented to the user
    TimeNr = {1:Props.Size(T_)};
end
[success,T] = qp_getdata(FI.Files(1),domain1,Props.Q1,'times',TimeNr{:});
if ~success
    error(lasterr)
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,domain,Props,t)
if ~isfield(FI,'DiffDomain') || isempty(FI.DiffDomain)
    domain1 = 1;
else
    domain1 = FI.DiffDomain(1);
end
StationNr = {};
if nargin>3
    StationNr = {t};
end
[success,S] = qp_getdata(FI.Files(1),domain1,Props.Q1,'stations',StationNr{:});
if ~success
    error(lasterr)
end
% -----------------------------------------------------------------------------


% -------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE ===================================
FI = guarantee_options(FI);
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd
    case 'initialize'
        optfig(mfig);
        diffval_lst = findobj(mfig,'tag','diffval_lst');
        diffval_ops = get(diffval_lst,'userdata');
        i = ustrcmpi(qp_option(FI,'diffval'), diffval_ops);
        if i<0
            i = 1;
        end
        set(diffval_lst,'value',i)

    case 'diffval'
        diffval_lst = findobj(mfig,'tag','diffval_lst');
        i = get(diffval_lst,'value');
        diffval_ops = get(diffval_lst,'userdata');
        NewFI = qp_option(NewFI,'diffval',diffval_ops{i});
        
    otherwise
        error(['Unknown option command: ',cmd])
end
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
function FI = guarantee_options(FI)
defopt = {'diffval' 'val1-val2'};
for i = 1:size(defopt,1)
    opt = defopt{i,1};
    val = defopt{i,2};
    if isequal(qp_option(FI,opt),[])
        FI = qp_option(FI,opt,val);
    end
end
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
function optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
uicontrol('Parent',h0, ...
    'Style','text', ...
    'BackgroundColor',Inactive, ...
    'Horizontalalignment','left', ...
    'Position',[11 voffset 160 18], ...
    'String','Difference Value', ...
    'Enable','on', ...
    'Tag','diffval_txt');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Active, ...
    'Callback','d3d_qp fileoptions diffval', ...
    'Position',[171 voffset 160 20], ...
    'String',{'Value 1 - Value 2','Value 1 where no Value 2','Value 2 where no Value 1'}, ...
    'Userdata',{'val1-val2','only-val1','only-val2'}, ...
    'Value',1, ...
    'Enable','on', ...
    'Tag','diffval_lst');
% -------------------------------------------------------------------------
