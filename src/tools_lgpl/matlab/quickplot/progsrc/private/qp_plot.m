function [hNewVec,Error,FileInfo,PlotState]=qp_plot(PlotState,Ops)
%QP_PLOT Plot function of QuickPlot.

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

T_=1; ST_=2; M_=3; N_=4; K_=5;

hNewVec=0;
Error=1;
specialplot='';

if isfield(PlotState,'FI')
    FileInfo = PlotState.FI;
    Domain = PlotState.Domain;
    Props = PlotState.Props;
    SubField = PlotState.SubField;
    Selected = PlotState.Selected;
    Parent = PlotState.Parent;
    hOld = PlotState.Handles;
    stats = PlotState.Stations;
    Ops = PlotState.Ops;
    Ops = qp_state_version(Ops);
    
    DimFlag = Props.DimFlag;
    
    SubSelected = Selected;
    SubSelected(~DimFlag) = [];
    if isfield(Props,'MNK') && Props.MNK
        Props.MNK = xyz_or_mnk(Ops,Selected,Props.MNK);
    end
    
    DataInCell = 0;
    if Props.NVal<0
        data = [];
    else
        if isfield(Ops,'axestype') && strcmp(Ops.axestype,'Time')
            [Chk,data.Time] = qp_getdata(FileInfo,Domain,Props,'times',Selected{T_});
            specialplot = 'time';
        elseif isfield(Ops,'extend2edge') && Ops.extend2edge
            [Chk,data,FileInfo] = qp_getdata(FileInfo,Domain,Props,'griddefdata',SubField{:},SubSelected{:});
        else
            vslice = 0;
            for i = 1:length(SubSelected)
                if iscell(SubSelected{i})
                    tp = SubSelected{i}{1};
                    if strcmp(tp,'XY') || strcmp(tp,'MN')
                        vslice = 1;
                        break
                    end
                end
            end
            if DimFlag(M_) && DimFlag(N_) && ...
                    (((isequal(Selected{M_},0) || length(Selected{M_})>1) && (~isequal(Selected{N_},0) && length(Selected{M_})==1)) ...
                    || ((isequal(Selected{N_},0) || length(Selected{N_})>1) && (~isequal(Selected{M_},0) && length(Selected{M_})==1)))
                vslice = 1;
            end
            presentationtype = Ops.presentationtype;
            data_at_edges = isfield(Props,'Geom') && ~isempty(strfind(Props.Geom,'EDGE'));
            if vslice && strcmp(presentationtype,'edges') && ~data_at_edges
                presentationtype = 'patch_slices';
            end
            switch presentationtype
                case {'patches','patches with lines','patch centred vector','polygons','patch_slices'}
                    [Chk,data,FileInfo] = qp_getdata(FileInfo,Domain,Props,'gridcelldata',SubField{:},SubSelected{:});
                    DataInCell = 1;
                    if strcmp(presentationtype,'patch_slices')
                        if size(data.X,2)==2 && size(data.X,1)>2
                            % The following lines are not valid for geographic coordinates!
                            data.X = (data.X(:,1,:) + data.X(:,2,:))/2;
                            data.Y = (data.Y(:,1,:) + data.Y(:,2,:))/2;
                        elseif size(data.X,1)==2 && size(data.X,2)>2
                            % The following lines are not valid for geographic coordinates!
                            data.X = (data.X(1,:,:) + data.X(2,:,:))/2;
                            data.Y = (data.Y(1,:,:) + data.Y(2,:,:))/2;
                        end
                    end
                otherwise
                    [Chk,data,FileInfo] = qp_getdata(FileInfo,Domain,Props,'griddata',SubField{:},SubSelected{:});
            end
        end
        if ~Chk
            % error message already generated by qp_getdata
            return
        elseif isempty(data)
            ui_message('error','Did not get any data from %s file.',FileInfo.FileType)
            return
        end
    end
else
    data = PlotState;
    FileInfo = 'unknown origin';
    data.Geom = 'UGRID2D-FACE';
    Selected = {1 [] 1:2 [] []}; %TODO
    Props.DimFlag = [1 0 1 0 0]; %TODO
    DimFlag = Props.DimFlag;
    SubField = {};
    Ops = qp_state_version(Ops);
    Props.Name = data.Name;
    Props.NVal = 1; %TODO
    stats = {}; % TODO
    hOld = [];
    Parent = gca;
end

hCLIMSYMM = [];
if isfield(Ops,'symmetriccolourlimits') && Ops.symmetriccolourlimits
    if iscell(hOld) && length(hOld{end}) == 1 && ishandle(hOld{end})
        xde = get(hOld{end}, 'xdata');
        if isequal(size(xde),[1 2]) && all(isnan(xde(:)))
            hCLIMSYMM = hOld{end};
            hOld(end) = [];
        end
    end
end
if iscell(hOld)
    hOldVec=cat(1,hOld{:});
else
    hOldVec=hOld;
    hOld={hOld};
end

if ~strcmp(Parent,'loaddata')
    %
    % Determine all objects in the axes. If this is a new plot then the new
    % object will be added on top, but if this is an update of an existing
    % object then the new object will be located at the location of the old
    % object in the object stack.
    %
    hNewTag='';
    lParents=get(hOldVec(ishandle(hOldVec)),'Parent');
    if iscell(lParents)
        lParents=unique([lParents{:}]);
    end
    nParents = length(lParents);
    %
    % PchildBefore lists objects plotted on top of this object.
    % PchildAfter lists objects plotted below this object.
    %
    PchildBefore = cell(1,nParents);
    PchildAfter = cell(1,nParents);
    SortObjs=1;
    for lP=1:nParents
        Pchild=allchild(lParents(lP));
        if isempty(hOldVec) || ~ishandle(hOldVec(1))
            %
            % If there is no old object, then plot new object on top.
            %
            PchildBefore{lP}=[];
            PchildAfter{lP}=Pchild;
        else
            hNewTag=get(hOldVec(1),'tag');
            IdxObj=find(Pchild==hOldVec(1));
            if isempty(IdxObj)
                %
                % If there is an old object, but not in the current Parent then
                % switch off object sorting.
                %
                SortObjs=0;
                PchildBefore{lP}=[];
                PchildAfter{lP}=[];
            else
                %
                % Old object exists in current axes, identify objects before
                % and after this object.
                %
                PchildBefore{lP}=Pchild(1:IdxObj);
                PchildBefore{lP}=PchildBefore{lP}(~ismember(PchildBefore{lP},hOldVec));
                PchildAfter{lP}=Pchild(IdxObj:end);
                PchildAfter{lP}=PchildAfter{lP}(~ismember(PchildAfter{lP},hOldVec));
            end
        end
    end
    %
    if isfield(Ops,'zlevel') && ~isequal(Ops.zlevel,'auto')
        Level = Ops.zlevel;
    else
        Level = -1;
        for i = 1:length(hOldVec)
            if ishandle(hOldVec(i))
                iLevel = getappdata(hOldVec(i),'Level');
                if ~isempty(iLevel)
                    Level = iLevel;
                end
            end
        end
        if Level<0
            Level = 0;
            Pchild=allchild(Parent);
            for i = 1:length(Pchild)
                iLevel = getappdata(Pchild(i),'Level');
                if ~isempty(iLevel)
                    Level = max(Level,iLevel);
                end
            end
            Level = Level+500;
        end
    end
end

for i=5:-1:1
    multiple(i) = (length(Selected{i})>1) | isequal(Selected{i},0);
end

if isfield(Ops,'axestimezone_shift') && ~isnan(Ops.axestimezone_shift)
    [Chk,datatimezone_shift,datatimezone_str] = qp_getdata(FileInfo,Domain,Props,'timezone');
    if isnan(datatimezone_shift)
        error('Cannot convert unknown time zone to %s',Ops.axestimezone_str)
    else
        for i = length(data):-1:1
            data(i).Time = data(i).Time + (Ops.axestimezone_shift-datatimezone_shift)/24;
        end
    end
end

Quant=Props.Name;
Units='';
if ~isempty(data) && isfield(data,'Units')
    Units=data(1).Units;
end
%
if isfield(Ops,'units')
    if isequal(Ops.units,'**Hide**')
        Units='';
    elseif isfield(data,'XComp')
        % data conversion of vector component will be done in computecomponent
        if isempty(Ops.units) % default values
            if (isfield(Ops,'vectorcomponent') && strcmp(Ops.vectorcomponent,'angle')) || ...
                    (isfield(Ops,'vectorcolour') && strcmp(Ops.vectorcolour,'angle'))
                Units='radians';
            else
                % data units used above should be correct
            end
        else
            Units=Ops.units;
        end            
    elseif ~isempty(Ops.units) && ~isempty(Units)
        if isfield(data,'TemperatureType')
            if data.TemperatureType
                TempType = 'absolute';
            else
                TempType = 'relative';
            end
        else
            TempType = 'unspecified';
        end
        dataX=qp_unitconversion(Units,Ops.units,data,TempType);
        if ~ischar(dataX)
            data=dataX;
            dataX=[];
            Units=data(1).Units;
        end
    end
end

Ops.basicaxestype = Ops.axestype;
try
    length_unit = ~ischar(qp_unitconversion(Units,'m'));
catch
    length_unit = 0;
end
try
    empty_unit = ~ischar(qp_unitconversion(Units,''));
catch
    empty_unit = 0;
end
if ~isempty(strfind(Props.Name,'level')) && (length_unit || empty_unit)
    Ops.axestype = strrep(Ops.axestype,'Z',['Z [',Units,']']);
elseif ~isempty(strfind(Ops.axestype,'Val'))
    Ops.axestype = strrep(Ops.axestype,'Val',['Val [',Units,']']);
end

FirstFrame=isempty(hOldVec);

% in case of track colour make sure to save a coordinate before it's lost.
if isfield(Ops,'trackcolour')
    data.Name = [data.Name, ': ', Ops.trackcolour];
    Quant = data.Name;
    if isfield(data,'XY')
        data.Val = cell(size(data.XY));
        for i = 1:length(data.XY)
            data.Val{i} = data.XY{i}(:,3);
            data.XY{i} = data.XY{i}(:,1:2);
        end
    else
        switch Ops.trackcolour
            case 'x coordinate'
                data.Val = data.X;
                if isfield(data,'XUnits')
                    data.Units = data.XUnits;
                else
                    data.Units = '';
                end
            case 'y coordinate'
                data.Val = data.Y;
                if isfield(data,'YUnits')
                    data.Units = data.YUnits;
                else
                    data.Units = '';
                end
            case 'z coordinate'
                data.Val = data.Z;
                if isfield(data,'ZUnits')
                    data.Units = data.ZUnits;
                else
                    data.Units = '';
                end
            case 'time'
                data.Val = repmat(data.Time,1,size(data.X,2));
                data.Units = '<matlab_time>';
        end
    end
    Props.NVal = 1;
    Units = data.Units;
end

if isfield(Ops,'plotcoordinate')
    % TODO: take into account the EdgeGeometry length ...
    switch Ops.plotcoordinate
        case {'path distance','reverse path distance'}
            if isfield(data,'FaceNodeConnect') || isfield(data,'EdgeNodeConnect')
                switch data.ValLocation
                    case 'FACE'
                        % here we should actually identify the point at
                        % which we go from one face to the next. Such that
                        % we get N data and N+1 coordinates.
                        X = data.X;
                        Y = data.Y;
                        X(end+1,:) = 0;
                        Y(end+1,:) = 0;
                        FNC = data.FaceNodeConnect;
                        nNodes = sum(~isnan(FNC),2);
                        FNC(isnan(FNC)) = length(X);
                        data.X = sum(X(FNC),2)./nNodes;
                        data.Y = sum(Y(FNC),2)./nNodes;
                    case 'EDGE'
                        iNode = stitch_edges(data.EdgeNodeConnect);
                        nodeMask = iNode==0;
                        iNode(nodeMask) = 1;
                        data.X = data.X(iNode);
                        data.Y = data.Y(iNode);
                        if any(nodeMask)
                            data.X(nodeMask) = NaN;
                            data.Y(nodeMask) = NaN;
                            edgeMask = nodeMask(1:end-1) | nodeMask(2:end);
                            data.Val(~edgeMask) = data.Val;
                            data.Val(edgeMask) = NaN;
                        end
                    case 'NODE'
                        % data.X/Y already contains the node coordinates
                end
                x = data.X;
                y = data.Y;
                for fld = {'FaceNodeConnect','EdgeNodeConnect','ValLocation'}
                    if isfield(data,fld{1})
                        data = rmfield(data,fld{1});
                    end
                end
            elseif isfield(data,'Y')
                if size(data.X,2)==2 && size(data.X,1)>2
                    % The following lines are not valid for geographic coordinates!
                    data.X = (data.X(:,1,:) + data.X(:,2,:))/2;
                    data.Y = (data.Y(:,1,:) + data.Y(:,2,:))/2;
                elseif size(data.X,1)==2 && size(data.X,2)>2
                    % The following lines are not valid for geographic coordinates!
                    data.X = (data.X(1,:,:) + data.X(2,:,:))/2;
                    data.Y = (data.Y(1,:,:) + data.Y(2,:,:))/2;
                end
                x = data.X(:,:,1);
                y = data.Y(:,:,1);
            else
                if size(data.X,2)==2 && size(data.X,1)>2
                    data.X = (data.X(:,1,:) + data.X(:,2,:))/2;
                elseif size(data.X,1)==2 && size(data.X,2)>2
                    data.X = (data.X(1,:,:) + data.X(2,:,:))/2;
                end
                x = data.X(:,:,1);
                y = 0*x;
            end
            if strcmp(Ops.plotcoordinate,'reverse path distance')
                x = rot90(x,2);
                y = rot90(y,2);
            end
            if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                s = pathdistance(x,y,'geographic');
                data.XUnits = 'm';
            else
                s = pathdistance(x,y);
            end
            %if ~isequal(size(data.X),size(data.Val)) && ~isfield(data,'dX_tangential')
            %    ds = s(min(find(s>0)))/2;
            %    if ~isempty(ds)
            %        s = s-ds;
            %    end
            %end
            if strcmp(Ops.plotcoordinate,'reverse path distance')
                s = rot90(s,2);
            end
            s = reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
        case 'x coordinate'
            s = data.X;
        case 'y coordinate'
            s = data.Y;
        case 'time'
            s = repmat(data.Time,[1 size(data.X,3)]);
    end
    data.X = squeeze(s);
    flds = {'Z','Val','XComp','YComp','ZComp'};
    for i = 1:length(flds)
        fld = flds{i};
        if isfield(data,fld)
            data.(fld) = squeeze(data.(fld));
        end
    end
    if isfield(data,'Y')
        data = rmfield(data,'Y');
        if isfield(data,'YUnits')
            data = rmfield(data,'YUnits');
        end
    end
    data.Geom = 'sSEG';
end

if length(data) == 1 && ...
        (strcmp(Ops.axestype,'Time-Z') || ...
        strcmp(Ops.axestype,'Val-Z'))
    if isfield(data,'ValLocation')
        switch data.ValLocation
            case 'FACE'
                FNC = data.FaceNodeConnect;
                missing = isnan(FNC);
                nNodes = size(missing,2)-sum(missing,2);
                FNC(missing) = 1;
                data.X = reshape(data.X(FNC),size(FNC));
                data.X(missing) = 0;
                data.X = sum(data.X,2)./nNodes;
                if isfield(data,'Y')
                    data.Y = reshape(data.Y(FNC),size(FNC));
                    data.Y(missing) = 0;
                    data.Y = sum(data.Y,2)./nNodes;
                end
        end
        data.Geom = 'sSEG';
        for c = {'FaceNodeConnect','EdgeNodeConnect','ValLocation','ZLocation','SEG','XY','XYZ','TRI','EdgeGeometry'}
            s = c{1};
            if isfield(data,s)
                data = rmfield(data,s);
            end
        end
    end
end

if strcmp(Ops.presentationtype,'vector') || ...
        strcmp(Ops.presentationtype,'markers') || ...
        strcmp(Ops.presentationtype,'values') || ...
        strcmp(Ops.presentationtype,'labels')
    % data = geom2pnt(data);
    for i = length(data):-1:1
        if isfield(data,'ValLocation')
            LOC = data(i).ValLocation;
        else
            LOC = 'NODE';
        end
        if isfield(data,'SEG')
            data(i).EdgeNodeConnect = data(i).SEG;
        end
        if isfield(data,'XYZ')
            data(i).X = data(i).XYZ(:,:,:,1);
            data(i).Y = data(i).XYZ(:,:,:,2);
            if size(data(i).XYZ,4)>2
                data(i).Z = data(i).XYZ(:,:,:,3);
            end
        elseif isfield(data,'XY')
            if iscell(data(i).XY)
                data(i).X = NaN(size(data(i).XY));
                data(i).Y = data(i).X;
                for j = 1:numel(data(i).XY)
                    if ~isempty(data(i).XY{j})
                        if size(data(i).XY{j},1)==1
                            data(i).X(j) = data(i).XY{j}(1);
                            data(i).Y(j) = data(i).XY{j}(2);
                        else
                            d = pathdistance(data(i).XY{j}(:,1),data(i).XY{j}(:,2));
                            uNode = d~=[-1;d(1:end-1)] & ~isnan(d);
                            XY = interp1(d(uNode),data(i).XY{j}(uNode,1:2),d(end)/2);
                            data(i).X(j) = XY(1);
                            data(i).Y(j) = XY(2);
                        end
                    end
                end
            else
                data(i).X = data(i).XY(:,1);
                data(i).Y = data(i).XY(:,2);
            end
        end
        switch LOC
            case 'EDGE'
                if isfield(data,'Geom') && (strcmp(data(i).Geom,'sQUAD') || strcmp(data(i).Geom,'sSEG'))
                    data(i).EdgeNodeConnect = [1:length(data(i).X)-1;2:length(data(i).X)]';
                end
                if isfield(data,'EdgeGeometry') && ~isempty(data(i).EdgeGeometry)
                    if isfield(data, 'XUnits')
                        Units = data.XUnits;
                    else
                        Units = '';
                    end
                    [data(i).X, data(i).Y] = geometry_midpoints(data(i).EdgeGeometry, Units);
                else
                    data(i).X = mean(shaped_subsref(data(i).X,data(i).EdgeNodeConnect),2);
                    if isfield(data,'Y')
                        data(i).Y = mean(shaped_subsref(data(i).Y,data(i).EdgeNodeConnect),2);
                    end
                    if isfield(data,'Z')
                        data(i).Z = (data(i).Z(:,1:end-1) + data(i).Z(:,2:end))/2;
                    end
                end
            case 'FACE'
                FNC = data(i).FaceNodeConnect;
                missing = isnan(FNC);
                nNodes = size(missing,2)-sum(missing,2);
                FNC(missing) = 1;
                data(i).X = reshape(data(i).X(FNC),size(FNC));
                data(i).X(missing) = 0;
                data(i).X = sum(data(i).X,2)./nNodes;
                if isfield(data,'Y')
                    data(i).Y = reshape(data(i).Y(FNC),size(FNC));
                    data(i).Y(missing) = 0;
                    data(i).Y = sum(data(i).Y,2)./nNodes;
                end
        end
        data(i).Geom = 'sSEG';
    end
    for c = {'FaceNodeConnect','EdgeNodeConnect','ValLocation','SEG','XY','XYZ','TRI','EdgeGeometry'}
        s = c{1};
        if isfield(data,s)
            data = rmfield(data,s);
        end
    end
end

[data,s] = qp_thinning(data,Ops);

vpt='';
[NVal,NValStr]=convertnval(Props.NVal);
if NVal==0.6 || NVal==0.9
    % 0.6 = thindam optionally coloured
    % 0.9 = coloured thindam
    NVal=0.5;
elseif  NVal==1.9 
    if isequal(Ops.presentationtype,'edges') || ...
             isequal(Ops.presentationtype,'edges m') || ...
              isequal(Ops.presentationtype,'edges n') || ...
              isequal(Ops.presentationtype,'values')
        % 1.9 = coloured thindam or vector perpendicular to thindam
        NVal=0.5;
    else
        % vector case: vector location is determined by computecomponent
        NVal=2;
        data.XComp = data.XDamVal;
        data.YComp = data.YDamVal;
        data = rmfield(data,{'XDam','YDam','XDamVal','YDamVal'});
        Ops.vectorcomponent='edge';
    end
end

VectorPlot=0;
%if isfield(data,'XComp')
if ~isempty(Ops.vectorcomponent)
    [data,scalar]=computecomponent(data,Ops);
    % HACK: if it's the edge normal or tangential component, take the
    % absolute value to avoid problems with direction
    if strncmp(Ops.vectorcomponent,'edge ',5)
        data.Val = abs(data.Val);
    end
    %
    if strcmp(Ops.vectorcomponent,'edge')
        if isempty(Ops.vectorcolour)
            Units='';
        end
    elseif ~isempty(strfind(Ops.vectorcomponent,'vector'))
        % no text added for vector and patch centred vector
        if ~isempty(Ops.vectorcolour)
            Quant=[Quant ', ' Ops.vectorcolour];
            Units = data(1).Units;
        elseif any(strcmp(Ops.basicaxestype,{'X-Y','Lon-Lat'}))
            Units='';
        end
    else
        Quant=[Quant ', ' Ops.vectorcomponent];
        Units = data(1).Units;
    end
    if strcmp(Ops.units,'**Hide**')
        Units = '';
    end
    if scalar
        NVal=1;
    else
        VectorPlot=1;
    end
end

if isfield(Ops,'operator') && ~strcmp(Ops.operator,'none')
    flds = {'Val','XDamVal','YDamVal'};
    % Ops.operator = 'isnan';
    for i = 1:length(flds)
        fldi = flds{i};
        if isfield(data,fldi)
            for d = 1:numel(data)
                data(d).(fldi) = feval(Ops.operator,data(d).(fldi));
            end
        end
    end
end

minx=inf;
miny=inf;
minz=inf;
minv=inf;
maxx=-inf;
maxy=-inf;
maxz=-inf;
maxv=-inf;
for d=1:length(data)
    if isfield(data,'X') % time series may not have an X co-ordinate !
        minx=min(minx,min(data(d).X(:)));
        maxx=max(maxx,max(data(d).X(:)));
    end
    if isfield(data,'Y')
        miny=min(miny,min(data(d).Y(:)));
        maxy=max(maxy,max(data(d).Y(:)));
    end
    if isfield(data,'Z')
        minz=min(minz,min(data(d).Z(:)));
        maxz=max(maxz,max(data(d).Z(:)));
    end
end
npnt=0;
if isfield(data,'Val') && isnumeric(data(1).Val) && ~VectorPlot
    for d=length(data):-1:1
        if isfield(data,'X')
            szX=size(data(d).X);
        elseif isfield(data,'Y')
            szX=size(data(d).Y);
        elseif isfield(data,'Z')
            szX=size(data(d).Z);
        else
            szX=-1;
        end
        if  isequal(szX,size(data(d).Val))
            if isfield(data,'X')
                I=isnan(data(d).X);
                data(d).Val(I)=NaN;
                %data(d).X(I)=maxx;
            end
            if isfield(data,'Y')
                I=isnan(data(d).Y);
                data(d).Val(I)=NaN;
                %data(d).Y(I)=maxy;
            end
            if isfield(data,'Z')
                I=isnan(data(d).Z);
                data(d).Val(I)=NaN;
                %data(d).Z(I)=maxz;
            end
        end
        npnt=npnt+sum(~isnan(data(d).Val(:)));
        maxv=max(maxv,max(data(d).Val(:)));
        minv=min(minv,min(data(d).Val(:)));
    end
elseif isfield(data,'XComp')
    for d=1:length(data)
        I = data(d).XComp(:)==0;
        if isfield(data,'YComp')
            I = I & data(d).YComp(:)==0;
        end
        if isfield(data,'ZComp')
            I = I & data(d).ZComp(:)==0;
        end
        I = I | isnan(data(d).XComp(:));
        npnt=npnt+sum(~I);
    end
    npnt=max(npnt,1);
elseif isfield(data,'XDamVal')
    for d=1:length(data)
        npnt = npnt + sum(data(d).XDamVal(:)~=0);
        npnt = npnt + sum(data(d).YDamVal(:)~=0);
    end
    npnt=max(npnt,1);
end

if isfield(Ops,'vectorscalingmode')
    if strcmp(Ops.axestype,'Lon-Lat') || (isfield(data,'XUnits') && strcmp(data(1).XUnits,'deg'))
        % axes in Lon-Lat coordinates
        % scale XComp depending on latitude
        % note: this scaling needs to be done AFTER the vectorcomponent for colouring has been computed.
        for d=length(data):-1:1
            data(d).XComp = data(d).XComp./max(cosd(data(d).Y),1e-7);
        end
    end
    switch Ops.vectorscalingmode
        case ''
            quivopt={};
        case 'automatic'
            quivopt={'automatic'};
        case 'manual'
            for d=length(data):-1:1
                if isfield(data,'XComp')
                    data(d).XComp=Ops.vectorscale*data(d).XComp;
                end
                if isfield(data,'YComp')
                    data(d).YComp=Ops.vectorscale*data(d).YComp;
                end
                if isfield(data,'ZComp')
                    data(d).ZComp=Ops.vectorscale*data(d).ZComp;
                end
            end
            quivopt={0};
        case {'manual normalised','automatic normalised'}
            for d=length(data):-1:1
                if strcmp(vpt,'magnitude')
                    VecMag=data(d).Val;
                else
                    VecMag=data(d).XComp.^2;
                    if isfield(data,'YComp')
                        VecMag=VecMag+data(d).YComp.^2;
                    end
                    if isfield(data,'ZComp')
                        VecMag=VecMag+data(d).ZComp.^2;
                    end
                    VecMag=sqrt(VecMag);
                end
                %VecMag(VecMag<1e-3)=NaN;
                VecMag(VecMag==0)=1;
                if isfield(data,'XComp')
                    data(d).XComp=Ops.vectorscale*data(d).XComp./VecMag;
                end
                if isfield(data,'YComp')
                    data(d).YComp=Ops.vectorscale*data(d).YComp./VecMag;
                end
                if isfield(data,'ZComp')
                    data(d).ZComp=Ops.vectorscale*data(d).ZComp./VecMag;
                end
            end
            if strcmp(Ops.vectorscalingmode,'manual normalised')
                quivopt={0};
            else
                quivopt={'automatic'};
            end
    end
else
    quivopt={};
end

if isequal(quivopt,{'automatic'})
    del2=0;
    if isfield(data,'X')
        del2 = del2 + (maxx-minx).^2/npnt;
    end
    if isfield(data,'Y')
        del2 = del2 + (maxy-miny).^2/npnt;
    end
    if isfield(data,'Z') && isfield(data,'ZComp')
        del2 = del2 + (maxz-minz).^2/npnt;
    end
    maxlen=0;
    for d=1:length(data)
        V=0;
        if isfield(data,'XComp')
            V=V+data(d).XComp.^2;
        elseif isfield(data,'XDamVal')
            maxlen=max(maxlen,max(abs(data(d).XDamVal(:))));
        end
        if isfield(data,'YComp')
            V=V+data(d).YComp.^2;
        elseif isfield(data,'YDamVal')
            maxlen=max(maxlen,max(abs(data(d).YDamVal(:))));
        end
        if isfield(data,'ZComp') && isfield(data,'Z')
            V=V+data(d).ZComp.^2;
        end
        if isfield(data,'X')
            V(isnan(data(d).X))=NaN;
        end
        if isfield(data,'Y')
            V(isnan(data(d).Y))=NaN;
        end
        if isfield(data,'Z')
            V(isnan(data(d).Z))=NaN;
        end
        maxlen=max(maxlen,max(V(:)));
    end
    if del2~=0
        maxlen=sqrt(maxlen/del2);
        if maxlen>0
            autoscale = 0.9 / maxlen;
        else
            autoscale = 0.9;
        end
    else
        autoscale = 0.9;
    end
    %
    % apply "automatic" scaling
    %
    for d=length(data):-1:1
        if isfield(data,'XComp')
            data(d).XComp=autoscale*data(d).XComp;
        elseif isfield(data,'XDamVal')
            data(d).XDamVal=autoscale*data(d).XDamVal;
        end
        if isfield(data,'YComp')
            data(d).YComp=autoscale*data(d).YComp;
        elseif isfield(data,'YDamVal')
            data(d).YDamVal=autoscale*data(d).YDamVal;
        end
        if isfield(data,'ZComp')
            data(d).ZComp=autoscale*data(d).ZComp;
        end
    end
    %
    quivopt={0};
end

classes_between_thresholds = 1;
if isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'coloured contour lines')
    classes_between_thresholds = 0;
end

if Props.NVal==6
    if isfield(data,'ClassVal')
        for d = 1:length(data)
            [~,data(d).Val] = ismember(data(d).Val,data(d).ClassVal);
        end
        Ops.Thresholds = data(1).ClassVal;
    else
        Ops.Thresholds = 1:length(data(1).Classes);
    end
    Ops.Thresholds(end+1) = inf;
elseif isfield(Ops,'thresholds') && ~strcmp(Ops.thresholds,'none')
    if isfield(Ops,'colourlimits') && isequal(size(Ops.colourlimits),[1 2])
        minmax = Ops.colourlimits;
    else
        miv = inf;
        mv  = -inf;
        for d = 1:length(data)
            miv = min(miv,min(data(d).Val(:)));
            mv  = max(mv ,max(data(d).Val(:)));
        end
        if isfield(Ops,'symmetriccolourlimits') && Ops.symmetriccolourlimits
            miv = min(miv,-mv);
            mv = max(mv,-miv);
        end
        minmax = [miv mv];
    end
    [Ops.Thresholds,Ops.PlotClass] = compthresholds(Ops,minmax,classes_between_thresholds);
else
    Ops.Thresholds = 'none';
    if isfield(Ops,'climclipping') && Ops.climclipping
        for fldc = {'Val'}
            fld = fldc{1};
            if isfield(data,fld)
                for d = 1:length(data)
                    Val = data(d).(fld);
                    Val(Val < Ops.colourlimits(1) | Val > Ops.colourlimits(2)) = NaN;
                    data(d).(fld) = Val;
                end
            end
        end
    end
end
if ~isfield(Ops,'PlotClass')
    if isequal(Ops.Thresholds,'none')
        Ops.PlotClass = [];
    else
        Ops.PlotClass = true(size(Ops.Thresholds));
    end
end

stn='';
if any(cellfun('isclass',Selected,'cell'))
    stn='';
elseif isfield(data,'LocationName') && length(data)==1
    if ischar(data.LocationName)
        stn = data.LocationName;
    elseif iscell(data.LocationName)
        if length(data.LocationName)==1
            stn = data.LocationName{1};
        else
            stn = '<multiple>';
        end
    end
elseif length(Selected{ST_})>1 || isequal(Selected{ST_},0)
    stn='<multiple>';
elseif ~isempty(stats)
    if iscell(stats)
        stn=stats{Selected{ST_}};
    else
        stn=deblank(stats(Selected{ST_},:));
    end
elseif DimFlag(ST_)
    [Chk,stn]=qp_getdata(FileInfo,Domain,Props,'stations');
    if iscell(stn)
        stn=stn{Selected{ST_}};
    else
        stn=stn(Selected{ST_},:);
    end
elseif DimFlag(M_) && DimFlag(N_) && DimFlag(K_) && none(multiple([M_ N_ K_]))
    stn=sprintf('point (%i,%i,%i)',Selected{M_},Selected{N_},Selected{K_});
elseif DimFlag(M_) && ~DimFlag(N_) && DimFlag(K_) && none(multiple([M_ K_]))
    stn=sprintf('point (%i,%i)',Selected{M_},Selected{K_});
elseif DimFlag(M_) && DimFlag(N_) && none(multiple([M_ N_]))
    stn=sprintf('point (%i,%i)',Selected{M_},Selected{N_});
elseif DimFlag(M_) && ~DimFlag(N_) && ~multiple(M_)
    stn=sprintf('point %i',Selected{M_});
end
stn=strrep(stn,'\','\\');
stn=strrep(stn,'_','\_');

if ~isempty(SubField)
    [Chk,subfs]=qp_getdata(FileInfo,Domain,Props,'subfields',SubField{1});
    if Chk && ~isempty(subfs)
        Quant=cat(2,Quant,': ',subfs{1});
    end
end

TStr='';
if isfield(data,'Time') && length(data(1).Time)==1
    TStr = qp_time2str(data(1).Time,DimFlag(T_));
    if isfield(Ops,'axestimezone_shift') && ~isnan(Ops.axestimezone_shift)
        TStr = [TStr ' (' Ops.axestimezone_str ')'];
    end
end

fld = 'XYZ';
for dir = 1:3
    X = fld(dir);
    if isfield(data,[X 'Units']) && ~isempty(data(1).([X 'Units']))
        fac = qp_unitconversion(data.([X 'Units']),'m');
        if ischar(fac) %distance unit not compatible with m
            % conversion not possible
        elseif fac==1
            % conversion not needed, but unit may not be 'm'
            % set unit string such that following routines work optimally
            for d = length(data):-1:1
                data(d).([X 'Units']) = 'm';
            end
        elseif isfield(data,X)
            for d = length(data):-1:1
                data(d).(X) = data(d).(X)*fac;
                data(d).([X 'Units']) = 'm';
            end
        elseif isfield(data,'XYZ')
            for d = length(data):-1:1
                sz      = size(data(d).XYZ);
                if dir<=sz(end)
                    sl      = repmat({':'},size(sz));
                    sl(end) = dir;
                    data(d).XYZ(sl{:}) = data(d).XYZ(sl{:})*fac;
                    data(d).([X 'Units']) = 'm';
                end
            end
        end
    end
end

if isfield(Ops,'presentationtype')
    switch Ops.presentationtype
        case {'patches','patches with lines'}
            if isfield(data,'ValLocation')
                for d = length(data):-1:1
                    switch data(d).ValLocation
                        case 'NODE'
                            % compute face averaged values
                            FaceNodeConnect = data(d).FaceNodeConnect;
                            Val = data(d).Val;
                            %
                            Msk = isnan(FaceNodeConnect);
                            FaceNodeConnect(Msk) = 1;
                            Val = Val(FaceNodeConnect);
                            Val(Msk) = 0;
                            Val = sum(Val,2)./sum(~Msk,2);
                            %
                            data(d).Val = Val;
                            data(d).ValLocation = 'FACE';
                    end
                end
            end
    end
end
data = qp_clipvalues(data, Ops);

if ~isempty(Parent) && all(ishandle(Parent)) && strcmp(get(Parent(1),'type'),'axes')
    pfig=get(Parent(1),'parent');
    set(0,'currentfigure',pfig)
    set(pfig,'currentaxes',Parent(1))
end

if isfield(Ops,'linestyle') && isfield(Ops,'marker') && ~strcmp(Ops.presentationtype,'markers')
    LineParams = {};
    if strcmp(Ops.presentationtype,'coloured contour lines')
        LineParams = {};
    else
        LineParams = {'color',Ops.colour};
    end
    if isfield(Ops,'linewidth')
        Ops.LineParams=[LineParams, ...
            {'linewidth',Ops.linewidth, ...
            'linestyle',Ops.linestyle, ...
            'marker',Ops.marker, ...
            'markersize',Ops.markersize, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour}];
    else % linewidth typically not specified if linestyle = 'none'
        Ops.LineParams=[LineParams, ...
            {'linestyle',Ops.linestyle, ...
            'marker',Ops.marker, ...
            'markersize',Ops.markersize, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour}];
    end
elseif isfield(Ops,'marker')
    Ops.LineParams={'linestyle','none', ...
        'marker',Ops.marker, ...
        'markersize',Ops.markersize, ...
        'markeredgecolor',Ops.markercolour, ...
        'markerfacecolor',Ops.markerfillcolour};
end
if isfield(Ops,'horizontalalignment') && isequal(Ops.horizontalalignment,'centre')
    Ops.horizontalalignment='center';
end
if isfield(Ops,'fontsize')
    Ops.FontParams={'color',Ops.colour, ...
        'fontunits','points', ...
        'fontsize',Ops.fontsize};
    if isfield(Ops,'horizontalalignment')
        Ops.FontParams = cat(2,Ops.FontParams, { ...
            'horizontalalignment',Ops.horizontalalignment, ...
            'verticalalignment',Ops.verticalalignment});
    end
    if matlabversionnumber>=6.05
        if strcmp(Ops.textboxfacecolour,'none')
            TextBoxParams={'edgecolor','none','backgroundcolor','none'};
        else
            TextBoxParams={'edgecolor',Ops.colour,'backgroundcolor',Ops.textboxfacecolour};
        end
        Ops.FontParams=cat(2,Ops.FontParams,TextBoxParams);
    end
end

%
% If horizontal units is degrees, change to longitude and latitude plot
% type.
%
if isfield(data,'XUnits') && ...
        (strcmp(data(1).XUnits,'deg') || strcmp(data(1).XUnits,'degree'))
    Ops.axestype = strrep(Ops.axestype,'X-Y','Lon-Lat');
    Ops.axestype = strrep(Ops.axestype,'X-','Lon-');
end
%
% If it the plot contains a Z co-ordinate.
%
if isfield(Ops,'basicaxestype') && ~isempty(strfind(Ops.basicaxestype,'Z'))
    %
    % If the elevation unit has not yet been specified, do so now.
    %
    if isempty(strfind(Ops.axestype,'Z ['))
        %
        % Get it either from the ZUnit field or assume it to be metres.
        %
        if isfield(data,'ZUnit') && ~isempty(data(1).ZUnit)
            Zunit=cat(2,'[',data(1).ZUnit,']');
        else
            Zunit='[m]';
        end
        Ops.axestype=cat(2,Ops.axestype,' ',Zunit);
    end
end

ChangeCLim=1;

diststr = 'x coordinate';
isdist  = 0;
if isfield(Ops,'plotcoordinate') && ~isempty(Ops.plotcoordinate)
    switch Ops.plotcoordinate
        case {'path distance','reverse path distance'}
            isdist = 1;
            if multiple(M_) && ~multiple(N_)
                if isempty(Selected{N_})
                    diststr = 'distance';
                else
                    name=sprintf('n=%i',Selected{N_});
                    diststr = sprintf('distance along cross-section %s',name);
                end
            elseif multiple(N_) && ~multiple(M_)
                if isempty(Selected{M_})
                    diststr = 'distance';
                else
                    name=sprintf('m=%i',Selected{M_});
                    diststr = sprintf('distance along cross-section %s',name);
                end
            else
                diststr = 'distance along path';
            end
        case 'x coordinate'
            if isfield(data,'XUnits') && isequal(data(1).XUnits,'deg')
                diststr = 'longitude';
            else
                diststr = 'x coordinate';
            end
        case 'y coordinate'
            % data.Y has been moved to data.X while processing Ops.plotcoordinate
            if isfield(data,'XUnits') && isequal(data(1).XUnits,'deg')
                diststr = 'latitude';
            else
                diststr = 'y coordinate';
            end
        case 'coordinate'
            if isfield(data,'XName')
                diststr = data(1).XName;
            else
                diststr = 'coordinate';
            end
    end
end

if NVal==4
    switch Ops.presentationtype
        case {'markers','edges','tracks'}
            NVal = 0;
    end
end

%==========================================================================
% Begin of actual plotting
%==========================================================================
Quant = protectstring(Quant);
if isequal(Parent,'loaddata')
    % load data
    hNewVec = data;
    Error = 0;
    return
elseif ~isempty(specialplot)
    switch specialplot
        case 'time'
            axtype = getappdata(Parent, 'BasicAxesType');
            hNew = hOld;
            switch axtype
                case {'analog clock','digital clock','calendar page'}
                    md_clock(Parent, data.Time)
                    hNewVec = [hNew{:}];
                otherwise
                    hNewVec = hOld{1};
                    if strncmp(axtype, 'Time', 4)
                        ylim = get(Parent, 'ylim');
                        if isempty(hNewVec)
                            hNewVec = line(data.Time*[1 1], ylim, Ops.LineParams{:});
                        else
                            set(hNewVec, 'xdata', data.Time*[1 1], 'ydata', ylim)
                        end
                    else
                        xlim = get(Parent, 'xlim');
                        if isempty(hNewVec)
                            hNewVec = line(xlim, data.Time*[1 1], Ops.LineParams{:});
                        else
                            set(hNewVec, 'xdata', xlim, 'ydata',  data.Time*[1 1])
                        end
                    end
                    hNew{1} = hNewVec;
            end
    end
elseif NVal==-2
    [Chk,hNewVec,FileInfo]=qp_getdata(FileInfo,Domain,Props,'plot',Parent,Ops,hOld,SubField{:},SubSelected{:});
    Error = ~Chk;
    return
elseif NVal==-1
    [Chk,hNewVec,FileInfo]=qp_getdata(FileInfo,Domain,Props,'plot',Parent,Ops,hOld,SubField{:},SubSelected{:});
    hNew{1}=hNewVec;
    hObsolete=setdiff(hOld{1},hNew{1});
    delete(hObsolete(ishandle(hObsolete)));
    if isfield(Ops,'axestype') && ~isempty(Ops.axestype)
        setaxesprops(Parent,Ops.axestype)
    else
        set(Parent,'layer','top')
        %   elseif ~isappdata(Parent,'AxesType')
        %      setaxesprops(Parent,'<blocking>')
    end
else
    Param.ChangeCLim=1;
    Param.NVal=NVal;
    Param.multiple=multiple;
    Param.FirstFrame=FirstFrame;
    Param.Quant=Quant;
    Param.Units=Units;
    Param.TStr=TStr;
    Param.Selected=Selected;
    Param.quivopt=quivopt;
    Param.stats=stats;
    Param.stn=stn;
    Param.s=s;
    Param.compat7={};
    if matlabversionnumber>=7
        Param.compat7={'v6'};
    end

    hNew = hOld;
    for d = length(data):-1:1
        do=min(length(hOld),d);
        plotargs={hOld{do},Parent,Param,data(d),Ops,Props};
        geom='';
        if isfield(data,'Geom') && ~isempty(data(d).Geom)
            geom=data(d).Geom;
        elseif isfield(Props,'Geom')
            geom=Props.Geom;
        end
        if strcmp(geom,'POLYG') && ~DataInCell
            if DimFlag(M_) && DimFlag(N_)
                geom='';
            else
                geom='PNT';
            end
        end
        switch geom
            case {'SEG','SEG-NODE','SEG-EDGE'}
                [hNew{d},Param]=qp_plot_seg(plotargs{:});
            case 'PNT'
                [hNew{d},Param,Parent]=qp_plot_pnt(plotargs{:});
            case {'POLYL','POLYG'}
                [hNew{d},Param]=qp_plot_polyl(plotargs{:});
            case {'UGRID1D_NETWORK-NODE','UGRID1D_NETWORK-EDGE','UGRID1D-NODE','UGRID1D-EDGE','UGRID2D-NODE','UGRID2D-EDGE','UGRID2D-FACE'}
                [hNew{d},Param]=qp_plot_ugrid(plotargs{:});
            otherwise
                if ~isfield(data,'TRI')
                    % default structured mesh contourf function is not yet
                    % suitable for PlotClass.
                    switch Ops.presentationtype
                        case {'coloured contour lines','contour patches','contour patches with lines'}
                            Ops.climclipping = 0;
                            Ops.PlotClass(:) = true;
                    end
                end
                [hNew{d},Param,Parent]=qp_plot_default(plotargs{:});
                PlotState.Parent=Parent;
        end
        hNew{d} = hNew{d}(:);
    end
    for d = length(data)+1:length(hNew)
        delete(hNew{d});
    end
    hNew = hNew(1:length(data));
    
    ChangeCLim = strcmp(Ops.Thresholds,'none') || ~isempty(Ops.colourlimits);

    hNewVec=cat(1,hNew{:});
end

if isempty(specialplot) && isfield(Ops,'basicaxestype') && ~isempty(Ops.basicaxestype) && length(Parent)==1
    axestype = multiline(strtok(Ops.basicaxestype),'-','cell');
    nAxes = length(axestype);
    %
    dimension = cell(1,nAxes);
    unit = cell(1,nAxes);
    %
    for d = 1:nAxes
        switch axestype{d}
            case 'Time'
                dimension{d} = 'time';
                if ~isfield(Ops,'axestimezone_shift') || isnan(Ops.axestimezone_shift)
                    unit{d} = '';
                else
                    unit{d} = Ops.axestimezone_str;
                end
            case 'Distance'
                dimension{d} = 'distance';
                if isfield(data,'XUnits') && ~isempty(data(1).XUnits)
                    unit{d} = data(1).XUnits;
                    if strcmp(data(1).XUnits,'deg')
                        unit{d} = 'm';
                    end
                end
            case 'Val'
                dimension{d} = Quant;
                unit{d} = Units;
            case 'Y'
                dimension{d} = 'y coordinate';%'distance';
                if isfield(Props,'NName') && ~isempty(Props.NName)
                    dimension{d} = Props.NName;
                end
                if isfield(data,'YUnits') && ~isempty(data(1).YUnits)
                    unit{d} = data(1).YUnits;
                end
            case 'Z'
                dimension{d} = 'elevation';
                if isfield(data,'ZName')
                    dimension{d} = data(1).ZName;
                end
                if isfield(data,'ZUnits')
                    if ~isempty(data(1).ZUnits)
                        unit{d} = data(1).ZUnits;
                    end
                elseif isfield(data,'Units') && ~isempty(data(1).Units)
                    % only use data unit for "elevation" if this is a level unit as
                    % recognized in qp_interface_update_options. The requirements
                    % are:
                    % 1) Name should contain level
                    if ~isempty(strfind(data(1).Name,'level'))
                        % 2) Unit should be compatible with m
                        if ~ischar(qp_unitconversion(Units,'m')) || ...
                                ~ischar(qp_unitconversion(Units,''))
                            unit{d} = data(1).Units;
                        end
                    end
                end
            otherwise
                dimension{d} = diststr;%'distance';
                if isfield(Ops,'plotcoordinate') && strcmp(Ops.plotcoordinate,'y coordinate')
                    if isfield(Props,'NName') && ~isempty(Props.NName)
                        dimension{d} = protectstring(Props.NName);
                    end
                    if isfield(data,'YUnits') && ~isempty(data(1).YUnits)
                        unit{d} = data(1).YUnits;
                    end
                else
                    if isfield(Props,'MName') && ~isempty(Props.MName)
                        dimension{d} = protectstring(Props.MName);
                    end
                    if isfield(data,'XUnits') && ~isempty(data(1).XUnits)
                        unit{d} = data(1).XUnits;
                    end
                end
                if isdist && strcmp(unit{d},'deg')
                    unit{d} = 'm';
                end
        end
    end
    setaxesprops(Parent,Ops.axestype,dimension,unit)
    xyz = 'xyz';
    for d = 1:nAxes
        x = xyz(d);
        switch axestype{d}
            case 'Val'
                if isfield(data, 'Classes')
                    if isfield(data, 'ClassVal')
                        cv = data(1).ClassVal;
                    else
                        cv = 1:length(data(1).Classes);
                    end
                    set(Parent, ...
                        [x 'tick'], cv, ...
                        [x 'ticklabel'], data(1).Classes)
                end
        end
    end
end
%==========================================================================
% End of actual plotting
%==========================================================================

Error=0;

if isfield(Ops,'colourlimits') && ~isempty(Ops.colourlimits)
    if ChangeCLim
        set(Parent,'clim',Ops.colourlimits)
    end
elseif isfield(Ops,'symmetriccolourlimits') && Ops.symmetriccolourlimits
    clim = limits(hNewVec,'clim');
    clim = max(abs(clim));
    if isempty(hCLIMSYMM)
        hNew{end+1} = surface([NaN NaN], [NaN NaN], [NaN NaN;NaN NaN], ...
                              'cdata', [-1 1;-1 1]*clim);
    else
        set(hCLIMSYMM, 'cdata', [-1 1;-1 1]*clim)
        hNew{end+1} = hCLIMSYMM;
    end
end

if isfield(Ops,'colourmap') && ~isempty(Ops.colourmap)
    if ischar(Ops.colourmap)
        colormap(qp_colormap(Ops.colourmap));
    else
        ui_message('warning','Colourmap option not yet implemented.');
    end
end

if isfield(Ops,'colourbar') && ~strcmp(Ops.colourbar,'none')
    Chld =allchild(pfig);
    isAx =strcmp(get(Chld,'type'),'axes');
    nonAx=Chld(~isAx);
    Ax   =Chld(isAx);
    has_colorbar = ~isempty(findall(Parent,'tag','ColorbarDeleteProxy'));
    h=qp_colorbar(Ops.colourbar,'peer',Parent);
    if ~isempty(Units)
        if isequal(Units,'<matlab_time>')
            switch Ops.colourbar
                case 'vert'
                    tick(h,'y','autodate')
                case 'horiz'
                    tick(h,'x','autodate')
            end
            PName = Quant;
        else
            PName = sprintf('%s (%s)',Quant,Units);
        end
    else
        PName = Quant;
    end
    switch Ops.colourbar
        case 'vert'
            ylabel(h,PName)
        case 'horiz'
            xlabel(h,PName)
    end
    if ~has_colorbar && ~isempty(h)
        set(pfig,'children',[nonAx;h;Ax(ishandle(Ax) & (Ax~=h))])
        cbratio = qp_settings('colorbar_ratio');
        if cbratio>1
            switch Ops.colourbar
                case 'horiz'
                    set(h,'PlotBoxAspectRatio',[cbratio 1 1])
                case 'vert'
                    set(h,'PlotBoxAspectRatio',[1 cbratio 1])
            end
        end
        if ~strcmp(Ops.Thresholds,'none')
            Thresholds = Ops.Thresholds;
            PlotClass = Ops.PlotClass;
            Classes = {};
            LineParams = {};
            if Props.NVal==6
                Thresholds = Thresholds(1:end-1);
                PlotClass = PlotClass(1:end-1);
                LabelStyle = {'labelcolor'};
                Classes = {data(1).Classes};
            elseif classes_between_thresholds
                LabelStyle = {};
                % remove the infinity threshold ...
                Thresholds = Ops.Thresholds(1:end-1);
            elseif isfield(Ops,'LineParams') && ~isempty(Ops.LineParams)
                LabelStyle = {'labellines'};
                LineParams = {'lineparams',Ops.LineParams};
            else
                LabelStyle = {'labelcolor'};
            end
            classbar(h,1:length(Thresholds),LabelStyle{:},'label',Thresholds,Classes{:},'plotselect',PlotClass,'climmode','new',LineParams{:})
        end
    end
end

if isempty(hNewVec)
    hNewVec=line('parent',Parent(1),'xdata',[],'ydata',[],'zdata',[]);
    hNew{end+1}=hNewVec;
end
if isempty(hNewTag)
    hNewTag=sprintf('QPPlotTag [%bx-%bx]',now,rand);
end
set(hNewVec,'hittest','off','tag',hNewTag)

IUD.PlotState=PlotState;
IUD.PlotState.FI=FileInfo;
IUD.PlotState.Handles=hNew;
IUD.XInfo=[];
if isfield(data,'XInfo')
    IUD.XInfo=data(1).XInfo;
end
if ~isempty(Ops.Thresholds)
    IUD.XInfo.Thresholds=Ops.Thresholds;
end
set(hNewVec,'userdata',[])
set(hNewVec(1),'userdata',IUD)
%
% If object sorting is enabled (almost always) then let's put the new
% object at the right location in the object stack.
%
if SortObjs
    for lP=1:nParents
        Parent=lParents(lP);
        %
        % Identify objects that are children of this parent ...
        %
        childlist=allchild(Parent);
        hNew_lP=hNewVec(ismember(hNewVec,childlist));
        %
        % ... and put them in the right location.
        %
        children=[PchildBefore{lP};hNew_lP(:);PchildAfter{lP}];
        children(~ismember(children,childlist))=[];
        %
        % Put any children that didn't exist initially and that aren't
        % listed as being part of the new object, on top.
        %
        children=cat(1,childlist(~ismember(childlist,children)),children);
        set(Parent,'children',children)
    end
end
%
if ~isfield(Ops,'basicaxestype') || (~strcmp(Ops.basicaxestype,'X-Y-Z') && ~strcmp(Ops.basicaxestype,'X-Y-Val'))
    setzcoord(hNewVec,Level)
end
for i=1:length(hNewVec)
    a = get(hNewVec(i),'parent');
    if strcmp(get(a,'type'),'axes')
        cp = get(a,'cameraposition');
        if cp(3)<Level
            cp(3) = 1.1*Level;
            set(a,'cameraposition',cp,'cameraupvector',get(a,'cameraupvector'))
        end
    end
    %set(a,'zlim',limits(a,'zlim')+[-1 +1])
    %set(a,'CameraViewAngle',get(a,'CameraViewAngle'))
end
setappdata(hNewVec(1),'Level',Level)


function y = shaped_subsref(x,ind)
% x(ind) returns an array of the same shape as ind unless both x and ind
% are vectors. In that case x(ind) is a similar (row or column) vector as
% x. This function reshapes it to the shape of ind.
y = reshape(x(ind),size(ind));


function [x, y] = geometry_midpoints(EdgeGeometry, Units)
X = EdgeGeometry.X;
Y = EdgeGeometry.Y;
x = zeros(size(X));
y = zeros(size(X));
for i = 1:numel(X)
    xg = X{i};
    yg = Y{i};
    d = pathdistance(xg, yg, Units);
    dmid = d(end)/2;
    j = sum(d<dmid);
    fac = (dmid - d(j)) / (d(j+1) - d(j));
    x(i) = xg(j) + fac * (xg(j+1) - xg(j));
    y(i) = yg(j) + fac * (yg(j+1) - yg(j));
end


function iNode = stitch_edges(EdgeNodeConnect)
nEdges = size(EdgeNodeConnect,1);
if nEdges == 1
    iNode = EdgeNodeConnect;
    return
end
iNode = zeros(1,3*nEdges);
% identify match forward
match = ismember(EdgeNodeConnect(1,:),EdgeNodeConnect(2,:));
if sum(match) == 1 && find(match) == 1
    iNode(2:-1:1) = EdgeNodeConnect(1,:);
else
    iNode(1:2) = EdgeNodeConnect(1,:);
end
iN = 2;
iE = 2;
while iE <= nEdges
    % identify match backwards
    match = EdgeNodeConnect(iE,:) == iNode(iN);
    switch sum(match)
        case 0
            % no match ... skip one index
            iN = iN+1;
            % identify match forward
            if iE < nEdges
                match = ismember(EdgeNodeConnect(iE,:),EdgeNodeConnect(iE+1,:));
            else
                match = 0;
            end
            if sum(match) == 1 && find(match) == 1
                iNode(iN+(2:-1:1)) = EdgeNodeConnect(iE,:);
            else
                iNode(iN+(1:2)) = EdgeNodeConnect(iE,:);
            end
            iN=iN+2;
        case 1
            % great ... one match as expected ... extend to not match index.
            iN = iN+1;
            iNode(iN) = EdgeNodeConnect(iE,~match);
        case 2
            % both match ... edge starts and ends at same node
            iN = iN+1;
            iNode(iN) = iNode(iN-1);
    end
    iE = iE+1;
end
iNode = iNode(1:iN);
