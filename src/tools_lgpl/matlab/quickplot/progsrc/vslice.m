function [data,Slice] = vslice(data,v_slice,isel)
%VSLICE Vertical slice/section of 2D/3D data set.
%   DATA2DV = VSLICE(DATA3D,'XY',XY) extracts a vertical data slice out of
%   a 3D data set obtained from QPREAD along the specified (X,Y) line.
%   XY(:,1) should contain the X coordinates, and XY(:,2) the Y
%   coordinates. This function works for structured and unstructured meshes.
%
%   DATA2DV = VSLICE(DATA3D,'MN',MN) extracts a vertical data slice out of
%   a 3D data set obtained from QPREAD along a line the points indicates by
%   MN. For a structured grid, MN(:,1) should contain the indices in the
%   first index direction, and MN(:,2) should contain the indices in the
%   second index direction. For unstructured grid, MN should contain the
%   linear index of all points.
%
%   DATA1D = VSLICE(DATA2D,'XY',XY) extracts 1D line data out of a 2D data
%   set obtained from QPREAD along the specified (X,Y) line. XY(:,1) should
%   contain the X coordinates, and XY(:,2) the Y coordinates. The term
%   VSLICE is degenerate in this case.
%
%   DATA1D = VSLICE(DATA2D,'MN',MN) extracts 1D line data out of a 2D data
%   set obtained from QPREAD along a line the points indicates by MN. For a
%   structured grid, MN(:,1) should contain the indices in the first index
%   direction, and MN(:,2) should contain the indices in the second index
%   direction. For unstructured grid, MN should contain the linear index of
%   all points.
%
%   [DATA_OUT,XY_STRUCT] = VSLICE(DATA_IN,'XY',XY) returns a structure with
%   all information on the cross-sections of the XY line with the grid
%   associated with DATA_IN. If the (horizontal) grid doesn't change, the
%   XY_STRUCT can be reused for speed using the syntax:
%   DATA_OUT = VSLICE(DATA_IN,'XY',XY_STRUCT)
%
%   Example
%      quantities = qpread(FI);
%      ivel = strmatch('velocity',{quantities.Name},'exact');
%      Qvel = quantities(ivel);
%      sz = qpread(FI,Qvel,'size');
%      X = [0 35000]';
%      Y = [15000 15000]';
%      for t = 1:sz(1); %select time step where 1<=t<=sz(1)
%         data3d = qpread(FI,Qvel,'griddata',t);
%         data2dv = vslice(data3d,'XY',[X Y]);
%         surf(squeeze(data2dv.X),squeeze(data2dv.Z), ...
%                squeeze(data2dv.XComp.^2+data2dv.YComp.^2+data2dv.ZComp.^2))
%         view(0,90)
%         shading interp
%         drawnow
%      end
%
%   See also QPFOPEN, QPREAD, HSLICE, VRANGE.

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

if isfield(data,'Connect')
    data.FaceNodeConnect = data.Connect;
end
switch v_slice
    case 'MN'
        if isfield(data,'FaceNodeConnect') || isfield(data,'TRI') || isfield(data,'EdgeNodeConnect') || isfield(data,'SEG')
            multiTime = false;
            if isfield(data,'Time') && length(data.Time)>1
                multiTime = true;
            end
            if isfield(data,'FaceNodeConnect')
                switch data.ValLocation
                    case 'NODE'
                        if isfield(data,'EdgeNodeConnect')
                            EdgeNodeConnect = data.EdgeNodeConnect;
                        else
                            Faces = data.FaceNodeConnect;
                            nc = size(Faces,2);
                            iConnect = ceil(([0 0:2*nc-2])/2+0.1);
                            EdgeNodeConnect = Faces(:,iConnect);
                            ncP = sum(~isnan(Faces),2);
                            EdgeNodeConnect(:,1) = Faces(sub2ind(size(Faces),(1:size(Faces,1))',ncP));
                            EdgeNodeConnect = unique(sort(reshape(EdgeNodeConnect',[2 numel(Faces)]),1)','rows');
                            EdgeNodeConnect(any(isnan(EdgeNodeConnect),2),:) = [];
                        end
                        %
                        iedge = all(ismember(EdgeNodeConnect,isel),2);
                        data.EdgeNodeConnect = EdgeNodeConnect(iedge,:);
                        data.X  = data.X(isel);
                        data.Y  = data.Y(isel);
                        renum = zeros(max(isel),1);
                        renum(isel) = 1:length(isel);
                        data.EdgeNodeConnect = renum(data.EdgeNodeConnect);
                        % since data is defined at nodes, we can flip the
                        % edges without needing to worry about signs or
                        % directionality of the values.
                        data.EdgeNodeConnect = sort(data.EdgeNodeConnect,2);
                        [~,iOrder] = sort(data.EdgeNodeConnect(:,1));
                        data.EdgeNodeConnect = data.EdgeNodeConnect(iOrder,:);
                    case 'EDGE'
                        iedge = isel;
                        data.EdgeNodeConnect = data.EdgeNodeConnect(iedge,:);
                    case 'FACE'
                        iface = isel;
                        data.FaceNodeConnect = data.FaceNodeConnect(iface,:);
                end
            elseif isfield(data,'TRI')
                data.X = data.XYZ(:,isel,:,1);
                data.Y = data.XYZ(:,isel,:,2);
                if size(data.XYZ,4)>2
                    data.Z = data.XYZ(:,isel,:,3);
                end
                data = rmfield(data,'TRI');
                data = rmfield(data,'XYZ');
                multiTime = true;
            elseif isfield(data,'EdgeNodeConnect')
                switch data.ValLocation
                    case 'NODE'
                        data.X = data.X(isel,:);
                        data.Y = data.Y(isel,:);
                        data = rmfield(data,'EdgeNodeConnect');
                    case 'EDGE'
                        iedge = isel;
                        data.EdgeNodeConnect = data.EdgeNodeConnect(iedge,:);
                end
                data.Geom = 'sQUAD';
            else % isfield(data,'SEG')
                switch data.ValLocation
                    case 'NODE'
                        iedge = all(ismember(data.SEG,isel),2);
                        data.SEG = data.SEG(iedge,:);
                        data.XY  = data.XY(isel,:);
                        renum = zeros(max(isel),1);
                        renum(isel) = 1:length(isel);
                        data.SEG = renum(data.SEG);
                    case 'EDGE'
                        iedge = isel;
                        data.SEG = data.SEG(iedge,:);
                end
            end
            Flds = {'Val','XComp','YComp','ZComp'};
            for i=1:length(Flds)
                fld = Flds{i};
                if isfield(data,fld)
                    Tmp = data.(fld);
                    if multiTime
                        data.(fld) = Tmp(:,isel,:);
                    else
                        if size(Tmp,1)==1
                            szTmp = size(Tmp);
                            Tmp = reshape(Tmp,szTmp([2:end 1]));
                        end
                        data.(fld) = Tmp(isel,:);
                    end
                end
            end
        else
            szX = size(data.X);
            szX = szX(1:2); nX=szX(1)*szX(2);
            szX1 = szX-1; nX1=szX1(1)*szX1(2);
            sliceMN = piecewise(isel,szX);
            ind = sub2ind(szX,sliceMN(:,1),sliceMN(:,2));
            ind1 = [];
            Flds = {'X','Y','Z','Val','XComp','YComp','ZComp'};
            for i=1:length(Flds)
                fld = Flds{i};
                if isfield(data,fld)
                    Tmp = data.(fld);
                    szTmp = size(Tmp);
                    if isequal(szTmp(1:2),szX)
                        Tmp = reshape(Tmp,[nX 1 szTmp(3:end)]);
                        data.(fld) = Tmp(ind,:,:);
                    elseif isequal(szTmp(1:2),szX1)
                        if isempty(ind1)
                            ind1 = sub2ind(szX1,sliceMN(:,1),sliceMN(:,2));
                        end
                        Tmp = reshape(Tmp,[nX1 1 szTmp(3:end)]);
                        data.(fld) = Tmp(ind1,:,:);
                    end
                end
            end
        end
    case 'XY'
        if isfield(data,'Val')
            szV = size(data.Val);
        elseif isfield(data,'XComp')
            szV = size(data.XComp);
        else
            szV = [];
        end
        if isfield(data,'FaceNodeConnect')
            geomin = {data.FaceNodeConnect,data.X,data.Y};
            if isfield(data,'EdgeNodeConnect')
                geomin{4} = data.EdgeNodeConnect;
            end
        elseif isfield(data,'TRI')
            geomin = {data.TRI,data.XYZ(:,:,:,1),data.XYZ(:,:,:,2)};
        elseif isfield(data,'Time') && length(data.Time)>1 && ndims(data.X)==length(szV)
            szX = size(data.X);
            geomin = {reshape(data.X,szX(2:end)),reshape(data.Y,szX(2:end))};
        else
            geomin = {data.X,data.Y};
        end
        if isstruct(isel)
            Slice = isel;
        else
            Slice = arbcross(geomin{:},isel(:,1),isel(:,2));
        end
        nXLoc = [];
        nValLoc = [];
        nZLoc = [];
        Flds = {'X','Y','Z','Val','XComp','YComp','ZComp'};
        for i=1:length(Flds)
            fld = Flds{i};
            if isfield(data,fld)
                if isequal(fld,'X')
                    data.X = Slice.x;
                    data.dX_tangential = Slice.dxt;
                    nXLoc = size(data.X,1);
                elseif isequal(fld,'Y')
                    data.Y = Slice.y;
                    data.dY_tangential = Slice.dyt;
                else
                    szV = size(data.(fld));
                    if isfield(data,'Time')
                        if szV(1)==length(data.Time)
                            dms = [2:max(length(szV),3) 1];
                            data.(fld) = permute(data.(fld),dms);
                        end
                    elseif szV(1)==1
                        dms = [2:max(length(szV),3) 1];
                        data.(fld) = permute(data.(fld),dms);
                    end
                    if isequal(fld,'Z')
                        if isfield(data,'ZLocation') && ~isempty(data.ZLocation)
                            data.(fld) = arbcross(Slice,{data.ZLocation data.(fld)});
                        else
                            data.(fld) = arbcross(Slice,data.(fld));
                        end
                        nZLoc = size(data.(fld),1);
                    elseif isfield(data,'ValLocation') && ~isempty(data.ValLocation)
                        data.(fld) = arbcross(Slice,{data.ValLocation data.(fld)});
                        nValLoc = size(data.(fld),1);
                    else
                        data.(fld) = arbcross(Slice,data.(fld));
                        nValLoc = size(data.(fld),1);
                    end
                    if isfield(data,'Time') && length(data.Time)==szV(1)
                        szV = size(data.(fld));
                        if length(data.Time)==1
                            dms = [length(szV)+1 1:length(szV)];
                        else
                            dms = [length(szV) 1:length(szV)-1];
                        end
                        data.(fld) = permute(data.(fld),dms);
                    end
                end
            end
        end
        if isfield(data,'FaceNodeConnect')
            data=rmfield(data,'FaceNodeConnect');
            if isfield(data,'EdgeNodeConnect')
                data=rmfield(data,'EdgeNodeConnect');
            end
        elseif isfield(data,'TRI')
            szV = size(data.XYZ);
            if isfield(data,'Time') && length(data.Time)==szV(1)
                dms = [2:max(length(szV),3) 1];
                data.XYZ = permute(data.XYZ,dms);
            end
            data.X = arbcross(Slice,data.XYZ(:,:,1,:));
            nXLoc = size(data.X,1);
            data.dX_tangential = Slice.dxt;
            data.Y = arbcross(Slice,data.XYZ(:,:,2,:));
            data.dY_tangential = Slice.dyt;
            if size(data.XYZ,3)>2
                data.Z = arbcross(Slice,data.XYZ(:,:,3,:));
            end
            data=rmfield(data,'TRI');
            data=rmfield(data,'XYZ');
            if isfield(data,'Time') && length(data.Time)==szV(1)
                szV = size(data.X);
                if length(data.Time)==1
                    dms = [length(szV)+1 1:length(szV)];
                else
                    dms = [length(szV) 1:length(szV)-1];
                end
                data.X = permute(data.X,dms);
                data.Y = permute(data.Y,dms);
                if isfield(data,'Z')
                    data.Z = permute(data.Z,dms);
                end
            end
        end
        data.Geom = 'sQUAD';
        if isempty(nValLoc)
            % no values, so no ValLocation
        elseif nValLoc==nXLoc
            data.ValLocation = 'NODE';
        else
            data.ValLocation = 'EDGE';
        end
        if isempty(nZLoc)
            % no z-values, so no ZLocation
        elseif nZLoc==nXLoc
            data.ZLocation = 'NODE';
        else
            data.ZLocation = 'EDGE';
        end
    otherwise
        error('Expected ''XY'' or ''MN'' slice TYPE.')
end
