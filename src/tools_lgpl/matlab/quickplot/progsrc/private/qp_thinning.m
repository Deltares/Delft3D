function [data,s] = qp_thinning(data,Ops)
%QP_THINNING Reduce number of values in QP data structure.
%   DATA = QP_THINNING(DATA,OPS) reduces the number of values in DATA based
%   on the options specified in OPS where OPS is a structure containing the
%   following fields:
%    * thinningmode     = 'none', 'uniform', or 'distance'
%    * thinningfactors  = sampling step if the thinningmode is equal to
%                         'uniform'
%    * thinningdistance = minimum sampling distance if the thinningmode is
%                         equal to 'distance'.

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

if isfield(data,'Time')
    ntim = length(data(1).Time);
else
    ntim = 1;
end
multi_time = ntim>1;
%
s = [];
if ~isfield(Ops,'thinningmode')
    return
end
%
if isfield(data,'TRI')
    data = rmfield(data,'TRI');
end
%
if isfield(data,'XYZ') && strcmp(Ops.thinningmode,'regrid')
    if length(data)>1
        error('The regrid thinning option hasn''t been implemented yet for multiple domains.')
    end
    %
    rangemin = squeeze(min(data.XYZ,[],2));
    rangemax = squeeze(max(data.XYZ,[],2));
    drange = rangemax - rangemin;
    x = (0:floor(drange(1)/Ops.thinningdistance))*Ops.thinningdistance;
    x = x + (drange(1)-x(end))/2 + rangemin(1);
    y = (0:floor(drange(2)/Ops.thinningdistance))*Ops.thinningdistance;
    y = y + (drange(2)-y(end))/2 + rangemin(2);
    [X,Y]=meshgrid(x,y);
    if size(data.XYZ,4)>2
        data.Z=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.XYZ(1,:,1,3),X,Y);
    end
    if isfield(data,'XComp')
        data.XComp=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.XComp,X,Y);
    end
    if isfield(data,'YComp')
        data.YComp=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.YComp,X,Y);
    end
    if isfield(data,'ZComp')
        data.ZComp=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.ZComp,X,Y);
    end
    if isfield(data,'Val')
        data.Val=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.Val,X,Y);
    end
    data.X = X;
    data.Y = Y;
    data = rmfield(data,'XYZ');
    return
end
%
if isfield(data,'XYZ')
    for d = 1:length(data)
        data(d).X=data(d).XYZ(:,:,:,1);
        if size(data(d).XYZ,4)>1
            data(d).Y=data(d).XYZ(:,:,:,2);
        end
        if size(data(d).XYZ,4)>2
            data(d).Z=data(d).XYZ(:,:,:,3);
        end
    end
    data = rmfield(data,'XYZ');
end
%
if strcmpi(Ops.thinningmode,'none') || strcmpi(Ops.thinningmode,'dynamic')
    return
end

if length(data)>1
    slice2DV = false;
elseif ~multi_time && xor(size(data.X,1)>1,size(data.X,2)>1) && size(data.X,3)>1 && isfield(data,'Y')
    slice2DV = true;
else
    slice2DV = false;
end

if multi_time
    if isfield(data,'Val')
        szVal = size(data(1).Val);
    else
        szVal = [];
    end
    if isfield(data,'XComp')
        szVal = size(data(1).XComp);
    end
    if ~isequal(szVal,size(data(1).X))
        error('Thinning not yet implemented for X and Val of different size.')
    end
end

switch lower(Ops.thinningmode)
    case 'uniform'
        for d = 1:length(data)
            if isfield(data,'Val')
                sz = size(data(d).Val);
            else
                sz = size(data(d).XComp);
            end
            sel = cell(size(sz));
            start = 1 + multi_time;
            if multi_time
                sel{1} = ':';
            end
            for i = start:length(sel)
                sel{i} = 1:Ops.thinningfactors(i-multi_time):sz(i);
                sz(i) = length(sel{i});
            end
            for fcell = {'X','Y','Z','XComp','YComp','ZComp','Val'}
                f = fcell{1};
                if isfield(data,f)
                    if multi_time
                        sz(1) = size(data(d).(f),1);
                    end
                    data(d).(f)=reshape(data(d).(f)(sel{:}),sz);
                end
            end
        end
        
    case 'distance'
        npnt = zeros(1,length(data));
        if multi_time
            select_all = {':',':'};
            select_one = {1,':'};
            for d = 1:length(data)
                npnt(d) = numel(data(d).X)/ntim;
            end
        else
            select_all = {':'};
            select_one = {':'};
            for d = 1:length(data)
                npnt(d) = numel(data(d).X);
            end
        end
        tnpnt = sum(npnt);
        tmp1 = zeros(1,tnpnt);
        x = tmp1;
        %
        if isfield(data,'Y') && isfield(data,'Z')
            y = x;
            z = x;
            %
            tnpnt = 0;
            for d = 1:length(data)
                x(tnpnt+(1:npnt(d))) = data(d).X(select_one{:});
                y(tnpnt+(1:npnt(d))) = data(d).Y(select_one{:});
                z(tnpnt+(1:npnt(d))) = data(d).Z(select_one{:});
                tnpnt = tnpnt+npnt(d);
            end
            %
            I=~isnan(x) & ~isnan(y) & ~isnan(z);
            II=find(I);
            Ind=reducepoints(Ops.thinningdistance,x(I),y(I),z(I));
        elseif isfield(data,'Y')
            y = x;
            %
            tnpnt = 0;
            for d = 1:length(data)
                x(tnpnt+(1:npnt(d))) = data(d).X(select_one{:});
                y(tnpnt+(1:npnt(d))) = data(d).Y(select_one{:});
                tnpnt = tnpnt+npnt(d);
            end
            %
            I=~isnan(x) & ~isnan(y);
            II=find(I);
            Ind=reducepoints(Ops.thinningdistance,x(I),y(I));
        elseif isfield(data,'Z')
            z = x;
            %
            tnpnt = 0;
            for d = 1:length(data)
                x(tnpnt+(1:npnt(d))) = data(d).X(select_one{:});
                z(tnpnt+(1:npnt(d))) = data(d).Z(select_one{:});
                tnpnt = tnpnt+npnt(d);
            end
            %
            I=~isnan(x) & ~isnan(z);
            II=find(I);
            Ind=reducepoints(Ops.thinningdistance,x(I),z(I));
        else
            tnpnt = 0;
            for d = 1:length(data)
                x(tnpnt+(1:npnt(d))) = data(d).X(select_one{:});
                tnpnt = tnpnt+npnt(d);
            end
            %
            I=~isnan(x);
            II=find(I);
            Ind=reducepoints(Ops.thinningdistance,x(I));
        end
        I = false(size(I));
        I(II(Ind)) = true;
        %
        if nargout>1 && slice2DV
            if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                s=pathdistance(data.X(:,:,1),data.Y(:,:,1),'geographic');
            else
                s=pathdistance(data.X(:,:,1),data.Y(:,:,1));
            end
            s=reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
            s=s(I);
        end
        %
        tmpa = zeros(ntim,tnpnt);
        for fcell = {'X','Y','Z','XComp','YComp','ZComp','Val','dX_tangential','dY_tangential'}
            f = fcell{1};
            if isfield(data,f)
                tmpf = tmpa;
                tnpnt = 0;
                for d = 1:length(data)
                    tmpf(:,tnpnt+(1:npnt(d))) = data(d).(f)(select_all{:});
                    tnpnt = tnpnt+npnt(d);
                    data(d).(f) = [];
                end
                if multi_time
                    data(1).(f) = tmpf(:,I);
                else
                    data(1).(f) = tmpf(I);
                end
            end
        end
        data(2:end) = [];
end
