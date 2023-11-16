function B1 = subsref(A,S)
%SUBSREF Subscripted reference qp_data_resource object.

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

final_si = false;
nsubscripts = length(S);
si=1;
%
Resource = resourcemanager('retrieve',A.Key);
%
while si<=nsubscripts
    si_start=si;
    if ~strcmp(S(si).type,'.')
        error('Invalid subscript type.')
    elseif ~ischar(S(si).subs)
        error('Subscript string expected.')
    end
    %
    cmd = lower(S(si).subs);
    switch cmd
        case 'reset'
            B = A;
            B.LocationNrs = 1:length(Resource.Locations);
            B.QuantityNrs = 1:length(Resource.Quantities);
            si = si+1;
            final_si = true;
        case 'resource'
            B = Resource;
            si = si+1;
            final_si = true;
        case 'key'
            B = A.Key;
            si = si+1;
            final_si = true;
        case {'ndimensions','dimensions','dimensionnames'}
            if length(A.QuantityNrs)==1
                % if there is only one quantity, keep dimensions sorted in
                % original order
                QuantDims = Resource.Quantities(A.QuantityNrs).Dimensions;
                if ~A.Raw
                    QuantDims = QuantDims(1,:);
                    [mem,idx]=ismember(QuantDims,Resource.Locations(A.LocationNrs).SpatialDimensions);
                    QuantDims(mem)=Resource.Locations(A.LocationNrs).DimConversionTable(idx(mem));
                end
            else
                QuantDims = {Resource.Quantities(A.QuantityNrs).Dimensions};
                multistag = find(cellfun('size',QuantDims,1)>1);
                for q = multistag
                    QuantDims{q} = QuantDims{q}(:)';
                end
                QuantDims = unique(cat(2,QuantDims{:}));
                if ~A.Raw
                    for l = A.LocationNrs
                        [mem,idx]=ismember(QuantDims,Resource.Locations(l).SpatialDimensions);
                        QuantDims(mem)=Resource.Locations(l).DimConversionTable(idx(mem));
                    end
                    QuantDims = unique(QuantDims);
                end
            end
            switch cmd
                case 'ndimensions'
                    B = repmat(size(QuantDims,2),size(QuantDims,1),1);
                case 'dimensions'
                    % get dimension structures for the dimensions still in use
                    dimUsed = ismember({Resource.Dimensions.Name},QuantDims);
                    B = Resource.Dimensions(dimUsed);
                    % sort the dimensions by name
                    [sortedDimNames,sortOrder]=sort({B.Name});
                    B = B(sortOrder);
                    if length(A.QuantityNrs)==1
                        % if there is only one quantity, return dimensions
                        % sorted in their original order
                        [QuantDimsSorted,I,J] = unique(QuantDims);
                        B = reshape(B(J),size(QuantDims));
                    end
                case 'dimensionnames'
                    B = QuantDims;
            end
            si = si+1;
            final_si = true;
        case 'size'
            if length(A.QuantityNrs)~=1
                error('Data size can only be determined if one location/quantity has been selected.')
            end
            %
            QuantDims = Resource.Quantities(A.QuantityNrs).Dimensions;
            if ~A.Raw
                QuantDims = QuantDims(1,:);
                [mem,idx]=ismember(QuantDims,Resource.Locations(A.LocationNrs).SpatialDimensions);
                QuantDims(mem)=Resource.Locations(A.LocationNrs).DimConversionTable(idx(mem));
            end
            if si+1<=nsubscripts && isequal(S(si+1).type,'()')
                if iscellstr(S(si+1).subs)
                    validDims = ismember(S(si+1).subs,QuantDims(:));
                    if any(~validDims)
                        invalidDims = find(~validDims);
                        error('Invalid dimension: %s.',S(si+1).subs{invalidDims(1)})
                    end
                    QuantDims = S(si+1).subs;
                else
                    QuantDims = QuantDims(:,cat(2,S(si+1).subs{:}));
                end
                si = si+1;
            end
            B = repmat(NaN,size(QuantDims));
            ResDims = Resource.Dimensions;
            ResDimNames = {ResDims.Name};
            for i = 1:numel(QuantDims)
                j = strmatch(QuantDims{i},ResDimNames,'exact');
                if ~isempty(j)
                    B(i) = length(ResDims(j).Values);
                end
            end
            si = si+1;
            final_si = true;
        case {'timedependent','ntimes','times','ntimedims','timedimnames'}
            if length(A.QuantityNrs)~=1
                error('Time can only be determined if one location/quantity has been selected.')
            end
            %
            QTimeDims = Resource.Quantities(A.QuantityNrs).TimeDimensions;
            LTimeDims = Resource.Locations(A.LocationNrs).TimeDimensions;
            TimeDims = unique([LTimeDims QTimeDims]);
            %
            ResDims = Resource.Dimensions;
            DimList = {ResDims.Name};
            iTimeDims = zeros(size(TimeDims));
            for i = 1:length(TimeDims)
                iTimeDims(i) = strmatch(TimeDims{i},DimList,'exact');
            end
            switch cmd
                case 'timedependent'
                    B = ~isempty(TimeDims);
                case 'ntimedims'
                    B = length(TimeDims);
                case 'timedimnames'
                    B = TimeDims;
                case 'ntimes'
                    if isempty(TimeDims)
                        B = 0;
                    else
                        B = zeros(size(TimeDims));
                        for i = 1:length(TimeDims)
                            B(i) = length(ResDims(iTimeDims(i)).Values);
                        end
                    end
                case 'times'
                    if isempty(TimeDims)
                        error('Quantity is time independent.')
                    elseif length(TimeDims)>1
                        error('Quantity has multiple time dimensions.')
                    end
                    B = ResDims(iTimeDims(1)).Values;
            end
            si = si+1;
            final_si = true;
        case {'nsubfields','subfields','subfieldnames','subfieldvalues'}
            if length(A.QuantityNrs)~=1
                error('Time can only be determined if one location/quantity has been selected.')
            end
            %
            Quant = Resource.Quantities(A.QuantityNrs);
            QuantDims = Quant.Dimensions(1,:); % only spatial indices may differ for quantities defined at multiple stagger locations.
            iLoc = strmatch(Quant.Location,{Resource.Locations.Name}','exact');
            SpatialDims = Resource.Locations(iLoc).SpatialDimensions;
            ResDims = Resource.Dimensions;
            DimList = {ResDims.Name};
            sfdim = true(size(QuantDims));
            sfidx = zeros(size(QuantDims));
            for i = 1:length(QuantDims)
                j = strmatch(QuantDims{i},DimList,'exact');
                if isequal(ResDims(j).Type,'discrete-time') || isequal(ResDims(j).Type,'continuous-time')
                    sfdim(i) = false;
                elseif ismember(QuantDims{i},SpatialDims)
                    sfdim(i) = false;
                else
                    sfidx(i) = j;
                end
            end
            %
            switch cmd
                case 'nsubfields'
                    B = sum(sfdim);
                case 'subfieldnames'
                    B = QuantDims(sfdim);
                case 'subfields'
                    B = ResDims(sfidx(sfidx~=0));
                case 'subfieldvalues'
                    sfDims = ResDims(sfidx(sfidx~=0));
                    %
                    if si+1<=nsubscripts && isequal(S(si+1).type,'()')
                        if length(S(si+1).subs)>1
                            error('Single dimension index expected for subfieldvalues.')
                        elseif iscellstr(S(si+1).subs)
                            idim = ustrcmpi(S(si+1).subs{1},{sfDims.Name});
                            if idim<0
                                error('Invalid name of subfield dimension: %s.',S(si+1).subs{1})
                            end
                            sfDims = sfDims(idim);
                        else
                            if any(S(si+1).subs{1}>length(sfDims))
                                error('Invalid subfield dimension number: %g.',S(si+1).subs{1})
                            end
                            sfDims = sfDims(S(si+1).subs{1});
                        end
                        si = si+1;
                    else
                        error('Missing subfield name or number.')
                    end
                    %
                    B = sfDims.Values;
            end
            si = si+1;
            final_si = true;
        case {'nlocations','locations','locationnames'}
            switch cmd
                case 'nlocations'
                    B = length(A.LocationNrs);
                case 'locations'
                    B = Resource.Locations(A.LocationNrs);
                case 'locationnames'
                    B = {Resource.Locations(A.LocationNrs).Name}';
            end
            si = si+1;
            final_si = true;
        case 'location'
            if si+1>nsubscripts
                error('Missing domain name or number');
            elseif ~isequal(S(si+1).type,'()')
                error('Invalid domain selection syntax: OBJECT.Location(...).')
            end
            B = A;
            locationnr = S(si+1).subs;
            %
            current_LocNr  = A.LocationNrs;
            current_LocStr = {Resource.Locations(current_LocNr).Name};
            nLocNr = length(current_LocNr);
            %
            for i=1:length(locationnr)
                if islogical(locationnr{i})
                    nrs = 1:nLocNr;
                    locationnr{i} = nrs(locationnr{i});
                elseif ischar(locationnr{i})
                    locationname = locationnr{i};
                    locationnr{i} = strmatch(locationname,current_LocStr,'exact')';
                    if isempty(locationnr{i})
                        locationnr{i} = strmatch(locationname,current_LocStr)';
                    end
                else
                    check = ismember(locationnr{i},1:nLocNr);
                    if any(~check)
                        locationNrs = locationnr{i}(~check);
                        error('Location number %g invalid.',locationNrs(1))
                    end
                end
            end
            B.LocationNrs = current_LocNr(cat(2,locationnr{:}));
            thisLoc = ismember({Resource.Quantities(A.QuantityNrs).Location}, ...
                {Resource.Locations(B.LocationNrs).Name});
            B.QuantityNrs = A.QuantityNrs(thisLoc);
            si = si+2;
        case {'nquantities','quantities','quantitynames','listquantities'}
            switch cmd
                case 'nquantities'
                    B = length(A.QuantityNrs);
                case 'quantities'
                    B = Resource.Quantities(A.QuantityNrs);
                case 'quantitynames'
                    B = {Resource.Quantities(A.QuantityNrs).Name}';
                case 'listquantities'
                    Q = Resource.Quantities(A.QuantityNrs);
                    nQuant = length(Q);
                    nDigit = ceil(log10(nQuant));
                    B = {Q.Name};
                    nameLen = max(cellfun('length',B));
                    B = {Q.Unit};
                    unitLen = max(cellfun('length',B));
                    Str = sprintf('%%%ii %%-%is %%-%is\\n',nDigit,nameLen,unitLen);
                    for i = 1:nQuant
                        fprintf(Str,i,Q(i).Name,Q(i).Unit);
                    end
                    B = 'QuickPlot.NoOutput';
            end
            si = si+1;
            final_si = true;
        case {'nstations','stations','stationnames'}
            Locs = Resource.Locations(A.LocationNrs);
            if length(Locs)>1
                error('Stations can only be accessed if only one location has been selected.')
            else
                switch cmd
                    case 'nstations'
                        if ~strcmp(Locs.Topology,'Unstruct0D+')
                            B = 0;
                        else
                            PntDim = Locs.Dimensions{1};
                            ipnt = strmatch(PntDim,{Resource.Dimensions.Name},'exact');
                            B = length(Resource.Dimensions(ipnt).Values);
                        end
                    case {'stations','stationnames'}
                        if ~strcmp(Locs.Topology,'Unstruct0D+')
                            B = {};
                        else
                            PntDim = Locs.Dimensions{1};
                            ipnt = strmatch(PntDim,{Resource.Dimensions.Name},'exact');
                            B = Resource.Dimensions(ipnt).Values;
                        end
                end
            end
            si = si+1;
            final_si = true;
        case 'quantity'
            if si+1>nsubscripts
                error('Missing quantity name or number');
            elseif ~isequal(S(si+1).type,'()')
                error('Invalid quantity selection syntax: OBJECT.Quantity(...).')
            end
            B = A;
            quantnr = S(si+1).subs;
            %
            current_QuantStr = {Resource.Quantities(A.QuantityNrs).Name};
            nQuantNr = length(A.QuantityNrs);
            %
            for i = 1:length(quantnr)
                if islogical(quantnr{i})
                    nrs = 1:nQuantNr;
                    quantnr{i} = nrs(quantnr{i});
                elseif ischar(quantnr{i})
                    quantname = quantnr{i};
                    quantnr{i} = strmatch(quantname,current_QuantStr,'exact')';
                    if isempty(quantnr{i})
                        quantnr{i} = strmatch(quantname,current_QuantStr)';
                    end
                elseif isstruct(quantnr{i})
                    Qnt = quantnr{i};
                    ResQnts = Resource.Quantities(A.QuantityNrs);
                    fld = fieldnames(Qnt);
                    for f = 1:length(fld)
                        if ~isfield(ResQnts,fld{f})
                            Qnt = rmfield(Qnt,fld{f});
                        end
                    end
                    fld = fieldnames(ResQnts);
                    for f = 1:length(fld)
                        if ~isfield(Qnt,fld{f})
                            ResQnts = rmfield(ResQnts,fld{f});
                        end
                    end
                    for q = 1:nQuantNr
                        if isequal(Qnt,ResQnts(q))
                            quantnr{i} = q;
                            break
                        end
                    end
                    if isstruct(quantnr{i})
                        error('No match found for quantity structure.')
                    end
                else
                    check = ismember(quantnr{i},1:nQuantNr);
                    if any(~check)
                        QuantNrs = quantnr{i}(~check);
                        error('Quantity number %g invalid.',QuantNrs(1))
                    end
                end
            end
            B.QuantityNrs = A.QuantityNrs(cat(2,quantnr{:}));
            thisQnt = ismember({Resource.Locations(A.LocationNrs).Name}, ...
                {Resource.Quantities(B.QuantityNrs).Location});
            B.LocationNrs = A.LocationNrs(thisQnt);
            si = si+2;
        case 'unit'
            if si+1>nsubscripts
                error('Missing unit string.');
            elseif ~isequal(S(si+1).type,'()') ...
                    || length(S(si+1).subs)~=1 ...
                    || ~ischar(S(si+1).subs{1})
                error('Invalid unit syntax: OBJECT.Unit(''unit specification'').')
            end
            B = A;
            RequestedUnit = S(si+1).subs{1};
            Q = Resource.Quantities(A.QuantityNrs);
            okay = false(1,length(Q));
            for i = 1:length(Q)
                if isequal(RequestedUnit,Q(i).Unit)
                    okay(i) = true;
                else
                    a = qp_unitconversion(RequestedUnit,Q(i).Unit);
                    if ischar(a)
                        if length(Q)==1
                            error(a)
                        end
                    else
                        okay(i) = true;
                    end
                end
            end
            if length(Q)>1 && ~any(okay)
                error('None of the quantities has compatible units.')
            else
                B.QuantityNrs = A.QuantityNrs(okay);
                B.Unit = RequestedUnit;
            end
            si = si+2;
        case 'raw'
            B = A;
            B.Raw = true;
            si = si+1;
        case {'dimension','getdummy','get','getdata','getgrid','subfield'}
            getcmd = isequal(cmd(1:3),'get');
            if length(A.QuantityNrs)~=1
                if getcmd
                    error('Data can only be retrieved if only one location/quantity has been selected.')
                else
                    error('Dimension can only be set if only one location/quantity has been selected.')
                end
            end
            Quant = Resource.Quantities(A.QuantityNrs);
            if strcmp(cmd,'subfield')
                QuantDims = Quant.SubDimensions;
                dimtype = 'subfield dimension';
            else
                QuantDims = Quant.Dimensions;
                dimtype = 'dimension';
            end
            Loc = Resource.Locations(A.LocationNrs);
            if ~A.Raw && ~isempty(A.LocationNrs)
                QuantDims = QuantDims(1,:);
                [mem,idx]=ismember(QuantDims,Loc.SpatialDimensions);
                QuantDims(mem)=Loc.DimConversionTable(idx(mem));
                shorthand_allowed = true;
            else
                shorthand_allowed = size(QuantDims,1)==1;
                QuantDims = unique(QuantDims(:))';
            end
            ResDims = {Resource.Dimensions.Name};
            %
            Selection = A.Selection;
            %
            si = si+1;
            if si<=nsubscripts && isequal(S(si).type,'()')
                subs = S(si).subs;
                shorthand = true;
                for i = 1:length(subs)
                    if ~isnumeric(subs{i}) && ~isequal(subs{i},':')
                        shorthand = false;
                        break
                    end
                end
                if shorthand
                    if ~shorthand_allowed
                        error('Shorthand dimension selection not allowed for data\ndefined at %s.',stagprint(Quant.Stagger))
                    end
                    nsel = length(subs);
                    if nsel>length(QuantDims)
                        error('Too many %s specifications.',dimtype)
                    end
                    for i = 1:nsel
                        v = subs{i};
                        if isequal(v,':')
                            j = strmatch(QuantDims{i},ResDims,'exact');
                            v = 1:length(Resource.Dimensions(j).Values);
                        end
                        Selection.(QuantDims{i}) = v;
                        Selection = adjustselect(Selection, ...
                            Loc,Resource.Dimensions,QuantDims{i}); %<----
                    end
                else
                    nsel = floor(length(subs)/2);
                    if 2*nsel~=length(subs)
                        error('Invalid dimension/selection pair.')
                    end
                    subs = reshape(subs,2,nsel);
                    for i = 1:nsel
                        if ~ischar(subs{1,i})
                            error('Invalid dimension/selection pair %i.',i)
                        end
                        dim = subs{1,i};
                        idim = ustrcmpi(dim,QuantDims);
                        if idim<0
                            error('No %s called ''%s''.',dimtype,dim)
                        end
                        jdim = strmatch(QuantDims{idim},ResDims,'exact');
                        if isequal(subs{2,i},':')
                            Selection.(QuantDims{idim}) = 1:length(Resource.Dimensions(jdim).Values);
                        else
                            Selection.(QuantDims{idim}) = subs{2,i};
                        end
                        Selection = adjustselect(Selection, ...
                            Loc,Resource.Dimensions,QuantDims{idim}); %<----
                    end
                end
                si = si+1;
            elseif ~getcmd
                error('Missing %s selection.',dimtype)
            end
            if getcmd
                %
                % Get the actual data from the resource and finish.
                %
                final_si = true;
                %
                % If no subselection of a dimension has been made, select
                % the whole dimension.
                %
                for i = 1:length(QuantDims)
                    if ~isfield(Selection,QuantDims{i})
                        j = strmatch(QuantDims{i},ResDims,'exact');
                        Selection.(QuantDims{i}) = 1:length(Resource.Dimensions(j).Values);
                        Selection = adjustselect(Selection, ...
                            Loc,Resource.Dimensions,QuantDims{i}); %<----
                    end
                end
                %
                Quant = rmfield(Quant,{'SpatialDimensions','TimeDimensions','SubDimensions'});
                QName = {Quant.Name};
                if strcmp(cmd,'getdata') || isempty(Loc)
                    %
                    % Get only data.
                    %
                else
                    %
                    % Determine grid coordinates.
                    %
                    GridQuant = [Loc.XCoord Loc.YCoord];
                    switch Loc.Topology
                        case {'Struct3D','Unstruct3D'}
                            k = 1;
                        case 'Struct2D+'
                            k = floor(find(ismember(Loc.SpatialDimensions(7:end),QuantDims))/3)+1;
                        case 'Unstruct2D+'
                            k = floor(find(ismember(Loc.SpatialDimensions(4:end),QuantDims))/3)+1;
                        case 'Unstruct1D+'
                            k = floor(find(ismember(Loc.SpatialDimensions(3:end),QuantDims))/3)+1;
                        case 'Unstruct0D+'
                            k = floor(find(ismember(Loc.SpatialDimensions(2:end),QuantDims))/3)+1;
                        otherwise
                            k = [];
                    end
                    if ~isempty(k)
                        GridQuant(3) = Loc.ZCoord(k);
                    end
                    GName = {'QuickPlot.x_coordinate' 'QuickPlot.y_coordinate'};
                    if length(GridQuant)==3
                        GName{3} = 'QuickPlot.z_coordinate';
                    end
                    %
                    if strcmp(cmd,'getgrid')
                        %
                        % Get only grid.
                        %
                        Quant = GridQuant;
                        QName = GName;
                    else
                        %
                        % Get both grid and data.
                        %
                        Ngrid = length(GridQuant);
                        Quant(1+(1:Ngrid)) = GridQuant;
                        QName(1+(1:Ngrid)) = GName;
                    end
                end
                %
                B = cell(1,length(Quant));
                if strcmp(cmd,'getdummy')
                    for i = 1:length(Quant)
                        B{i} = {[]};
                    end
                    cmd = 'wrapdummy';
                else
                    for i = 1:length(Quant)
                        B{i} = feval(Resource.Functions.data, ...
                            Resource.RawData,Quant(i),Selection);
                    end
                    cmd = 'wrap';
                end
                [Quant.Name] = deal(QName{:});
                B = qp_data(cmd,Quant,Loc,Resource.Dimensions,Selection,B);
                if ~isempty(A.Unit)
                    B = B.Unit(A.Unit);
                end
            else
                %
                % Dimension or subfield statement: adjust selection field
                % and continue.
                %
                B = A;
                B.Selection = Selection;
            end
        otherwise
            %
            % Try to match the field name to a Location if there are
            % multiple.
            %
            found = false;
            Loc = Resource.Locations(A.LocationNrs);
            if length(A.LocationNrs)>1
                l = strmatch(S(si).subs,{Loc.Name},'exact');
                if isempty(l)
                    l = strmatch(S(si).subs,{Loc.Name});
                end
                if ~isempty(l)
                    found = true;
                    B = A;
                    B.LocationNrs = A.LocationNrs(l);
                    thisLoc = ismember({Resource.Quantities(A.QuantityNrs).Location}, ...
                        {Resource.Locations(B.LocationNrs).Name});
                    B.QuantityNrs = A.QuantityNrs(thisLoc);
                    si = si+1;
                end
            end
            %
            % Try to match the field name to a Quantity if there are
            % multiple and it was not a location.
            %
            if ~found && length(A.QuantityNrs)>1
                l = strmatch(S(si).subs,{Resource.Quantities(A.QuantityNrs).Name},'exact');
                if isempty(l)
                    l = strmatch(S(si).subs,{Resource.Quantities(A.QuantityNrs).Name});
                end
                if ~isempty(l)
                    found = true;
                    B = A;
                    B.QuantityNrs = A.QuantityNrs(l);
                    thisQnt = ismember({Loc.Name}, ...
                        {Resource.Quantities(B.QuantityNrs).Location});
                    B.LocationNrs = A.LocationNrs(thisQnt);
                    si = si+1;
                end
            end
            %
            % Try to match the field name to a Dimension if there is only
            % one quantity and location.
            %
            if ~found && length(A.QuantityNrs)==1
                QuantDims = Resource.Quantities(A.QuantityNrs).Dimensions;
                if ~A.Raw
                    QuantDims = QuantDims(1,:);
                    [mem,idx]=ismember(QuantDims,Loc.SpatialDimensions);
                    QuantDims(mem)=Loc.DimConversionTable(idx(mem));
                else
                    QuantDims = unique(QuantDims(:)');
                end
                l = ustrcmpi(S(si).subs,QuantDims);
                if l>0
                    Dim = QuantDims{l};
                    found = true;
                    if si+1>nsubscripts
                        error('Missing dimension selection.');
                    elseif ~isequal(S(si+1).type,'()')
                        error('Invalid dimension selection syntax: OBJECT.Dimension(...).')
                    end
                    B = A;
                    if iscellstr(S(si+1).subs)
                        lr = ustrcmpi(Dim,{Resource.Dimensions.Name});
                        Values = Resource.Dimensions(lr).Values;
                        for i=1:length(S(si+1).subs)
                            iV = ustrcmpi(S(si+1).subs{i},Values);
                            if iV>0
                                S(si+1).subs{i} = iV;
                            else
                                error('Invalid or non-unique %s index ''%s''',Dim,S(si+1).subs{i})
                            end
                        end
                    end
                    B.Selection.(Dim) = cat(2,S(si+1).subs{:});
                    B.Selection = adjustselect(B.Selection, ...
                        Loc,Resource.Dimensions,Dim); %<----
                    si = si+2;
                end
            end
            %
            % If still no match found, throw error
            %
            if ~found
                error('Unknown field or function ''%s''.',S(si).subs)
            end
    end
    if final_si
        if si <= nsubscripts
            B = subsref(B,S(si:end));
            si = nsubscripts+1;
        end
    else
        if si_start >= si
            error('Missing subscript index (si) increment in while loop!')
        end
        A = B;
    end
end

if ~isequal(B,'QuickPlot.NoOutput')
    B1 = B;
end
