function [Data, errmsg] = qp_netcdf_get(FI,var,varargin)
%QP_NETCDF_GET Get data from netcdf file and reshape.
%   [DATA,ERR] = QP_NETCDF_GET(FILE,VAR,REQDIMS,REQIND) reads data for the
%   variable VAR from the specified netcdf file FILE. The order of the
%   dimensions of VAR (and optional additional dimensions) is to be specified
%   by the cell string REQDIMS, and the corresponding requested indices are
%   given by REQIND. The data is returned as DATA and ERR will be a non-empty
%   string is an error occurs.

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

errmsg='';
%
if isempty(var)
    errmsg='Variable name is empty.';
    error(errmsg)
elseif ischar(var)
    varstr = var;
    varid = strmatch(varstr,{FI.Dataset.Name},'exact')-1;
    if isempty(varid)
        error('Variable ''%s'' not found in file.',varstr)
    end
elseif isstruct(var)
    varid = var.Varid;
else
    varid = var;
end
Info = FI.Dataset(varid+1);
%
RequestDims = {};
RequestedSubset = {};
netcdf_use_fillvalue = qp_settings('netcdf_use_fillvalue');
mesh_subsets = {};
%
i = 1;
while i<=length(varargin)
    if ischar(varargin{i})
        switch lower(varargin{i})
            case 'netcdf_use_fillvalue'
                netcdf_use_fillvalue = varargin{i+1};
            case 'mesh_subsets'
                mesh_subsets = varargin{i+1};
            otherwise
                error('Unknown input argument %i "%s" in qp_netcdf_get',i+2,varargin{i})
        end
        i = i+1;
    elseif isempty(RequestDims)
        RequestDims = varargin{i};
    elseif isempty(RequestedSubset)
        RequestedSubset = varargin{i};
    else
        error('Unknown input argument %i in qp_netcdf_get',i+2)
    end
    i = i+1;
end
%
if isempty(RequestDims)
    RequestDims = Info.Dimension;
end
if isempty(RequestedSubset)
    RequestedSubset = repmat({':'}, 1, length(RequestDims));
end
for i = length(RequestDims):-1:1
    if isequal(RequestedSubset{i},0) || strcmp(RequestedSubset{i},':')
        idim = strcmp(RequestDims{i}, Info.Dimension);
        RequestedSubset{i} = 1:Info.Size(idim);
    end
end
%
if isfield(Info,'Mesh') && iscell(Info.Mesh) && strcmp(Info.Mesh{1},'ugrid')
    imesh = Info.Mesh{3};
    imdim = Info.Mesh{4};
    mInfo = FI.Dataset(imesh);
    mdim  = mInfo.Mesh{5+imdim};
else
    mdim = '';
end
%
rank = length(Info.Dimension);
base_rank = rank;
N=length(RequestDims);
permuted=zeros(1,N);
for d=1:N
    DName=RequestDims{d};
    if ~isempty(DName)
        d_netcdf = strcmp(DName,Info.Dimension);
        if none(d_netcdf) &&  ~isempty(mdim)
            imdim2 = find(strcmp(DName,mInfo.Mesh(5:end)))-1;
            if ~isempty(imdim2)
                canConvert = false;
                if ~isempty(mesh_subsets)
                    isMDIM = strcmp(mdim,mesh_subsets(:,2));
                    if sum(isMDIM)==1
                        canConvert = true;
                        d_netcdf = strcmp(mdim,Info.Dimension);
                        RequestedSubset{d} = mesh_subsets{isMDIM,3};
                    end
                end
                if ~canConvert
                    error('Spatial dimension mismatch for variable "%s": requested dimension "%s", known dimension "%s"',Info.Name,DName,mdim)
                end
            end
        end
        %
        d_netcdf = find(d_netcdf);
        if isempty(d_netcdf)
            %
            % requested dimension does not occur in NetCDF source
            % use higher dimensions to fill in these dimensions.
            %
            rank=rank+1;
            permuted(d)=rank;
        else
            %
            % requested dimension occurs in NetCDF source
            %
            permuted(d)=d_netcdf;
        end
    end
end
%
for d=1:base_rank
    if all(permuted~=d)
        errmsg=sprintf('Dimension ''%s'' of ''%s'' could not be matched to any of the requested dimensions: ',Info.Dimension{d},Info.Name);
        errmsg=cat(2,errmsg,sprintf('''%s'', ',RequestDims{permuted~=0}));
        errmsg(end-1:end)=[];
        error(errmsg)
    end
end
%
if nargin==3 || base_rank == 0
    Data = nc_vargetr(FI.Filename,FI.Dataset(varid+1).Name);
    if length(FI.Dataset(varid+1).Size)>1 && ~isequal(size(Data),FI.Dataset(varid+1).Size)
        Data = reshape(Data,FI.Dataset(varid+1).Size);
    end
else
    %
    % Convert data subset in QP dimension order to NetCDF dimension order
    %
    RS_netcdf=cell(1,base_rank);
    start_coord=zeros(1,base_rank);
    count_coord=zeros(1,base_rank);
    %fprintf('%s: %d %d %d %d %d\n',FI.Dataset(varid+1).Name,permuted);
    %
    % The following block selection procedure is inefficient if a relatively
    % limited number of values is requested compared to full range of
    % indices, e.g. [1 2 100]
    %
    for d=1:N
        p=permuted(d);
        if p>0 && p<=base_rank
            RS_netcdf(p)=RequestedSubset(d);
            start_coord(p)=min(RS_netcdf{p})-1;
            count_coord(p)=max(RS_netcdf{p})-start_coord(p);
            RS_netcdf{p}=RS_netcdf{p}-start_coord(p);
        end
    end
    %
    Data = nc_vargetr(FI.Filename,FI.Dataset(varid+1).Name,start_coord,count_coord);
    if length(count_coord)>1
        Data = reshape(Data,count_coord);
    end
    Data = Data(RS_netcdf{:});
end
%
if ~isa(Data,'double') && ~isa(Data,'char') && ~isa(Data,'string')
    Data = double(Data);
end
% if ~isa(Data,'char') && ~isa(Data,'string')
%     fprintf('file: %s\nvariable: %s\n',FI.Filename,FI.Dataset(varid+1).Name)
%     fprintf('min: %g\nmax: %g\nhas NaNs: %i\n',min(Data(~isnan(Data))),max(Data(~isnan(Data))),any(isnan(Data(:))));
% end
%
permuted(permuted==0)=[];
if length(permuted)>1
    Data=permute(Data,permuted);
end
%
if ~isempty(Info.Attribute)
    Attribs = {Info.Attribute.Name};
    %
    missval = strmatch('missing_value',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value; % might be a vector according to https://www.unidata.ucar.edu/software/netcdf/docs/attribute_conventions.html
        Data(ismember(Data,missval))=NaN;
    end
    %
    missval = strmatch('valid_min',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        Data(Data<missval)=NaN;
    end
    %
    missval = strmatch('valid_max',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        Data(Data>missval)=NaN;
    end
    %
    missval = strmatch('valid_range',Attribs,'exact');
    if ~isempty(missval)
        missval = Info.Attribute(missval).Value;
        Data(Data<missval(1) | Data>missval(2))=NaN;
    end
    %
    missval = strmatch('_FillValue',Attribs,'exact');
    if ~isempty(missval)
        % the following code is not allowed for Datatype = char, in
        % nc_interpret we should have removed _FillValue attributes for
        % char.
        missval = Info.Attribute(missval).Value;
        if ~isnumeric(missval)
            % _FillValue = "-999.9f" is not valid
        else
            switch netcdf_use_fillvalue
                case 'exact_match'
                    Data(Data==missval) = NaN;
                otherwise % 'valid_range'
                    if missval > 0
                        Data(Data>=missval) = NaN;
                    else
                        Data(Data<=missval) = NaN;
                    end
            end
        end
    else
        % NCL standard or general standard?
        % The following are the default settings for _FillValue by type: 
        %
        switch Info.Datatype
            case 'short'
                Data(Data<=-32767)=NaN;
            case 'int'
                Data(Data<=-2147483647)=NaN;
            case {'single','double'}
                Data(Data>=9.9692099683868690e+36)=NaN;
        end
    end
    %
    scale_factor = strmatch('scale_factor',Attribs,'exact');
    add_offset   = strmatch('add_offset'  ,Attribs,'exact');
    if ~isempty(scale_factor) || ~isempty(add_offset)
        if ~isempty(scale_factor)
            scale_factor = Info.Attribute(scale_factor).Value;
        else
            scale_factor = 1.0;
        end
        if ~isempty(add_offset)
            add_offset = Info.Attribute(add_offset).Value;
        else
            add_offset = 0.0;
        end
        Data = Data*scale_factor + add_offset;
    end
end
