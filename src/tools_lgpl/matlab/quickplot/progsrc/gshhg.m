function data = gshhg(cmd,varargin)
%GSHHG Read and plot GSHHG data (in netCDF format).
%   Routine to perform a limited set of operations on the GSHHG data set
%   (in particular the netCDF version thereof). GSHHG is A Global
%   Self-consistent, Hierarchical, High-resolution Geography Database. For
%   more information see one of the following two links:
%   http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
%   http://www.soest.hawaii.edu/pwessel/gshhg/index.html
%   The current version of this tool has been tested on version 2.2.2.
%
%   HANDLES = GSHHG('plot', ...
%                   'rootfolder',ROOTFOLDER, ...
%                   'type',TYPE, ...
%                   'resolution',RESOLUTION, ...
%                   'bins',BINS, ...
%                   'parent',AX)
%   reads the data for the specified BINS from the netCDF file
%   binned_TYPE_RESOLUTION.nc located in ROOTFOLDER and plots the data in
%   axes AX. If the bins are not specified the data for all bins will be
%   plotted. If the resolution is not specified the coarse resolution data
%   set will be used. If the type is not specified the GSHHS data set will
%   be used, and if the parent is not specified then the plot will use the
%   current axes.
%
%   Known data set types are:
%     * coast/shore lines (file names contains: gshhs)
%     * rivers (file name contains: river)
%     * borders (file name contains: border)
%
%   Known resolutions are:
%     * coarse/crude (c)
%     * low (l)
%     * intermediat (i)
%     * high (h)
%     * fine/full (f).
%
%   See also LANDBOUNDARY, SHAPE, M_GSHHS (of M_MAP toolbox).

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

cmd = lower(cmd);
switch cmd
    case {'plot','bins','res','type'}
    otherwise
        error('Command "%s" not implemented.',cmd)
end
type = 'GSHHS';
unknown_type = 0;
res = 'auto';
unknown_res  = 0;
ibin = 'auto';
parent = 'gca';
color = 'default';
i=1;
while i<=nargin-1
    switch lower(varargin{i})
        case {'root','rootfolder'}
            root = varargin{i+1};
        case 'type'
            type = varargin{i+1};
            switch lower(type)
                case {'s','shoreline','shorelines','shore lines','shore','shores','gshhs','c','coastline','coastlines','coast','coasts','gshhc'}
                    type = 'GSHHS';
                case {'r','river','rivers'}
                    type = 'river';
                case {'b','border','borders','country and state borders'}
                    type = 'border';
                otherwise
                    unknown_type = 1;
            end
        case 'resolution'
            res = varargin{i+1};
            switch lower(res)
                case {'coarse','crude','c'}
                    res = 'c';
                case {'low','l'}
                    res = 'l';
                case {'intermediate','i'}
                    res = 'i';
                case {'high','h'}
                    res = 'h';
                case {'full','fine','f'}
                    res = 'f';
                otherwise
                    unknown_res = 1;
            end
        case 'bins'
            ibin = varargin{i+1};
        case 'parent'
            parent = varargin{i+1};
        case 'color'
            color = varargin{i+1};
    end
    i=i+2;
end
%
if strcmp(cmd,'type')
    data = type;
    return
end
%
if isequal(parent,'gca')
    parent = gca;
end
%
if strcmp(cmd,'res')
    xlim = get(parent,'xlim');
    ylim = get(parent,'ylim');
    dx = xlim(2)-xlim(1);
    dy = ylim(2)-ylim(1);
    %
    data = 'c';
    res = {'c' 'l' 'i' 'h' 'f'};
    for i = 1:5
        try
            ncfile = [root filesep 'binned_' type '_' res{i} '.nc'];
            min_per_bin = double(nc_varget(ncfile,'Bin_size_in_minutes'));
            deg_per_bin = min_per_bin/60;
            %
            data = res{i};
            if dx/deg_per_bin>4 || dy/deg_per_bin>4
                return
            end
        catch
        end
    end
    return
elseif isequal(res,'auto')
   res = gshhg('res','root',root,'type',type,'parent',parent);
end
%
try
    ncfile = [root filesep 'binned_' type '_' res '.nc'];
    nr_totbin = double(nc_varget(ncfile,'N_bins_in_file'));
catch
    if unknown_type
        error('Data set "%s" not recognized.',type)
    elseif unknown_res
        error('Resolution "%s" unknown.',res)
    else
        error('Unable to open file "%s".',ncfile)
    end
end
%
if isequal(ibin,'auto')
    if isequal(cmd,'bins')
        ibin = 0:nr_totbin-1;
    else
        ibin = gshhg('bins','root',root,'type',type,'res',res,'parent',parent);
    end
end
%
maxuint16 = 65535;
%
min_per_bin = double(nc_varget(ncfile,'Bin_size_in_minutes'));
deg_per_bin = min_per_bin/60;
%
nr_lonbin = double(nc_varget(ncfile,'N_bins_in_360_longitude_range'));
nr_latbin = double(nc_varget(ncfile,'N_bins_in_180_degree_latitude_range'));
nr_segtot = double(nc_varget(ncfile,'N_segments_in_file'));
nr_pnttot = double(nc_varget(ncfile,'N_points_in_file'));
%
ilon_of_bin = mod(ibin,nr_lonbin);
ilat_of_bin = nr_latbin-1 - (ibin-ilon_of_bin)/nr_lonbin;
%
xlim = get(parent,'xlim');
if strcmp(cmd,'bins')
    ylim = get(parent,'ylim');
    lon = ilon_of_bin*deg_per_bin;
    lat = -90 + ilat_of_bin*deg_per_bin;
    %
    in_view = ((lon>=xlim(1) & lon+deg_per_bin<=xlim(2)) | ...
               (lon< xlim(1) & lon+deg_per_bin> xlim(1)) | ...
               (lon< xlim(2) & lon+deg_per_bin> xlim(2)) | ...
               (lon-360>=xlim(1) & lon-360+deg_per_bin<=xlim(2)) | ...
               (lon-360< xlim(1) & lon-360+deg_per_bin> xlim(1)) | ...
               (lon-360< xlim(2) & lon-360+deg_per_bin> xlim(2))) & ...
              ((lat>=ylim(1) & lat+deg_per_bin<=ylim(2)) | ...
               (lat< ylim(1) & lat+deg_per_bin> ylim(1)) | ...
               (lat< ylim(2) & lat+deg_per_bin> ylim(2)));
    data = ibin(in_view);
    return
end
too_far_east = deg_per_bin*ilon_of_bin>xlim(2);
ilon_of_bin(too_far_east) = ilon_of_bin(too_far_east)-360/deg_per_bin;
%
first_seg_of_bin = nc_varget(ncfile,'Id_of_first_segment_in_a_bin');
first_seg_of_bin = double(first_seg_of_bin(ibin+1));
nseg_per_bin = nc_varget(ncfile,'N_segments_in_a_bin');
nseg_per_bin = double(nseg_per_bin(ibin+1));
%
handle = zeros(1,length(ibin));
bin_start = 1;
while bin_start <= length(ibin)
    %
    % load and plot data for consecutive segments (in consecutive bins if
    % there are no empty bins) in one step.
    %
    first_seg = first_seg_of_bin(bin_start);
    nseg = nseg_per_bin(bin_start);
    bin_end = bin_start;
    while bin_end < length(ibin)
        if first_seg+nseg == first_seg_of_bin(bin_end+1)
            bin_end = bin_end+1;
            nseg = nseg+nseg_per_bin(bin_end);
        else
            break
        end
    end
    %
    if first_seg + nseg == nr_segtot
        pnt_id = nc_varget(ncfile,'Id_of_first_point_in_a_segment',first_seg,nseg);
        npnt_seg = double([diff(pnt_id);nr_pnttot-pnt_id(end)]);
    else
        pnt_id = nc_varget(ncfile,'Id_of_first_point_in_a_segment',first_seg,nseg+1);
        npnt_seg = double(diff(pnt_id));
    end
    %
    first_pnt = double(pnt_id(1));
    npnt_totseg = sum(npnt_seg);
    if npnt_totseg>0
        x = double(nc_varget(ncfile,'Relative_longitude_from_SW_corner_of_bin',first_pnt,npnt_totseg));
        y = double(nc_varget(ncfile,'Relative_latitude_from_SW_corner_of_bin',first_pnt,npnt_totseg));
        x(x<0) = x(x<0) + maxuint16;
        y(y<0) = y(y<0) + maxuint16;
        %
        if nseg>1
            % allocate space for NaNs in case of multiple segments
            x(end+nseg-1)=0;
            y(end+nseg-1)=0;
        end
        %
        lastpnt = npnt_totseg;
        isegtot = nseg;
        for j = bin_end:-1:bin_start
            for iseg = 1:nseg_per_bin(j)
                ipnt_org = (lastpnt-npnt_seg(isegtot)+1):lastpnt;
                ipnt_new = ipnt_org+isegtot-1;
                x(ipnt_new) = deg_per_bin*(ilon_of_bin(j)+x(ipnt_org)/maxuint16);
                y(ipnt_new) = -90 + deg_per_bin*(ilat_of_bin(j)+y(ipnt_org)/maxuint16);
                %
                if ipnt_new(1)>1
                    % insert NaNs in case of multiple segments
                    x(ipnt_new(1)-1) = NaN;
                    y(ipnt_new(1)-1) = NaN;
                end
                lastpnt = lastpnt-npnt_seg(isegtot);
                isegtot = isegtot-1;
            end
        end
        handle(bin_start) = line(x,y,'parent',parent);
    end
    %
    bin_start = bin_end+1;
end
handle(handle==0)=[];
if ~isequal(color,'default')
    set(handle,'color',color)
end
if nargout>0
    data = handle;
end