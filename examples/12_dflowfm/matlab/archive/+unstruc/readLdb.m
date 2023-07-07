function [polyxs, polyys] = readLdb(filename)
%unstruc.readLdb Read a land boundary (= a set of polylines).
%   [xs, ys] = unstruc.readLdb(ldbfilename) reads land boundary from
%       specified file and stores it as a set of polylines in two cell
%       arrays with the x- and y-coordinates, respectively.
%
%   LDB file format:
%   <polylinename>
%   <int nr of rows
%
%   See also unstruc.plotLdb

% $Id: readLdb.m 13354 2010-11-02 18:16:37Z dam_ar $

fid = fopen(filename,'r');
if (fid == -1)
    error('unstruc.readLdb: cannot open file ''%s''.', filename);
end

xymiss = 999.999;


fprintf('Scanning LDB file...\n');
polyxs = {}; % set of polylines
polyys = {};
while ~feof(fid)
    blank = true;
    while (blank)
        head   = fgetl(fid);
        blank = all(isspace(head));
    end
    if feof(fid)
        break;
    end
    % New block started, now read dims and all coordinates.
    dims   = fscanf(fid, '%d', 2);
    if numel(dims) ~= 2 && ~isnumeric(dims)
        warning('unstruc.readLdb: Encountered invalid dimension-line. Exiting.');
        break;
    end
    
    fmt = repmat('%f', 1, dims(2)); % should become '%f%f', for example.
    coords = textscan(fid, fmt, dims(1)); % limit nr of lines read.

    polyx  = []; % one polyline: array with coordinates.
    polyy  = [];
    numl = length(coords{1});
    fprintf('Reading LDB block');
    progress=0;
    for i = 1:numl
        if ceil(100*i/numl) > progress
            progress = progress+1;
            fprintf('.');
        end
        if coords{1}(i) == xymiss && coords{2}(i) == xymiss
            polyxs{end+1} = polyx;
            polyys{end+1} = polyy;
            polyx = [];
            polyy = [];
        else
            polyx(end+1) = coords{1}(i);
            polyy(end+1) = coords{2}(i);
        end
    end
    if ~isempty(polyx)
            polyxs{end+1} = polyx;
            polyys{end+1} = polyy;
    end

    fprintf('\n');
end
fclose(fid);
end
