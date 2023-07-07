function plotLdb(varargin)
%unstruc.plotLdb Plot a land boundary (= a set of polylines).
%   unstruc.plotLdb(ldbfilename) reads land boundary from specified file
%       and plots it.
%   unstruc.plotLdb(lx, ly) plots land boundaries as specified in lx, ly,
%       which are cell arrays of polylines, e.g.:
%       lx={[1, 1.1, ...],[100, 99,..],..}.
%
%   unstruc.plotLdb(ax, ...) uses the specified axes handle for plotting.
%   unstruc.plotLdb(..., 'parent', ax, ...) does this too.
%
%   See also unstruc.readLdb

% $Id: plotLdb.m 13354 2010-11-02 18:16:37Z dam_ar $

[ax,args, nargs] = axescheck(varargin{:});
if isempty(ax)
    ax = gca;
else
    axes(ax);
end
cla;

%% Arg check + read LDB
if nargs == 0
    error('Missing arguments');
elseif ischar(args{1})
    ldbfile  = args{1};
    [lx, ly] = unstruc.readLdb(ldbfile);
elseif nargin >= 2
    lx = args{1};
    ly = args{2};
else
    error('Invalid arguments');
end

%% Start plotting
for i=1:length(lx)
    lh = line(lx{i},ly{i});
    set(lh, 'Color','k');
end

xlabel(ax, '');
ylabel(ax, '');
set(ax,'XTick',[]);
set(ax,'YTick',[]);
set(ax,'Box','on');
axis equal;
axis tight;
end