function plotNet(varargin)
%unstruc.plotNet Plot an unstructured net, possibly with flow data.
%   unstruc.plotNet(udata) plots an unstructured net.
%   Basic mode shows the net nodes and the links between them.
%   Input structure udata is as produced by unstruc.readNet or
%   unstruc.readMap
%
%   unstruc.plotNet(..., 'PropertyName',PropertyValue,...) sets the value
%   of the specified property. The following properties are available:
%
%   PlotType: Specifies what to plot. Accepted values:
%       'net':       Plots the net as a collection of nodes and links.
%       'flowcells': Plots the 2D flowcells and contours for 1D flow nodes.
%       'flow':      Plots solution data at cell centers (now: s1)
%
%   Time: Specifies for which time to plot. If unspecified, last time in
%       data series is used. Example: 'Time', 24*3600, ...
%
%   PlotFunc: now s1 by default [TODO]
%
%   Plot styles
%   NetNodeStyle: character string or cell array with prop/value pairs that
%       defines the point style for 2D netnodes. Default: ''
%   NetNode1DStyle: character string or cell array with prop/value pairs
%       that defines the point style for 1D netnodes. Default: thick black
%       dots.
%   NetLinkStyle: character string or cell array with prop/value pairs that
%       defines the line style for 2D netlinks. Default: '-r'
%   NetLink1DStyle: character string or cell array with prop/value pairs
%       that defines the line style for 1D netlinks. Default: '-b'
%
%   XYLim: [x1, x2, y1, y2] defines the limits for the x- and y-axis.
%       Useful when plotting huge nets to save time and memory.
%       [x1, x2] uses the same limits for y.
%
%   unstruc.plotNet(ax, ...) uses the specified axes handle for plotting.
%   unstruc.plotNet(..., 'parent', ax, ...) does this too.
%
%   See also unstruc.readNet

%   $Id: plotNet.m 13354 2010-11-02 18:16:37Z dam_ar $


%% Handle parameters
% Check if an axes handle was passed, otherwise create new axes.
[ax,args, nargs] = axescheck(varargin{:});
if isempty(ax)
    ax = gca;
else
    axes(ax);
end
cla;
hold on;

% Split arglist into udata and a list of all property-value pairs.
[udata, args] = parseparams(args);
udata = udata{1};

% Parse all parameters that specify what to plot
opts.PlotType = {'net', 'flowcells', 'flow'};
opts.Time     = realmax(); % Select last time by default.
opts.NetNodeStyle   = []; % Plot styles defaults are set below
opts.NetNode1DStyle = []; % (parseargs does not like multi-type structs)
opts.NetLinkStyle   = [];
opts.NetLink1DStyle = [];
opts.NetCellStyle   = [];
opts.NetCell1DStyle = [];
opts.FlowFieldStyle = [];
opts.XYLim = [];

opts = parseargs(opts, args{:});

opts.NetNodeStyle   = toCell(opts.NetNodeStyle, {'.','MarkerSize',5,'MarkerFaceColor','k','markerEdgeColor','k'});
opts.NetNode1DStyle = toCell(opts.NetNode1DStyle, {'.','MarkerSize',5,'MarkerFaceColor','k','markerEdgeColor','k'});
opts.NetLinkStyle   = toCell(opts.NetLinkStyle,   {'b-','LineWidth',2});
opts.NetLink1DStyle = toCell(opts.NetLink1DStyle, {'r-','LineWidth',2});
opts.NetCellStyle   = toCell(opts.NetCellStyle,   {'EdgeColor','k','FaceColor','b'});
opts.NetCell1DStyle = toCell(opts.NetCell1DStyle, {'EdgeColor','k','FaceColor','r'});
opts.FlowFieldStyle = toCell(opts.FlowFieldStyle, {'Color','k'});

% Axes limits (for faster plotting)
if numel(opts.XYLim)==2
    opts.XYLim = [opts.XYLim(:); opts.XYLim(:)];
elseif ~isempty(opts.XYLim) && numel(opts.XYLim) ~= 4
    warning('Invalid XYLim specification. Using entire domain.');
    opts.XYLim = [];
end

% Include flow cell data (or only net data)
haveCellData = isfield(udata, 'FlowElem_xcc'); % Other fields are now assumed to exist as well...

if strcmp(opts.PlotType,'flow') % For 'flow', plot a subtle net underneath it.
    opts.NetNodeStyle   = {};
    opts.NetNode1DStyle = {};
    opts.NetLinkStyle   = {'LineStyle','none', 'Marker','none'};
    opts.NetLink1DStyle = {'LineStyle','none', 'Marker','none'};%'-', 'Color',[.3 .3 .3]};
end

%% Handle axes limits
if isempty(opts.XYLim)
    nNetNode  = size(udata.NetNode_x, 1);
    nNetLink  = size(udata.NetLink,2);
    in = 1:nNetNode;
    il = 1:nNetLink;
    if haveCellData
        nCell = size(udata.FlowElem_xcc, 1);
        ic = 1:nCell;
        nFlowLink = size(udata.FlowLink, 2);
        ifl = 1:nFlowLink;
        % todo 1D
    end
else
    in = find(udata.NetNode_x >= opts.XYLim(1) & udata.NetNode_x <= opts.XYLim(2) & ...
              udata.NetNode_y >= opts.XYLim(3) & udata.NetNode_y <= opts.XYLim(4));
    nNetNode = numel(in);
    
    il = find(any(udata.NetNode_x(udata.NetLink) >= opts.XYLim(1) & udata.NetNode_x(udata.NetLink) <= opts.XYLim(2) & ...
                  udata.NetNode_y(udata.NetLink) >= opts.XYLim(3) & udata.NetNode_y(udata.NetLink) <= opts.XYLim(4)));
    nNetLink = numel(il);

    if haveCellData
        % ic is first still a logical array, will be index array later.
        ic = any(udata.FlowElemContour_x >= opts.XYLim(1) & udata.FlowElemContour_x <= opts.XYLim(2) & ...
                 udata.FlowElemContour_y >= opts.XYLim(3) & udata.FlowElemContour_y <= opts.XYLim(4));

        % ifl contains all link numbers that have one or two nodes in ic.
        ifl = find(ic(udata.FlowLink(1,:)) | ic(udata.FlowLink(2,:)));
        ic  = find(ic);

        nCell = numel(ic);
        nFlowLink = size(udata.FlowLink, 1);
    end
end


%% Analyze some net data
il1 = il(udata.NetLinkType(il)==1);
il  = il(udata.NetLinkType(il)~=1);

NetNodeType(1:numel(udata.NetNode_x)) = 0;
NetNodeType(udata.NetLink(1,il1))    = 1;
NetNodeType(udata.NetLink(2,il1))    = 1;
NetNodeType(udata.NetLink(1,il))     = 2;
NetNodeType(udata.NetLink(2,il))     = 2;
in1 = find(NetNodeType==1);
in  = find(NetNodeType==2);



%% Start plotting
axis equal;

% First plot step: net or flowcells. May be overlaid by second plot step.
switch opts.PlotType
case {'net', 'flow'}
    % netlinks
    lhs1 = plot(udata.NetNode_x(udata.NetLink(:,il1)), udata.NetNode_y(udata.NetLink(:, il1)), opts.NetLink1DStyle{:});
    lhs2 = plot(udata.NetNode_x(udata.NetLink(:,il)),  udata.NetNode_y(udata.NetLink(:, il)),  opts.NetLinkStyle{:});

    % netnodes:
    if ~isempty(opts.NetNode1DStyle)
        plot(udata.NetNode_x(in1), udata.NetNode_y(in1), opts.NetNode1DStyle{:});
    end
    if ~isempty(opts.NetNodeStyle)
        plot(udata.NetNode_x(in), udata.NetNode_y(in), opts.NetNodeStyle{:});
    end
    
    title(sprintf('Unstruc net: #nodes=%d, #1D netlinks=%d, # 2D netlinks=%d', ...
                  numel(udata.NetNode_x), sum(udata.NetLinkType==1), sum(udata.NetLinkType==2)));
case 'flowcells'
	for k=1:nCell
        j = ic(k);
        nCN = find(isnan(udata.FlowElemContour_x(:,j)), 1);
        if isempty(nCN)
            nCN = size(udata.FlowElemContour_x, 1);
        else
            nCN = nCN-1;
        end
        ph = fill(udata.FlowElemContour_x(1:nCN, j), ...
                  udata.FlowElemContour_y(1:nCN, j),'r');
        set(ph, opts.NetCellStyle{:});
        %lh = line(udata.NetNode_x(udata.NetCellNode(2:nCN+1, j)), ...
        %          udata.NetNode_y(udata.NetCellNode(2:nCN+1, j)));
        %set(lh, opts.NetCell1DStyle{:});
        %set(ph, opts.NetCell1DStyle{:});

        % 2. Draw underlying net node for 1D flow cell
        %lh = line(udata.NetNode_x(udata.NetCellNode(2, j)), ...
        %udata.NetNode_y(udata.NetCellNode(2, j)));
        %set(lh, 'Marker','o','MarkerSize',4,'MarkerEdgeColor','k','MarkerFaceColor','r');
    end
%    title(sprintf('Flow cells: #1D flowcells=%d, #2D flowcells=%d, # 1D flowlinks=%d, #2D flowlinks=%d', ...
%                  size(udata.NetCellLink1D, 2), size(udata.NetCellLink, 2), ...
%                  sum(udata.NetCellNode(1,:)==1), sum(udata.NetCellNode(1,:)>1), 1, 1));
end

% Second plot step: probably flow data on top of net.
switch opts.PlotType
case 'flow'
    itime = find(udata.time >= opts.Time, 1); % Find first time >= requested time.
    if isempty(itime)
        itime = size(udata.time, 1);
        if opts.Time ~= realmax()
            warning('Requested time %g not found, using last time %g.', opts.Time, udata.time(itime));
        end
    end
	for k=1:nCell
        j = ic(k);
        nCN = find(isnan(udata.FlowElemContour_x(:,j)), 1);
        if isempty(nCN)
            nCN = size(udata.FlowElemContour_x(:,j), 1);
        else
            nCN = nCN-1;
        end

        ph = patch(udata.FlowElemContour_x(1:nCN, j), ...
                   udata.FlowElemContour_y(1:nCN, j), udata.s1(j,itime));
        set(ph,'EdgeColor','none');
    end
    colorbar;
	quiver(udata.FlowElem_xcc(ic), udata.FlowElem_ycc(ic), ...
    	   udata.ucx(ic,itime), udata.ucy(ic,itime),...
           opts.FlowFieldStyle{:});
    title(sprintf('Flow field + s1 at t=%6.3f',udata.time(itime)));
end

%% Final markup of axes
if ~isempty(opts.XYLim)
    axis equal;
    set(ax, 'XLim', opts.XYLim(1:2), 'YLim', opts.XYLim(3:4));
else
    axis auto;
    axis equal;
end
hold off;
    
end
    %
function cellValue = toCell(value, defaultValue)
% Puts value in a cell array, if it isn't already so, and uses a default
% when empty. (For plot arguments)
    if isempty(value)
        if nargin >=2 
            value = defaultValue;
        else
            cellValue = {};
            return;
        end
    end
    if ~iscell(value)
        cellValue = {value};
    else
        cellValue = value;
    end

end
