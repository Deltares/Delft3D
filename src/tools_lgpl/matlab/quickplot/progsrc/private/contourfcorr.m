function [cout,H,CS] = contourfcorr(varargin)
%CONTOURFCORR Filled contour plot (corrected).
%   CONTOURF(...) is the same as CONTOUR(...) except that the contours
%   are filled.  Areas of the data at or above a given level are filled.
%   Areas below a level are either left blank or are filled by a lower
%   level.  NaN's in the data leave holes in the filled contour plot.
%
%   C = CONTOURF(...) returns contour matrix C as described in CONTOURC
%   and used by CLABEL.
%
%   [C,H,CF] = CONTOURF(...) also returns a column vector H of handles
%   to PATCH objects and the contour matrix CF for the filled areas.
%   The UserData property of each object contains the height value for each
%   contour.
%
%   Example
%      z=peaks; contourf(z), hold on, shading flat
%      [c,h]=contour(z,'k-'); clabel(c,h), colorbar
%
%   See also CONTOUR, CONTOUR3, CLABEL, COLORBAR.

% To correct erroneous ordering of the full area patches
% when the grid is clipped using NaN in the values matrix
% and a constant value for the coordinates. Reason for
% error: the NaNs are replaced by a small value, for
% contour lines close to that value the enclosed area is
% smaller (goes to zero) than the area enclosed by a
% contour line of approximately min(val). This causes
% the contours for smaller values to be plotted after the
% contours for the larger values. The lowest value will
% appear to belong to lie between the smallest thresholds.
% The following code reproduces this phenomenon:
%
% [xx,yy]=meshgrid(1:10,1:10);
% xx([1 end],:)=0;
% xx(:,[1 end])=0;
% yy=xx';
% zz=ones(10,10);
% zz(5,5)=2;
% zz(xx==0)=NaN;
% surf(xx,yy,zz)
% figure;
% contourf(xx,yy,zz,[-2 -1 0 1.5])
% colorbar
%
% This function solves this problem by basing the ordering on
% a dummy, rectangular grid.

% Original version
%   Author: R. Pawlowicz (IOS)  rich@ios.bc.ca   12/14/94
%   Copyright (c) 1984-98 by The MathWorks, Inc.
%   %Revision: 1.23 %  %Date: 1998/04/09 13:17:29 %
%
% Modified and improved version
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

if nargin<1
    error('Not enough input arguments.')
elseif nargin>6
    error('Too many input arguments.')
end

if ischar(varargin{1})
    style = varargin{1};
    %
    switch style
        case 'line'
            style = 1;
        case 'cline'
            style = 2;
        case 'patch'
            style = 3;
    end
    argin = varargin(2:end);
else
    style = 3;
    argin = varargin;
end
nin = length(argin);

% Check for empty arguments.
for i = 1:nin
    if isempty(argin{i})
        error ('Invalid Argument - Input matrix is empty');
    end
end

% Trim off the last arg if it's a string (line_spec).
if ischar(argin{end})
    [lin,col,mark,msg] = colstyle(argin{end});
    if ~isempty(msg), error(msg); end
    nin = nin - 1;
else
    lin = '';
    col = '';
end

switch nin
    case 4
        [x,y,z,nv] = deal(argin{1:4});
        if (size(y,1)==1), y=y'; end;
        if (size(x,2)==1), x=x'; end;
        [mz,nz] = size(z);
    case 3
        [x,y,z] = deal(argin{1:3});
        nv = [];
        if (size(y,1)==1), y=y'; end;
        if (size(x,2)==1), x=x'; end;
        [mz,nz] = size(z);
    case 2
        [z,nv] = deal(argin{1:2});
        [mz,nz] = size(z);
        x = 1:nz;
        y = (1:mz)';
    case 1
        z = argin{1};
        [mz,nz] = size(z);
        x = 1:nz;
        y = (1:mz)';
        nv = [];
end
[mz0,nz0] = size(z);
x0 = 1:nz0;
y0 = (1:mz0)';

if nin <= 2
    [mc,nc] = size(argin{1});
    lims = [1 nc 1 mc];
else
    lims = [min(argin{1}(:)),max(argin{1}(:)), ...
        min(argin{2}(:)),max(argin{2}(:))];
end

i = find(isfinite(z));
minz = min(z(i));
maxz = max(z(i));

% Generate default contour levels if they aren't specified
if length(nv) <= 1
    if isempty(nv)
        CS=contourc([minz maxz ; minz maxz]);
    else
        CS=contourc([minz maxz ; minz maxz],nv);
    end
    
    % Find the levels
    ii = 1;
    nv = minz; % Include minz so that the contours are totally filled
    while (ii < size(CS,2))
        nv=[nv CS(1,ii)];
        ii = ii + CS(2,ii) + 1;
    end
end

% Don't fill contours below the lowest level specified in nv.
% To fill all contours, specify a value of nv lower than the
% minimum of the surface.
draw_min = any(nv <= minz) & style>2;

% Get the unique levels
largest_value_LE_minz = max(nv(nv<=minz));
no_lower_bound = largest_value_LE_minz == -realmax;
nv = sort([minz nv(:)']);
zi = [1, find(diff(nv))+1];
nv = nv(zi);

% Surround the matrix by a very low region to get closed contours, and
% replace any NaN with low numbers as well.

zz=[ repmat(NaN,1,nz+2) ; repmat(NaN,mz,1) z repmat(NaN,mz,1) ; repmat(NaN,1,nz+2)];
if style>2
    kk=isnan(zz(:));
    zz(kk)=-realmax;
end
% using -realmax instead of
% * -inf since this seems to result in contours drawn at value+eps rather
%   than value-eps at least for the example given above in R2009B
% * minz-1e4*(maxz-minz) since this moves contours too far in the direction
%   of NaN missing data points.

xx0 = [2*x0(:,1)-x0(:,2), x0, 2*x0(:,nz0)-x0(:,nz0-1)];
yy0 = [2*y0(1,:)-y0(2,:); y0; 2*y0(mz0,:)-y0(mz0-1,:)];
warnstate=[];
try
    warnstate    = warning('query', 'MATLAB:contours:DeprecatedErrorOutputArgument');
    warnstate(2) = warning('query', 'MATLAB:contours:EmptyErrorOutputArgument');
    warning('off', 'MATLAB:contours:DeprecatedErrorOutputArgument')
    warning('off', 'MATLAB:contours:EmptyErrorOutputArgument')
end
try
    [CS,msg]=contours(xx0,yy0,zz,nv);
catch
    if ~isempty(warnstate)
        warning(warnstate)
    end
    rethrow(lasterror)
end
if ~isempty(warnstate)
    warning(warnstate)
end
if ~isempty(msg)
    error(msg)
end

% Find the indices of the curves in the c matrix, and get the
% area of closed curves in order to draw patches correctly.
ii = 1;
ncurves = 0;
I = [];
Area=[];
while ii < size(CS,2)
    nl=CS(2,ii);
    ncurves = ncurves + 1;
    I(ncurves) = ii;
    if style>2
        if nl==1
            Area(ncurves) = 0;
        else
            xp=CS(1,ii+(1:nl));  % First patch
            yp=CS(2,ii+(1:nl));
            Area(ncurves)=sum( diff(xp).*(yp(1:nl-1)+yp(2:nl))/2 );
        end
    end
    ii = ii + nl + 1;
end

if style>2 && ncurves>0
    % Plot patches in order of decreasing size. This makes sure that
    % all the levels get drawn, no matter if we are going up a hill or
    % down into a hole. Lowest curve is largest and encloses higher data
    % always.
    keep = true(size(Area));
    [sortedAreas,IA]=sort(-abs(Area));
    uniqueAreas = unique(sortedAreas);
    % check for duplicate areas - first by size
    for uA = uniqueAreas
        ii = find(sortedAreas == uA);
        if numel(ii) == 1
            % area size is unique, no further checks needed
            continue
        else
            % multiple polygons of same size
            nPnts = CS(2,I(IA(ii)));
            uniquePoints = unique(nPnts);
            for uP = uniquePoints
                jj = find(nPnts == uP);
                if numel(jj) == 1
                    % number of polygon points is unique, no further checks
                    % needed
                    continue
                else
                    % multiple polygons of same size and same number of
                    % points ... check if they are the same
                    jj = ii(jj);
                    % polygons can only be identical if at least one is
                    % less than minz
                    levels = CS(1,I(IA(jj)));
                    holes = jj(levels < minz);
                    nonholes = jj(levels >= minz);
                    level = [];
                    poly = [];
                    for hole = holes
                        for nhole = nonholes
                            if isequal(CS(1:2,I(IA(hole))+(1:uP)), ...
                                    CS(1:2,I(IA(nhole))+(1:uP)))
                                % twice the same polygon ...
                                if uA == uniqueAreas(1)
                                    % for the biggest area, choose the highest level
                                    new_level = CS(1,I(IA(nhole)));
                                    if isempty(level)
                                        level = new_level;
                                        keep(hole) = false;
                                        poly = nhole;
                                    elseif new_level > level
                                        % new polygon has higher level
                                        level = new_level;
                                        keep(poly) = false;
                                        poly = nhole;
                                    else
                                        % other polygon had higher level
                                        keep(nhole) = false;
                                    end
                                else
                                    % for other areas, choose the hole
                                    keep(nhole) = false;
                                    nonholes(nonholes == nhole) = [];
                                end
                            else
                                % polygons actually not the same, do nothing
                            end
                        end
                    end
                end
            end
        end
    end
    IA = IA(keep);
else
    IA = 1:ncurves;
end

newplot;
if ~ishold
    view(2);
    set(gca,'box','on');
    set(gca,'xlim',lims(1:2),'ylim',lims(3:4))
end

if ~ischar(get(gca,'color'))
    bg = get(gca,'color');
else
    bg = get(gcf,'color');
end
if isempty(col)
    edgec = get(gcf,'defaultsurfaceedgecolor');
else
    edgec = col;
end
if isempty(lin)
    edgestyle = get(gcf,'defaultpatchlinestyle');
else
    edgestyle = lin;
end

% Tolerance for edge comparison
xtol = 0.1*(lims(2)-lims(1))/size(z,2);
ytol = 0.1*(lims(4)-lims(3))/size(z,1);

if nargout>0
    cout = [];
end
xx=x(:)'; yy=y(:)';
szx=size(x);
if min(szx)==1
    szn=length(yy);
    szm=length(xx);
else
    szn=szx(1);
    szm=szx(2);
end

H    = zeros(ncurves,1);
CS2  = cell(1,max(IA));
cout = cell(1,max(IA));
iH   = 0;
for jj=IA
    nl=CS(2,I(jj));
    lev=CS(1,I(jj));
    if (lev ~= -realmax) && (lev ~= minz || draw_min )
        mp=CS(1,I(jj)+(1:nl));
        np=CS(2,I(jj)+(1:nl));
        ipm=floor(mp);
        ipn=floor(np);
        ipm1=min(szm,ipm+1); % prevent index szm+1
        ipn1=min(szn,ipn+1); % prevent index szn+1
        dmp=mp-ipm;
        dnp=np-ipn;
        ipm=max(1,ipm); % prevent index 0
        ipn=max(1,ipn); % prevent index 0
        if min(szx)==1
            xp=(1-dmp).*xx(ipm)+dmp.*xx(ipm1);
            yp=(1-dnp).*yy(ipn)+dnp.*yy(ipn1);
        else
            ip=sub2ind(szx,ipn,ipm);
            ip_m=sub2ind(szx,ipn,ipm1);
            ip_n=sub2ind(szx,ipn1,ipm);
            ip_mn=sub2ind(szx,ipn1,ipm1);
            xp=(1-dmp).*((1-dnp).*xx(ip)+dnp.*xx(ip_n))+dmp.*((1-dnp).*xx(ip_m)+dnp.*xx(ip_mn));
            yp=(1-dmp).*((1-dnp).*yy(ip)+dnp.*yy(ip_n))+dmp.*((1-dnp).*yy(ip_m)+dnp.*yy(ip_mn));
            %
            mnp=[mp;np];
            mnc=mnp(:,1:end-1)+diff(mnp,1,2)/2;
            imnc=floor(mnc);
            ipc=sub2ind(szx,imnc(2,:),imnc(1,:));
            dmnc=(mnc-imnc)>0;
            ii=find(isnan(z(ipc)+z(ipc+dmnc(2,:))+z(ipc+mz*dmnc(1,:))+z(ipc+dmnc(2,:)+mz*dmnc(1,:))));
            if isempty(ii)
                ii2=[];
            else
                ndiag=length(ii);
                ii2=ii+(1:ndiag);
                nl = nl+ndiag;
            end
            isel=1:nl;
            isel(ii2)=[];
            xp(isel)=xp;
            yp(isel)=yp;
            for l = 1:length(ii)
                i2=ii2(l);
                il=ipc(ii(l));
                if isnan(z(il))
                    i1=il+mz+1;
                elseif isnan(z(il+1))
                    i1=il+mz;
                elseif isnan(z(il+mz))
                    i1=il+1;
                elseif isnan(z(il+mz+1))
                    i1=il;
                end
                xp(i2)=x(i1);
                yp(i2)=y(i1);
            end
        end
        CS2{jj} = [lev xp;nl yp];
        %
        % If the patch is filled then we need to shift the level when going
        % down. You can tell whether we are going up or down by checking
        % the sign of the area (since curves are oriented so that the high
        % side is always the same side).
        %
        if style>2 && (sign(Area(jj)) ~= sign(Area(IA(1))))
            kk=find(nv==lev);
            if kk > 1 + sum(nv<=minz)*(~draw_min)
                lev=nv(kk-1);
            else
                lev=NaN;      % missing data section
            end
        end
        
        iH = iH+1;
        if style==1
            H(iH) = line([xp NaN],[yp NaN],'color',edgec, ...
                'linestyle',edgestyle,'userdata',lev);
        elseif style==2
            H(iH) = patch([xp NaN],[yp NaN],1, ... % can't use color here directly if xp/yp are 1x2 arrays
                'facevertexcdata',repmat(lev,size(xp)+[0 1])', ...
                'edgecolor','flat','facecolor','none', ...
                'linestyle',edgestyle,'userdata',lev);
        elseif (isfinite(lev)),
            H(iH) = patch(xp,yp,lev,'facecolor','flat','edgecolor',edgec, ...
                'linestyle',edgestyle,'userdata',lev);
        else
            H(iH) = patch(xp,yp,lev,'facecolor',bg,'edgecolor',edgec, ...
                'linestyle',edgestyle,'userdata',CS(1,I(jj)));
        end
        iLevel = find(nv==lev);
        if isempty(iLevel)
            % a hole
            setappdata(H(iH),'MinThreshold',NaN);
        elseif nv(iLevel) == minz
            if no_lower_bound
                setappdata(H(iH),'MinThreshold',NaN);
            else
                setappdata(H(iH),'MinThreshold',largest_value_LE_minz);
            end
        else
            setappdata(H(iH),'MinThreshold',nv(iLevel));
        end
        if isempty(iLevel)
            % a hole
            setappdata(H(iH),'MaxThreshold',NaN);
        elseif iLevel == length(nv)
            setappdata(H(iH),'MaxThreshold',NaN);
        else
            setappdata(H(iH),'MaxThreshold',nv(iLevel+1));
        end

        if nargout>0
            xp(abs(xp - lims(1)) < xtol | abs(xp - lims(2)) < xtol) = NaN;
            yp(abs(yp - lims(3)) < ytol | abs(yp - lims(4)) < ytol) = NaN;
            cout{jj} = [lev xp;nl yp];
        end
    end
end
H(iH+1:end) = [];
CS = cat(2,CS2{:});
cout = cat(2,cout{:});

numPatches = length(H);
if numPatches>1 && style>=3
    for i=1:numPatches
        set(H(i), 'faceoffsetfactor', 0, 'faceoffsetbias', (1e-3)+(numPatches-i)/(numPatches-1)/30);
    end
end