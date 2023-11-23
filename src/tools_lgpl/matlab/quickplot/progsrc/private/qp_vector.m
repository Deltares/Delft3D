function l=qp_vector(Parent,vectorstyle,x,y,z,xc,yc,zc,varargin)
%QP_VECTOR Wrapper for QUIVER and QUIVER3.
% vectorstyle = 'rooted arrow', 'centered arrow', 'rooted line'

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

args = varargin;
switch vectorstyle
    case 'centered arrow'
        x = x-xc/2;
        y = y-yc/2;
        if ~isempty(z)
            z = z-zc/2;
        end
    case 'rooted line'
        args{end+1} = 'o';
end
if isempty(z)
    l=LocalQuiver(Parent,x,y,[],xc,yc,[],args{:});
else
    l=LocalQuiver(Parent,x,y,z,xc,yc,zc,args{:});
end
if strcmp(vectorstyle,'rooted line')
    set(l(end),'marker','.')
end


function h = LocalQuiver(cax, varargin)
args = varargin;
nargs = length(args);

% Arrow head parameters
alpha = 0.33; % Size of arrow head relative to the length of the vector
beta = 0.33;  % Width of the base of the arrow head relative to the length
autoscale = 1; % Autoscale if ~= 0 then scale by this.
plotarrows = 1;

filled = 0;
ms = '';
vert = {};

nin = nargs;
% Parse the string inputs
while ischar(args{nin})
    vv = args{nin};
    if ~isempty(vv) && strcmpi(vv(1),'f')
        filled = 1;
        nin = nin-1;
    else
        [l,c,m,msg] = colstyle(vv);
        if ~isempty(msg)
            error('Unknown option "%s".', vv)
        end
        if ~isempty(m), ms = m; plotarrows = 0; end
        if isequal(m,'.'), ms = ''; end % Don't plot '.'
        nin = nin-1;
    end
end

[x,y,z,u,v,w] = deal(args{1:6});
threed = ~isempty(z);

if nin>6
    autoscale = args{nin};
    if length(autoscale)>1
        error('S must be a scalar.')
    end
end

% Scalar expand velocity components.
if numel(u)==1, u = u(ones(size(x))); end
if numel(v)==1, v = v(ones(size(u))); end
if threed && numel(w)==1
      w = w(ones(size(v)));
end

if autoscale
    % Base autoscale value on average spacing in the x and y
    % directions.  Estimate number of points in each direction as
    % either the size of the input arrays or the effective square
    % spacing if x and y are vectors.
    if min(size(x))==1, n=sqrt(numel(x)); m=n; else [m,n]=size(x); end
    delx = diff([min(x(:)) max(x(:))])/n;
    dely = diff([min(y(:)) max(y(:))])/m;
    del = delx.^2 + dely.^2;
    if threed
       delz = diff([min(z(:)) max(z(:))])/max(m,n);
       del = del + delz.^2;
    end
    if del>0
        if threed
            len = sqrt((u.^2 + v.^2 + w.^2)/del);
        else
            len = sqrt((u.^2 + v.^2)/del);
        end
        maxlen = max(len(:));
    else
        maxlen = 0;
    end
    
    if maxlen>0
        autoscale = autoscale*0.9 / maxlen;
    else
        autoscale = autoscale*0.9;
    end
    u = u*autoscale;
    v = v*autoscale;
    if threed
       w = w*autoscale;
    end
end

cax = newplot(cax);
next = lower(get(cax,'NextPlot'));

% Make velocity vectors
x = x(:).';
y = y(:).';
u = u(:).';
v = v(:).';
uu = [x;x+u;repmat(NaN,size(u))];
vv = [y;y+v;repmat(NaN,size(u))];
if threed
    z = z(:).';
    w = w(:).';
    ww = [z;z+w;repmat(NaN,size(u))];
    beta = beta * sqrt(u.*u + v.*v + w.*w) ./ (sqrt(u.*u + v.*v) + eps);
    vert = {ww(:)};
end
h1 = line(uu(:),vv(:),vert{:},'parent',cax);

if plotarrows
    % Make arrow heads and plot them
    hu = [x+u-alpha*(u+beta.*(v+eps));x+u; ...
        x+u-alpha*(u-beta.*(v+eps));repmat(NaN,size(u))];
    hv = [y+v-alpha*(v-beta.*(u+eps));y+v; ...
        y+v-alpha*(v+beta.*(u+eps));repmat(NaN,size(v))];
    if threed
        hw = [z+w-alpha*w;z+w;z+w-alpha*w;repmat(NaN,size(w))];    
        vert = {hw(:)};
    end
    h2 = line(hu(:),hv(:),vert{:},'parent',cax);
else
    h2 = [];
end

if ~isempty(ms) % Plot marker on base
    hu = x;
    hv = y;
    if threed
        vert = {z(:)};
    end
    h3 = line(hu(:),hv(:),vert{:},'parent',cax);
    set(h3,'marker',ms,'color',get(h1,'color'),'linestyle','none')
    if filled, set(h3,'markerfacecolor',get(h1,'color')); end
else
    h3 = [];
end

if nargout>0, h = [h1;h2;h3]; end
