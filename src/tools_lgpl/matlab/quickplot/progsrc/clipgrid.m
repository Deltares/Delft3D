function [xo,yo] = clipgrid(x,y,xp,yp,varargin)
%CLIPGRID Clip a grid away from the inside/outside of a polygon.
%   [XO,YO] = CLIPGRID(XI,YI,XP,YP,SIDE) clips the grid specified by the
%   XI,YI coordinates away from the SIDE ('inside' or 'outside') of the
%   polygon given by the XP,YP coordinates. In general the clipping will
%   not be perfect and manual improvements can be made (especially if the
%   polygon has sharp corners).
%
%   [XO,YO] = CLIPGRID(XI,YI,XP,YP) by default clips the grid away from the
%   inside of the polygon.
%
%   CLIPGRID(...) without output arguments plots the original grid, clipped
%   grid, deleted points together with the polygon.
%
%   CLIPGRID(...,FACTOR) uses the specified FACTOR in the evaluation of
%   Distance_Polygon_ClipPoint < FACTOR * Distance_ClipPoint_KeepPoint
%   where the first variable is the distance between the polygon and the
%   point to be clipped and the last variable is the distance between the
%   point to be clipped and the neighboring point to keep. If this
%   condition is satisfied the point to be clipped will be moved onto the
%   polygon; if the condition is not satisfied the point to keep will be
%   moved onto the polygon. By default FACTOR equals 0.9 which means that
%   points to keep will only be shifted if the distance between grid points
%   would otherwise be reduced to less than 10% of the original distance.
%   Small grid cells are least likely to form if FACTOR = 0.5, bigger
%   values of FACTOR lead to less grid distortion.
%
%   CLIPGRID without input arguments shows a simple example.
%
%   Note: because common grid orthogonality rules will be violated (local)
%   momentum conservation is no longer satisfied for Delft3D simulations on
%   such a grid! Apply this method only if local momentum conservation is
%   not relevant for the outcome of your simulation.
%
%   Example 1
%      [x,y]=meshgrid(0:100,0:100);
%      s1 = sin((0:5:90)*pi/180);
%      s2 = fliplr(s1);
%      clipgrid(x,y,[s1*100 s2*50],[s2*100 s1*50],'outside')
%
%   Example 2
%      [x,y]=meshgrid(0:50,0:40);
%      [xo,yo]=clipgrid(x,y,[17 23 13 7],[7 17 23 13]);
%      clipgrid(xo,yo,[37 43 33 27],[17 27 33 23])
%      line([17 23 13 7 17],[7 17 23 13 7],'linewidth',3)

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

clip = 'inside';
factor = 0.9;

if nargin==0
    [x,y,xp,yp]=testcase;
else
    for i = 1:nargin-4
        if ischar(varargin{i})
            clip = varargin{i};
        else
            factor = varargin{i};
        end
    end
end

% This function clips away the grid points INSIDE the polygon if clip
% equals 'inside' and clips away the grid points OUTSIDE the polygon if
% clip equals 'outside'.
%
switch lower(clip)
    case {'inside','in'}
        outside = false;
    case {'outside','out'}
        outside = true;
    otherwise
        error('Invalid clip setting. Expected ''inside'' or ''outside''.')
end

if xp(1)~=xp(end) || yp(1)~=yp(end)
    xp(end+1) = xp(1);
    yp(end+1) = yp(1);
end

if nargout==0
    [x1,y1] = doclipgrid(x,y,xp,yp,outside,factor);
    figure
    mesh(x1,y1,-200+0*x,'edgecolor','k','facecolor',[1 1 .7]);
    hold on
    mesh(x,y,-100+0*x,'edgecolor',[.7 .7 .7],'facecolor','none');
    Deleted = isnan(x1);
    line(x(Deleted),y(Deleted),'marker','.','markersize',5,'linestyle','none','color','r')
    line(xp,yp,-50+xp*0,'linewidth',3,'color',[.3 .3 1],'linestyle','--')
    mesh(x1,y1,0*x,'edgecolor','k','facecolor','none');
    view(0,90)
    grid off
    set(gca,'dataAspectRatio',[1 1 1],'color',[1 .7 .7])
else
    [xo,yo] = doclipgrid(x,y,xp,yp,outside,factor);
end

function [x,y] = doclipgrid(x,y,xp,yp,outside,factor)
[Inside,OnPoly] = inpolygon(x,y,xp,yp);
if outside
    %
    % We are looking at the polygon from inside out, but everything should
    % otherwise be the same.
    %
    Inside = ~Inside;
end
%
% Make sure that inside, outside and onpoly are three disjunct sets. The
% Inside(OnPoly) = false statement is default true but needs reconfirmation
% because we may have switched inside and outside.
%
Inside(OnPoly) = false;
Outside = ~Inside & ~OnPoly;

%
% By default delete only those points that are strictly inside the polygon.
%
ToBeDeleted = conv2(double(Inside),ones(3),'same')==9;

%
% Loop multiple times because some cases are more difficult to handle and
% need several small steps.
%
NRows = size(Inside,1);
NCols = size(Inside,2);
PointShifted = true;
iter = 0;
while PointShifted
    iter = iter + 1;
    PointShifted = iter==1;
    %
    % Loop over all inside points that have not yet been marked as to be
    % deleted.
    %
    [I,J] = find(Inside & ~ToBeDeleted);
    for i = 1:length(I)
        Ic = I(i);
        Jc = J(i);
        del = false;
        cont = false;
        if Ic==1 || Ic==NRows
            if Ic==1
                % top boundary
                dr = 1;
            else
                % bottom boundary
                dr = -1;
            end
            if Jc==1
                % top/bottom-left boundary
                LocOutside = Outside(Ic+([dr 0]),Jc+([1 0]));
                [dI,dJ,del,cont] = handlecorner(LocOutside);
                dJ = -dJ;
            elseif Jc==NCols
                % top/bottom-right boundary
                LocOutside = Outside(Ic+([dr 0]),Jc+([-1 0]));
                [dI,dJ,del,cont] = handlecorner(LocOutside);
            else
                % strictly top/bottom boundary
                LocOutside = Outside(Ic+([dr 0]),Jc+(-1:1));
                [dI,dJ,del,cont] = handlewall(LocOutside);
            end
            if Ic==1
                dI = -dI;
            end
        else
            if Jc==1
                % left boundary
                LocOutside = Outside(Ic+(-1:1),Jc+([1 0]));
                [dJ,dI,del,cont] = handlewall(LocOutside');
                dJ = -dJ;
            elseif Jc==NCols
                % right boundary
                LocOutside = Outside(Ic+(-1:1),Jc+([-1 0]));
                [dJ,dI,del,cont] = handlewall(LocOutside');
            else
                LocOutside = Outside(Ic+(-1:1),Jc+(-1:1));
                switch sum(sum(LocOutside.*[1e0 1e1 1e2;1e7 0 1e3; 1e6 1e5 1e4]))
                    case 0
                        % simplest case: no relevant neighbour, so it can be
                        % deleted straightaway.
                        del = true;
                        cont = true;
                    case {1,10000011,11000011,10000111,11000111}
                        dI = -1;
                        dJ = -1;
                    case {10,11,110,111}
                        dI = -1;
                        dJ = 0;
                    case {100,1110,1111,11110,111111}
                        dI = -1;
                        dJ = 1;
                    case {1000,1100,11000,11100}
                        dI = 0;
                        dJ = 1;
                    case {10000,111000,111100,1111000,1111100}
                        dI = 1;
                        dJ = 1;
                    case {100000,110000,1100000,1110000}
                        dI = 1;
                        dJ = 0;
                    case {1000000,11100000,11110000,11100001,11110001}
                        dI = 1;
                        dJ = -1;
                    case {10000000,11000000,10000001,11000001}
                        dI = 0;
                        dJ = -1;
                    otherwise
                        % Wooow, complex case. Next one, please!
                        cont = true;
                end
            end
        end
        if cont
            ToBeDeleted(Ic,Jc) = del;
            continue
        end
        if iter==1 && abs(dI)+abs(dJ)~=1
            continue
        end
        xLoc = [x(Ic,Jc) x(Ic+dI,Jc+dJ)];
        yLoc = [y(Ic,Jc) y(Ic+dI,Jc+dJ)];
        [xI,yI] = int_lnln(xp,yp,xLoc,yLoc);
        xI = mean(xI);
        yI = mean(yI);
        dist = (xLoc-xI).^2+(yLoc-yI).^2;
        if dist(1)<=factor*(dist(1)+dist(2))
            % inside point closest to polygon: shift it outside
            x(Ic,Jc) = xI;
            y(Ic,Jc) = yI;
            OnPoly(Ic,Jc) = true;
            Inside(Ic,Jc) = false;
            %Outside(Ic,Jc) = false;
        else
            % outside point closest to polygon: shift it inside
            x(Ic+dI,Jc+dJ) = xI;
            y(Ic+dI,Jc+dJ) = yI;
            OnPoly(Ic+dI,Jc+dJ) = true;
            %Inside(Ic+dI,Jc+dJ) = false;
            Outside(Ic+dI,Jc+dJ) = false;
            %
            ToBeDeleted(Ic,Jc) = sum(LocOutside(:))==1;
        end
        PointShifted = true;
    end
end
x(ToBeDeleted) = NaN;
y(ToBeDeleted) = NaN;


function [dI,dJ,del,cont] = handlewall(LocOutside)
% strictly bottom boundary:
% LocOutside = [. . .
%               . o .
%               x x x]
% where . = point may be inside or outside polygon (check!)
%       o = point consider (inside polygon thus LocOutside=0)
%       x = point not defined
dI = 0;
dJ = 0;
del = false;
cont = false;
if sum(LocOutside(:)) == 0
    % simplest case: no relevant neighbour, so it can be
    % deleted straightaway.
    del = true;
    cont = true;
elseif LocOutside(2,1) && LocOutside(2,3)
    % Wooow, complex case with wall effect. Next one, please!
    cont = true;
elseif LocOutside(2,1)
    % wall effect
    dI = 0;
    dJ = -1;
elseif  LocOutside(2,3)
    % wall effect
    dI = 0;
    dJ = 1;
elseif LocOutside(1,2)
    dI = -1;
    dJ = 0;
elseif LocOutside(1,1)
    if LocOutside(1,3)
        % Wooow, complex case (no wall effect). Next one, please!
        cont = true;
    else
        dI = -1;
        dJ = -1;
    end
else %if LocOutside(1,3)
    dI = -1;
    dJ = 1;
end

function [dI,dJ,del,cont] = handlecorner(LocOutside)
% bottom-right boundary:
% LocOutside = [. . x
%               . o x
%               x x x]
% where . = point may be inside or outside polygon (check!)
%       o = point consider (inside polygon thus LocOutside=0)
%       x = point not defined
dI = 0;
dJ = 0;
del = false;
cont = false;
if sum(LocOutside(:)) == 0
    % simplest case: no relevant neighbour, so it can be
    % deleted straightaway.
    del = true;
    cont = true;
elseif LocOutside(2,1) && LocOutside(1,2)
    if LocOutside(3,1)
        dI = -1;
        dJ = -1;
    else
        % Wooow, complex case with wall effect. Next one, please!
        cont = true;
    end
elseif LocOutside(2,1)
    % wall effect
    dI = 0;
    dJ = -1;
elseif LocOutside(1,2)
    % wall effect
    dI = -1;
    dJ = 0;
else %LocOutside(1,1)
    dI = -1;
    dJ = -1;
end

function [x,y,xp,yp] = testcase
x0 = 1:100;
y0 = 1:100;
[x,y] = meshgrid(x0,y0);
xp = ...
    [ 23.3871   29.3779   42.0507   55.4147   64.6313   66.9355   64.4009   54.7235   44.3548 ...
    37.9032   29.6083   26.8433   34.2166   23.1567   23.1567 ];
yp = ...
    [61.2573   70.9064   76.4620   76.4620   72.0760   63.0117   56.2865   51.3158   40.2047 ...
    33.1871   33.7719   46.0526   56.2865   51.9006   51.9006];
