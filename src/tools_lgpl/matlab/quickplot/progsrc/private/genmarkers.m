function hNew = genmarkers(hOld,Ops,Parent,Val,X,Y,Z)
%GENMARKERS Generic plot routine for marker plot.

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

zcoord=nargin>6;
hasvals=~isempty(Val);
if hasvals
    blank=isnan(Val(:));
else
    blank=0;
end
if zcoord
    blank=blank|isnan(X(:))|isnan(Y(:))|isnan(Z(:));
    Z=Z(~blank); Z=Z(:);
else
    blank=blank|isnan(X(:))|isnan(Y(:));
end
X=X(~blank); X=X(:);
Y=Y(~blank); Y=Y(:);
if hasvals
    Val=Val(~blank); Val=Val(:);
end
%
if zcoord
    xyz = cat(2,X,Y,Z);
else
    xyz = cat(2,X,Y);
end
if isempty(xyz)
    xyz=[];
end
FacesIndex=(1:length(Val))';
%
plot_using_patch=0;
if strcmp(Ops.markercolour,'auto')
    if hasvals
        plot_using_patch=1;
    else
        Ops.markercolour='b';
    end
end
if strcmp(Ops.markerfillcolour,'flat')
    if hasvals
        plot_using_patch=1;
    else
        Ops.markerfillcolour='none';
    end
end

if isfield(Ops,'Thresholds') && ~strcmp(Ops.Thresholds,'none')
    Thresholds = Ops.Thresholds;
else
    Thresholds = [];
end
%
if any(~ishandle(hOld)) || ~isempty(Thresholds)
    delete(hOld(ishandle(hOld)))
    hOld = [];
end

if plot_using_patch
    if ~isempty(hOld)
        hNew=hOld;
        set(hNew,'vertices',xyz, ...
            'facevertexcdata',Val, ...
            'faces',FacesIndex);
    elseif isempty(Thresholds)
        if isfield(Ops,'linewidth')
            % only specified for markers not equal to .
            lw = {'linewidth', Ops.linewidth};
        else
            % if marker equals . then linewidth isn't specified
            lw = {};
        end
        hNew=patch('vertices',xyz, ...
            'facevertexcdata',Val, ...
            'faces',FacesIndex, ...
            'parent',Parent, ...
            'edgecolor','flat', ...
            'facecolor','none', ...
            'linestyle','none', ...
            lw{:}, ...
            'marker',Ops.marker, ...
            'markersize',Ops.markersize, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
    else
        nThresholds = length(Thresholds);
        Thresholds(end+1) = inf;
        hNew=zeros(1,nThresholds);
        for i = 1:nThresholds
            iclass = Val>=Thresholds(i) & Val<Thresholds(i+1);
            FacesIndex=(1:sum(iclass))';
            if any(iclass)
                edgecolor = 'flat';
                markeredgecolor = Ops.markercolour;
                markerfacecolor = Ops.markerfillcolour;
                XYZ = xyz(iclass,:);
            else
                edgecolor = 'none';
                markeredgecolor = 'none';
                markerfacecolor = 'none';
                XYZ = [];
            end
            hNew(i)=patch('vertices',XYZ, ...
                'facevertexcdata',0*Val(iclass)+i, ...
                'faces',FacesIndex, ...
                'parent',Parent, ...
                'edgecolor',edgecolor, ...
                'facecolor','none', ...
                'linestyle','none', ...
                'marker',Ops.marker, ...
                'markersize',Ops.markersize, ...
                'markeredgecolor',markeredgecolor, ...
                'markerfacecolor',markerfacecolor);
        end
    end
else
    if isempty(xyz)
        xyz=zeros(0,2);
    end
    if ~isempty(hOld)
        hNew=hOld;
    else
        hNew=line('xdata',1, ...
            'ydata',1, ...
            'linestyle','none', ...
            'marker',Ops.marker, ...
            'markersize',Ops.markersize, ...
            'markeredgecolor',Ops.markercolour, ...
            'markerfacecolor',Ops.markerfillcolour);
    end
    set(hNew,'xdata',xyz(:,1), ...
        'ydata',xyz(:,2));
end
