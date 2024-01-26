function recolor(colit,FClr,TClr)
%RECOLOR Replaces one color by another color.
%   RECOLOR(Handle,FromColor,ToColor) replace the FromColor by the ToColor
%   for the graphics object represented by Handle and its child objects.
%
%   Example
%      L = plot(sin(0:.1:10));
%      recolor(gcf,get(L,'color'),[0.8 0 0.6])
%      recolor(gcf,get(gca,'xcolor'),[0.4 0 0.3])
%      recolor(gcf,get(gca,'color'),[1 0.8 0.95])
%      recolor(gcf,get(gcf,'color'),[0.9 0.5 0.8])

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

if ~ishandle(colit),
    error('Incorrect input argument.')
end
ColChar={'k' 'w' 'r' 'g' 'b' 'c' 'm' 'y'};
ColVec={[0 0 0],[1 1 1],[1 0 0],[0 1 0],[0 0 1],[0 1 1],[1 0 1],[1 1 0]};
if ischar(FClr)
    f=strmatch(lower(FClr),ColChar,'exact');
    if length(f)~=1
        error('Invalid character for FromColor')
    end
    FClr=ColVec{f};
end
if ischar(TClr)
    f=strmatch(lower(TClr),ColChar,'exact');
    if length(f)~=1
        error('Invalid character for ToColor')
    end
    TClr=ColVec{f};
end

colh=findall(colit); % find all children of selected objects
for i=1:length(colh)
    tp=get(colh(i),'type');
    switch tp
        case 'patch'
            Clrs={'edgecolor', 'facecolor', 'markeredgecolor', 'markerfacecolor'};
            for c=1:length(Clrs)
                Clr=get(colh(i),Clrs{c});
                if isequal(Clr,FClr)
                    set(colh(i),Clrs{c},TClr);
                end
            end
            % facevertexcdata

        case 'surface'
            Clrs={'edgecolor', 'facecolor', 'markeredgecolor', 'markerfacecolor'};
            for c=1:length(Clrs)
                Clr=get(colh(i),Clrs{c});
                if isequal(Clr,FClr)
                    set(colh(i),Clrs{c},TClr);
                end
            end
            % cdata

        case 'image'
            % cdata

        case 'light'
            Clr=get(colh(i),'color');
            if isequal(Clr,FClr)
                set(colh(i),'color',TClr);
            end

        case 'axes'
            Clrs={'color', 'xcolor', 'ycolor', 'zcolor', 'ambientlightcolor'};
            for c=1:length(Clrs)
                Clr=get(colh(i),Clrs{c});
                if isequal(Clr,FClr)
                    set(colh(i),Clrs{c},TClr);
                end
            end

        case 'line'
            Clrs={'color', 'markeredgecolor', 'markerfacecolor'};
            for c=1:length(Clrs)
                Clr=get(colh(i),Clrs{c});
                if isequal(Clr,FClr)
                    set(colh(i),Clrs{c},TClr);
                end
            end

        case 'figure'
            Clr=get(colh(i),'color');
            if isequal(Clr,FClr)
                set(colh(i),'color',TClr);
            end
            %colormap, dithermap

        case 'text'
            Clr=get(colh(i),'color');
            if isequal(Clr,FClr)
                set(colh(i),'color',TClr);
            end

        case 'uicontrol'
            Clrs={'foregroundcolor', 'backgroundcolor'};
            for c=1:length(Clrs)
                Clr=get(colh(i),Clrs{c});
                if isequal(Clr,FClr)
                    set(colh(i),Clrs{c},TClr);
                end
            end
    end
end
