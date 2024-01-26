function [ax,createops] = qp_createaxes(fig,cmd,varargin)
%QP_CREATEAXES Create an axes for plotting.

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

ax=[];
axname = 'New Plot';
createops = {};

switch cmd
    case 'relative'
        pos = varargin{1};
        unt = varargin{2};
        rel = varargin{3};
        nax = size(rel,1);
        for i = size(rel):-1:1
            subpos = [pos(1:2)+pos(3:4).*rel(i,1:2) pos(3:4).*rel(i,3:4)];
            ax(i)=axes('parent',fig, ...
                'units',unt, ...
                'position',subpos);
        end

    case 'oneplot'
        ax=Local_subplot(fig,1,1,1);
        set(ax,'tag',axname)

    case 'matrix'
        Correct=1;
        if nargin>2
            NR = varargin{1};
            NC = varargin{2};
            NP = varargin{3};
            inp='nonempty';
        else
            labels={'Number of Plots per Column','2'; ...
                'Number of Plots per Row','2'; ...
                'Plot Number(s)','1'};
            inp=stdinputdlg(labels(:,1),'Please Specify',1,labels(:,2));
            if ~isempty(inp)
                lasterr='';
                try
                    NR=str2vec(inp{1},'%d');
                    NC=str2vec(inp{2},'%d');
                    NP=str2vec(inp{3},'%d');
                catch
                    NR=1;
                    NC=1;
                    NP=1;
                    Correct=0;
                end
            end
        end
        Correct = Correct & isnumeric(NR) & isequal(size(NR),[1 1]) & ~isnan(NR) & ~isinf(NR) & (NR>0) & (NR==round(NR));
        Correct = Correct & isnumeric(NC) & isequal(size(NC),[1 1]) & ~isnan(NC) & ~isinf(NC) & (NC>0) & (NC==round(NC));
        Correct = Correct & isnumeric(NP) & isequal(size(NP,1),1) & ~any(isnan(NP(:))) & ~any(isinf(NP(:))) & all(NP(:)>0) & all(NP(:)==round(NP(:)));
        if Correct
            for i=length(NP(:)):-1:1;
                ax(i)=Local_subplot(fig,NR,NC,NP(i));
                axname_i=sprintf([axname ' (%d,%d,%d)'],NR,NC,NP(i));
                set(ax(i),'tag',axname_i);
            end
            createops = {NR,NC,NP};
        elseif ~isempty(inp)
            Str=lasterr;
            if isempty(Str)
                Str='Invalid numbers specified.';
            end
            ui_message('error',Str);
        end

    case {'specloc','Deltares Logo'}
        unit='normalized';
        if nargin>2
            Pos=varargin{1};
            if nargin>3
                unit=varargin{2};
            end
        else
            Pos=getnormpos(fig);
        end
        if ~isempty(Pos)
            ax=axes('parent',fig,'units',unit,'position',Pos);
            set(ax,'tag',axname)
            createops = {Pos,unit};
        end

    otherwise
        Str=sprintf('Requested axes type "%s" not yet implemented.',cmd);
        ui_message('warning',Str);
        return
end
qp_defaultaxessettings(ax)


function ax = Local_subplot(fig,nrows, ncols, thisPlot)
%LOCAL_SUBPLOT Create axes in tiled positions.
%   LOCAL_SUBPLOT(fig,m,n,p), breaks the Figure <fig> window into
%   an m-by-n matrix of small axes, selects the p-th axes for
%   for the current plot, and returns the axis handle.  The axes
%   are counted along the top row of the Figure window, then the
%   second row, etc.

% This is the percent offset from the subplot grid of the plotbox.
PERC_OFFSET_L = 2*0.09;
PERC_OFFSET_R = 2*0.045;
PERC_OFFSET_B = PERC_OFFSET_L;
PERC_OFFSET_T = PERC_OFFSET_R;
if nrows > 2
    PERC_OFFSET_T = 0.9*PERC_OFFSET_T;
    PERC_OFFSET_B = 0.9*PERC_OFFSET_B;
end
if ncols > 2
    PERC_OFFSET_L = 0.9*PERC_OFFSET_L;
    PERC_OFFSET_R = 0.9*PERC_OFFSET_R;
end

row = (nrows-1) -fix((thisPlot-1)/ncols);
col = rem (thisPlot-1, ncols);

% For this to work the default axes position must be in normalized coordinates
def_pos = [.13 .11 .775 .815];

col_offset = def_pos(3)*(PERC_OFFSET_L+PERC_OFFSET_R)/ ...
    (ncols-PERC_OFFSET_L-PERC_OFFSET_R);
row_offset = def_pos(4)*(PERC_OFFSET_B+PERC_OFFSET_T)/ ...
    (nrows-PERC_OFFSET_B-PERC_OFFSET_T);
totalwidth = def_pos(3) + col_offset;
totalheight = def_pos(4) + row_offset;
width = totalwidth/ncols*(max(col)-min(col)+1)-col_offset;
height = totalheight/nrows*(max(row)-min(row)+1)-row_offset;
position = [def_pos(1)+min(col)*totalwidth/ncols ...
    def_pos(2)+min(row)*totalheight/nrows ...
    width height];
if width <= 0.5*totalwidth/ncols
    position(1) = def_pos(1)+min(col)*(def_pos(3)/ncols);
    position(3) = 0.7*(def_pos(3)/ncols)*(max(col)-min(col)+1);
end
if height <= 0.5*totalheight/nrows
    position(2) = def_pos(2)+min(row)*(def_pos(4)/nrows);
    position(4) = 0.7*(def_pos(4)/nrows)*(max(row)-min(row)+1);
end

% create the axis:
if isappdata(fig,'MaximumPlotExtent')
    plotbox = getappdata(fig,'MaximumPlotExtent');
else
    plotbox = [0 0 1 1];
end
position = [plotbox(1:2) 0 0] + position.*plotbox([3 4 3 4]);
ax = axes('parent',fig,'units','normal','Position', position);
set(ax,'units',get(fig,'defaultaxesunits'))
