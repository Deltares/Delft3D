function classbar(ax,Th,varargin)
%CLASSBAR Converts a color bar into a classbar.
%   CLASSBAR(ColorbarHandle,Thresholds,NumberFormat) converts the specified
%   colorbar into a class bar using the specified number format (default
%   %1.4g).
%
%   CLASSBAR(ColorbarHandle,Thresholds,CellString) converts the specified
%   colorbar into a class bar with the labels specified in the cell string.
%
%   ...,'labelcolor')
%   labels the colors instead of the transitions. The colors should be
%   labeled in case of a CONTOUR plot, the transitions should be labeled in
%   case of a CONTOURF plot.
%
%   ...,'plotall')
%   makes sure that all classes are drawn irrespective of the classes used
%   in the plot. This includes the last class >= maximum threshold.
%
%   ...,'plot',N)
%   makes sure that the first N classes are drawn irrespective of the
%   classes used in the plot. If N>=length(Thresholds) this implies
%   'plotall'.
%
%   ...,'plotrange',[N1 N2])
%   plots only the classes in the range N1:N2.
%
%   ...,'label',ThresholdsVal)
%   uses the value in the ThresholdsVal vector to display along the class
%   bar. NaN values will not be labelled.
%
%   ...,'format',FormatString)
%   uses the specified format to label the classbar, for instance '%3.2f'.
%
%   ...,'max')
%   use this option if cdata of contour patches contain maximum instead of
%   minimum values.
%
%   ...,'climmode',ClimMode)
%   uses the specified mode for setting the clim of the axes. By default
%   the ClimMode equals 'extend', which extends the current clim to include
%   the new threshold range. Alternative option for ClimMode is: 'new'
%   which causes the clim to match the threshold limits.
%
%   Example 1
%       thr=.25:.1:.95;
%       contourf(rand(10),thr)
%       axis square
%       classbar(colorbar('horz'),thr)
%
%   Example 2
%       r=rand(10)-0.5;
%       thr=[min(r(:)) -0.1 0.1];
%       contourf(r,thr)
%       h=colorbar;
%       classbar(h,thr,{'negative','approx.zero','positive'},'labelcolor')
%
%   See also CONTOURF, COLORBAR.

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

if nargin<2
    error('Not enough input arguments.')
end

Format='%1.4g';
PlotAll=0;
PlotRange=[];
ColorByIndex=0;
sTh=[];
LabelCol=0;
MaxValues=0;
ClimMode='extend';
if nargin>2
    i=0;
    while i<length(varargin)
        i=i+1;
        tmp=varargin{i};
        if ischar(tmp)
            switch lower(tmp)
                case 'plotall'
                    PlotAll=inf;
                case 'labelcolor'
                    LabelCol=1;
                case 'colorbyindex'
                    ColorByIndex=1;
                case 'climmode'
                    i=i+1;
                    ClimMode=lower(varargin{i});
                case 'plot'
                    i=i+1;
                    PlotAll=varargin{i};
                case 'plotrange'
                    i=i+1;
                    PlotRange=varargin{i};
                case 'label'
                    i=i+1;
                    sTh=varargin{i};
                case 'format'
                    i=i+1;
                    Format=varargin{i};
                case 'max'
                    MaxValues=1;
                otherwise
                    Format=tmp;
            end
        else
            Format=tmp;
        end
    end
end

I=findobj(ax,'type','image');
try
    OrigAx=double(get(ax,'axes'));
catch
    UD=get(ax,'userdata');
    OrigAx=UD.PlotHandle;
end
Patches=findall(OrigAx,'type','patch');

CLim=get(OrigAx,'clim');
if ~ColorByIndex && ~strcmp(ClimMode,'new')
    CLimTh = CLim;
    if Th(1)<CLimTh(1)
        CLimTh(1)=Th(1);
    end
    if Th(end)>CLimTh(2)
        CLimTh(2)=Th(end);
    end
    set(ax,'clim',CLimTh);
    if ~isequal(CLim,CLimTh)
        set(OrigAx,'clim',CLimTh)
    end
end

Th=Th(:);
if isempty(sTh)
    sTh=Th;
end
NTh=length(Th);
N=NTh;

% The color value of the patches containing the minimum value of the data set
% does not equal the largest threshold value smaller than the minimum value,
% but it is equal to the minimum value. This results in the patches having a
% color that is not part of the classified color palette and more importantly
% it is not consistent between figures and data sets. Therefore, the following
% lines correct this by replacing all patch color values by the largest thres-
% hold values smaller than or equal to the current patch color value.
%   thr=[-100  10:10:60];
%   contourf(magic(8),thr)
%   classbar(colorbar,thr)

for i=1:length(Patches)
    cp=get(Patches(i),'cdata');
    if isequal(size(cp),[1 1])
        CVal = max(find(Th<=cp));
        if ~ColorByIndex
            CVal = Th(CVal);
        end
        set(Patches(i),'cdata',CVal);
    end
end

horbar=isempty(get(ax,'ytick')); % if no ytick, horz. bar

clim=limits(OrigAx,'clim');
if horbar % if no ytick, horz. bar
    if ColorByIndex
        cdat=transpose(1:NTh);
    else
        cdat=transpose(Th); % cdat(1) should be replaced by min(cdata)
    end
    xx='x';
    yy='y';
else
    if ColorByIndex
        cdat=1:NTh;
    else
        cdat=Th;
    end
    xx='y';
    yy='x';
end
set(I,'cdatamapping','scaled','cdata',cdat,[xx 'data'],[1 NTh]+0.5,'visible','off');
if ~iscell(Format)
    tckL=cell(1,length(Th));
    if length(sTh)==NTh-1
        for i=1:NTh
            if i==1
                tckL{i}=sprintf(['<' Format],sTh(i));
            elseif i==NTh
                tckL{i}=sprintf(['>' Format],sTh(i-1));
            else
                tckL{i}=sprintf([Format '-' Format],sTh(i-1:i));
            end
            if LabelCol
                tckL{i}=[' ' tckL{i}];
            end
        end
    else
        for i=1:NTh
            if isnan(sTh(i)) || isequal(sTh(i),-inf)
                tckL{i}='';
            else
                tckL{i}=sprintf(Format,sTh(i));
                if LabelCol
                    tckL{i}=[' ' tckL{i}];
                end
            end
        end
    end
    set(ax,[xx 'tick'],(1:NTh),[xx 'ticklabel'],tckL);
else
    set(ax,[xx 'ticklabel'],Format,[xx 'tick'],(1:NTh))
end
if LabelCol
    set(ax,[xx 'tick'],(1:NTh)+.5,'ticklength',[0 0])
end
YLim=get(ax,[yy 'lim']);

F=1;
if ~isempty(PlotRange)
    F=min(PlotRange);
    N=max(PlotRange);
elseif ~PlotAll
    if ColorByIndex
        N=min(find((1:NTh)>clim(2)))-1;
        F=max(find((1:NTh)<clim(1)))+1;
    else
        N=min(find(Th>clim(2)))-1;
        F=max(find(Th<clim(1)))+1;
    end
    if isempty(N) % max > threshold of last class
        N=length(Th);
    end
    if isempty(F) % min < threshold of first class
        F=1;
    end
else
    N=min(N,PlotAll);
end
if F==N+1
    F=N;
end
set(ax,[xx 'lim'],[F N+1]-MaxValues);
I=findobj(ax,'tag','classbounds');
if ~isempty(I)
    delete(I)
end
XP=[(1:N)+1; (1:N)+1; (1:N); (1:N)]-MaxValues;
YP=[repmat(YLim(1),[1 N]);repmat(YLim(2),[1 N])
    repmat(YLim(2),[1 N]);repmat(YLim(1),[1 N])];
if ~horbar
    TMP=YP;
    YP=XP;
    XP=TMP;
end
if ColorByIndex
    cdat = transpose(1:N);
else
    cdat = transpose(Th(1:N));
end
patch(XP,YP,1,'cdata',cdat,'parent',ax,'tag','classbounds')
set(OrigAx,'clim',get(ax,'clim'))
