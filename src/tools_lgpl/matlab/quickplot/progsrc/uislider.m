function uislider ( cmd, varargin )
%UISLIDER Create high resolution integer slider
%   The MATLAB uicontrol('style','slider') is based on a JAVA slider that
%   can store discrete values 0 to 1000000. MATLAB converts this into
%   floating point numbers in the min/max range you specify. However, the
%   limit of at most 1000001 discrete values remains. The UISLIDER function
%   wraps the function in a smart way to allow for larger value ranges.
%
%   UISLIDER(H,'PropertyName1',PropertyValue1,'PropertyName2',PropertyValue2,...)
%   converts uicontrol H into a slider and sets the specified properties.
%   The same call can also be used at any time to modify one or more of the
%   property values. The supported properties are:
%    * min      : minimum value of scroller (integer)
%    * max      : maximum value of scroller (integer)
%    * value    : initial value of scroller (integer)
%    * callback : string or function handle to be evaluated upon value change
%   The callback can access the current and previous values of the slider
%   by using the function calls:
%      PVAL = GETAPPDATA(GCBO,'PreviousValue');
%      CVAL = GETAPPDATA(GCBO,'CurrentValue');
%
%   UISLIDER without arguments opens a demo dialog.

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

if nargin==0
    f=figure('name','Slider Test');
    s=uicontrol('style','slider','parent',f,'position',[10 10 200 20]);
    uislider(s,'min',1,'max',40,'value',2,'callback',@democallback)
else
    h = cmd;
    if ~isscalar(h)
        for i=1:length(h)
            uislider(h(i),varargin{:})
        end
        return
    elseif ~ishandle(h) || ~strcmp(get(h,'type'),'uicontrol')
        error('Invalid handle provided for slider')
    end
    ival = [];
    if isempty(getappdata(h,'uislider'))
        minival = 0;
        maxival = 1000000;
        fcn     = '';
    else
        minival = getappdata(h,'minival');
        maxival = getappdata(h,'maxival');
        fcn     = getappdata(h,'slidercallback');
    end
    %
    for i = 1:2:length(varargin)
        switch lower(varargin{i})
            case 'min'
                minival = varargin{i+1};
            case 'max'
                maxival = varargin{i+1};
            case 'value'
                ival    = varargin{i+1};
            case 'callback'
                fcn     = varargin{i+1};
        end
    end
    %
    if isempty(ival)
        if isempty(getappdata(h,'uislider'))
            ival = minival;
        else
            ival = getappdata(h,'currentvalue');
            ival = min(max(minival,ival),maxival);
        end
    end
    if minival~=round(minival)
        error('Minimum value (%f) should be integer',minival)
    elseif maxival~=round(maxival)
        error('Maximum value (%f) should be integer',maxival)
    elseif ival~=round(ival)
        error('Value (%f) should be integer',ival)
    elseif minival>ival || ival>maxival
        error('Value (%i) should be in range [%i %i]',ival,minival,maxival)
    end
    %
    rval = relval(minival,ival,maxival);
    [smallstep,bigstep] = stepsizes;
    set(h,'style','slider','min',0,'max',1,'value',rval,'sliderstep',[smallstep bigstep],'callback',@callback);
    setappdata(h,'uislider',1)
    setappdata(h,'minival',minival)
    setappdata(h,'maxival',maxival)
    setappdata(h,'last_setvalue',get(h,'value'))
    setappdata(h,'previousvalue',ival)
    setappdata(h,'currentvalue',ival)
    setappdata(h,'slidercallback',fcn)
end

function democallback
h = gcbo;
fprintf('Slider change: from %d to %d\n',getappdata(h,'previousvalue'),getappdata(h,'currentvalue'))

function [smallstep,bigstep,eps] = stepsizes
smallstep = 1/1000000;
bigstep = 1/10000;
eps = smallstep/2;

function callback(h,varargin)
minival = getappdata(h,'minival');
maxival = getappdata(h,'maxival');
[smallstep,bigstep,eps] = stepsizes;
%
pv = getappdata(h,'last_setvalue');
cv = get(h,'value');
dv = cv-pv;
if dv<0
    dr = 'left';
    dv = abs(dv);
elseif dv>0
    dr = 'right';
else
    dr = '';
end
if dv>=bigstep-eps && dv<=bigstep+eps
    tp = ['jump_' dr];
elseif dv>=smallstep-eps && dv<=smallstep+eps
    tp = ['step_' dr];
else
    tp = 'slide';
end
%
ival = getappdata(h,'currentvalue');
switch tp
    case 'step_left'
        newival = max(ival-1,minival);
    case 'step_right'
        newival = min(ival+1,maxival);
    case 'jump_left'
        newival = max(ival-10,minival);
    case 'jump_right';
        newival = min(ival+10,maxival);
    otherwise
        newival = round(minival+cv*(maxival-minival));
        if isequal(newival,ival)
            switch dr
                case 'left'
                    newival = max(ival-1,minival);
                case 'right'
                    newival = min(ival+1,maxival);
            end
        end
end
if ~isequal(newival,ival)
    setappdata(h,'currentvalue',newival)
    setappdata(h,'previousvalue',ival)
    rval = relval(minival,newival,maxival);
    set(h,'value',rval)
    setappdata(h,'last_setvalue',get(h,'value'))
    fcn = getappdata(h,'slidercallback');
    if ~isempty(fcn)
        if isa(fcn,'function_handle')
            feval(fcn);
        else
            eval(fcn);
        end
    end
end

function vl = relval(mi,vl,ma)
vl = min(max(0,(vl-mi)/(ma-mi)),1);