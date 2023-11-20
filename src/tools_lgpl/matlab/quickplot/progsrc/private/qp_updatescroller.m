function qp_updatescroller(hNew,pfig)
%QP_UPDATESCROLLER Update list of items/dimensions that can be animated.

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

DimMenu = {'subfield' 'time' 'station' 'M' 'N' 'K'};
DimStr={'subfield','time step','station','M','N','K'};

UDs = get(hNew,'userdata');
if ~iscell(UDs)
    UDs = {UDs};
end
UDs(cellfun('isempty',UDs)) = [];
for i = 1:length(UDs)
    UD = UDs{i};
    ObjTag = get(hNew(i),'tag');
    
    Info = UD.PlotState.FI;
    DomainNr = UD.PlotState.Domain;
    Props = UD.PlotState.Props;
    subf = UD.PlotState.SubField;
    if isempty(subf)
        subf={[]};
    end
    
    [Chk,subfs]=qp_getdata(Info,DomainNr,Props,'subfields');
    [Chk,szTSMNK]=qp_getdata(Info,DomainNr,Props,'size');
    if ~Chk
        continue
    end
    %
    sz = [length(subfs) szTSMNK];
    selected = [subf UD.PlotState.Selected];
    DimFlag = [1 Props.DimFlag];
    Values  = cell(size(sz));
    Values{1} = subfs;
    if any(Props.DimFlag(3:end)==7)
       [Chk,Values(2:end)]=qp_getdata(Info,DomainNr,Props,'dimlabels');
    end
    for m_ = 2:length(sz)
        if isempty(Values{m_})
            Values{m_} = 1:sz(m_);
        end
    end
    %
    CanAnim = zeros(size(sz));
    for m_ = 1:length(sz)
        if DimFlag(m_) && (sz(m_)>1) && length(selected{m_})==1 && ~isequal(selected{m_},0)
            CanAnim(m_)=1;
        end
    end
    
    if any(CanAnim) && ishandle(pfig)
        animslid=findobj(pfig,'tag','animslid');
        if isempty(animslid)
            qp_figurebars(pfig)
            qp_createscroller(pfig)
            animslid=findobj(pfig,'tag','animslid');
        end
        animpush=findobj(pfig,'tag','animpush');
        set(animpush,'enable','on')
        uicm=findobj(pfig,'tag','animpushuicontextmenu');
        hAnimOptChecked=findall(get(uicm,'children'),'checked','on');
        if i==1
            AS=[];
            set(hAnimOptChecked,'checked','off')
        else
            AS=get(animslid,'userdata');
        end
        it=uimenu('label',Props.Name,'parent',uicm,'userdata',ObjTag);
        AnimSel=[];
        hAnimSel=[];
        %
        if isfield(Props,'DimName')
            DimMenu(3:length(Props.DimName)+1) = Props.DimName(2:end);
            DimStr(3:length(Props.DimName)+1)  = Props.DimName(2:end);
        end
        %
        if CanAnim(2)
            animdim = 2;
        else
            animdim = min(find(CanAnim));
        end
        for m_ = 1:length(sz)
            if CanAnim(m_)
                Anim.Dim=m_-1; %! CONVERT from 1:6 back to AnimLoc (0=subf, 1=time, ...]
                Anim.Label=DimStr{m_};
                Anim.Values=Values{m_};
                hMenu=uimenu('label',DimMenu{m_},'parent',it,'userdata',Anim,'callback','d3d_qp animselect');
                if m_ == animdim
                    AnimSel=Anim;
                    hAnimSel=hMenu;
                end
            end
        end
        %
        NAnimValues = length(AnimSel.Values);
        sstep=[min(1/(NAnimValues-1),0.1) min(10/(NAnimValues-1),0.9)];
        AS(end+1).Fld=AnimSel.Dim;
        AS(end).Label=AnimSel.Label;
        AS(end).Values=AnimSel.Values;
        AS(end).Tag=ObjTag;
        if length(AS)>1
            if ~isequal(AS(end).Fld,AS(end-1).Fld)
                AS = AS(end);
                set(hAnimOptChecked,'checked','off')
            end
        end
        t_=AnimSel.Dim+1; %! CONVERT from AnimOpt (0=subf, 1=time, ...] back to 1:6
        t=selected{t_};
        if iscellstr(AS.Values)
            Str=AS.Values{t};
        else
            Str=sprintf('%i',t);
            t=find(AS.Values==t);
        end
        %set(animslid,'userdata',AS,'value',1,'sliderstep',sstep,'Max',NAnimValues,'enable','on','value',t)`
        uislider(animslid,'value',t,'Max',NAnimValues)
        set(animslid,'userdata',AS,'enable','on')
        set(hAnimSel,'checked','on')
        set(animslid,'tooltip',sprintf('%s(%i)=%s',DimStr{t_},t,Str));
    end
end
