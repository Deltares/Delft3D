function qp_update_options(OH,UD,Ops)
%QP_UPDATE_OPTIONS Update QuickPlot user interface for plot options.

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

set(OH,'enable','off','visible','off')

if isempty(Ops)
    return
end

%--------------------------------------------------------------------------
Choices = [];
Choices = makechoices(Choices,Ops,'plotcoordinate');
Choices = makechoices(Choices,Ops,'vectorcomponent');
Choices = makechoices(Choices,Ops,'presentationtype');
Choices = makechoices(Choices,Ops,'vectorcolour');
Choices = makechoices(Choices,Ops,'angleconvention');
Choices = makechoices(Choices,Ops,'vectorstyle');
Choices = makechoices(Choices,Ops,'vectorscalingmode');
Choices = makechoices(Choices,Ops,'verticalscalingmode');
Choices = makechoices(Choices,Ops,'horizontalalignment');
Choices = makechoices(Choices,Ops,'verticalalignment');
Choices = makechoices(Choices,Ops,'thinningmode');
Choices = makechoices(Choices,Ops,'linestyle');
Choices = makechoices(Choices,Ops,'marker');
Choices = makechoices(Choices,Ops,'thresholddistribution');
Choices = makechoices(Choices,Ops,'colourmap');
%--------------------------------------------------------------------------

Active=UD.Active;

if isfield(Ops,'axestimezone_shift') && ~isnan(Ops.axestimezone_shift)
    set(findobj(OH,'tag','axestimezone'),'enable','on');
    atz=findobj(OH,'tag','axestimezone=?');
    set(atz,'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'plotcoordinate')
    set(findobj(OH,'tag','plotcoordinate'),'enable','on');
    pd=findobj(OH,'tag','plotcoordinate=?');
    set(pd,'value',1,imatch(Choices,Ops,'plotcoordinate'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'vectorcomponent')
    set(findobj(OH,'tag','component'),'enable','on');
    compon=findobj(OH,'tag','component=?');
    set(compon,'value',1,imatch(Choices,Ops,'vectorcomponent'),'enable','on','backgroundcolor',Active)
end

%    switch Ops.vectorcomponent
%        case {'vector','patch centred vector','vector (split x,y)','vector (split m,n)'}
%            Ops.presentationtype=Ops.vectorcomponent;
%        case 'edge'
%            Ops.presentationtype=Ops.vectorcomponent;
%    end

if isfield(Ops,'presentationtype')
    set(findobj(OH,'tag','presenttype'),'enable','on')
    pt=findobj(OH,'tag','presenttype=?');
    set(pt,'value',1,imatch(Choices,Ops,'presentationtype'),'enable','on','backgroundcolor',Active)
%    switch Ops.presentationtype
%        case 'vector'
%            Ops.vectorcomponent='edge';
%    end
end

if isfield(Ops,'vectorcolour')
    colvect=findobj(OH,'tag','colourvectors');
    set(colvect,'enable','on')
    colvecm=findobj(OH,'tag','vectorcolour=?');
    set(colvecm,'value',1,imatch(Choices,Ops,'vectorcolour'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'units')
    set(findobj(OH,'tag','dataunits'),'enable','on')
    dunit=findobj(OH,'tag','dataunits=?');
    if strcmp(Ops.units,'**Hide**')
        ival = 8;
    else
        ival = 7;
        dunitset=findobj(OH,'tag','dataunits=!');
        set(dunitset,'enable','on','backgroundcolor',Active,'string',Ops.units)
    end
    set(dunit,'enable','on','backgroundcolor',Active,'value',ival)
end

if isfield(Ops,'angleconvention')
    pd=findobj(OH,'tag','angleconvention=?');
    set(pd,'value',1,imatch(Choices,Ops,'angleconvention'),'enable','on','backgroundcolor',Active)
    set(findobj(OH,'tag','angleconvention'),'enable','on');
end

if isfield(Ops,'colourdams')
    coldams=findobj(OH,'tag','colourdams');
    set(coldams,'enable','on','value',Ops.colourdams)
end

if isfield(Ops,'operator')
    oper=findobj(OH,'tag','operator');
    set(oper,'enable','on')
    oper=findobj(OH,'tag','operator=?');
    set(oper,'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'unicolour')
    cun=findobj(OH,'tag','unicolour');
    set(cun,'enable','on','value',Ops.unicolour)
end

if isfield(Ops,'vectorstyle')
    set(findobj(OH,'tag','vectorstyle'),'enable','on')
    vstyle=findobj(OH,'tag','vectorstyle=?');
    set(vstyle,imatch(Choices,Ops,'vectorstyle'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'vectorscalingmode')
    set(findobj(OH,'tag','vecscalem'),'enable','on')
    vsmode=findobj(OH,'tag','vecscalem=?');
    set(vsmode,imatch(Choices,Ops,'vectorscalingmode'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'vectorscale')
    set(findobj(OH,'tag','1vecunit'),'enable','on')
    oneunitis=findobj(OH,'tag','1vecunit=?');
    set(oneunitis,'string',num2str(Ops.vectorscale),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'verticalscalingmode')
    set(findobj(OH,'tag','vertscalem'),'enable','on')
    vsm=findobj(OH,'tag','vertscalem=?');
    set(vsm,imatch(Choices,Ops,'verticalscalingmode'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'verticalscalefactor')
    set(findobj(OH,'tag','vscale'),'enable','on')
    enl=findobj(OH,'tag','vscale=?');
    set(enl,'string',num2str(Ops.verticalscalefactor),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'extend2edge')
    h=findobj(OH,'tag','extend2edge');
    set(h,'enable','on','value',Ops.extend2edge)
end

if isfield(Ops,'numformat')
    set(findobj(OH,'tag','numformat'),'enable','on');
    numform=findobj(OH,'tag','numformat=?');
    set(numform,'enable','on','backgroundcolor',Active,'string',Ops.numformat)
end

if isfield(Ops,'fontsize')
    set(findobj(OH,'tag','fontsize'),'enable','on');
    hFontsize=findobj(OH,'tag','fontsize=?');
    set(hFontsize,'enable','on','backgroundcolor',Active,'string',num2str(Ops.fontsize))
end

if isfield(Ops,'horizontalalignment')
    set(findobj(OH,'tag','alignment'),'enable','on');
    set(findobj(OH,'tag','horizontalalignment'),'enable','on');
    set(findobj(OH,'tag','verticalalignment'),'enable','on');
    hHorAlign=findobj(OH,'tag','horizontalalignment=?');
    set(hHorAlign,imatch(Choices,Ops,'horizontalalignment'),'enable','on','backgroundcolor',Active)
    %
    hVerAlign=findobj(OH,'tag','verticalalignment=?');
    set(hVerAlign,imatch(Choices,Ops,'verticalalignment'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'thinningmode')
    set(findobj(OH,'tag','thinfld'),'enable','on');
    thinfld=findobj(OH,'tag','thinfld=?');
    set(thinfld,imatch(Choices,Ops,'thinningmode'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'thinningfactors')
    set(findobj(OH,'tag','thinfact'),'enable','on');
    thinfact=findobj(OH,'tag','thinfact=?');
    set(thinfact,'string',num2str(Ops.thinningfactors),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'thinningdistance')
    set(findobj(OH,'tag','thindist'),'enable','on');
    thindist=findobj(OH,'tag','thindist=?');
    set(thindist,'string',num2str(Ops.thinningdistance),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'colour')
    set(findobj(OH,'tag','colour'),'enable','on')
    clrh=findobj(OH,'tag','colour=?');
    set(clrh,'enable','on','backgroundcolor',Ops.colour)
end

if isfield(Ops,'facecolour')
    if ~ischar(Ops.facecolour)
        clrh=findobj(OH,'tag','facecolour=?');
        set(clrh,'enable','on','backgroundcolor',Ops.facecolour)
    elseif strcmp(Ops.facecolour,'yes')
        fpoly=findobj(OH,'tag','fillpolygons');
        set(fpoly,'enable','on','value',1)
    end
end

if isfield(Ops,'fontsize')
    if isfield(Ops,'textboxfacecolour') && ~isequal(Ops.textboxfacecolour,'none')
        set(findobj(OH,'tag','textbox=?'),'enable','on','value',1)
        hTextbox=findobj(OH,'tag','textboxfacecolour=?');
        set(hTextbox,'enable','on','backgroundcolor',Ops.textboxfacecolour)
    else
        set(findobj(OH,'tag','textbox=?'),'enable','on','value',0)
    end
end

if isfield(Ops,'linestyle')
    set(findobj(OH,'tag','linestyle'),'enable','on')
    lns=findobj(OH,'tag','linestyle=?');
    set(lns,imatch(Choices,Ops,'linestyle'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'linewidth')
    set(findobj(OH,'tag','linewidth'),'enable','on')
    lnw=findobj(OH,'tag','linewidth=?');
    set(lnw,'enable','on','backgroundcolor',Active,'string',num2str(Ops.linewidth))
end

if isfield(Ops,'marker')
    set(findobj(OH,'tag','marker'),'enable','on')
    mrk=findobj(OH,'tag','marker=?');
    set(mrk,imatch(Choices,Ops,'marker'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'markersize') && ~isequal(Ops.marker,'none')
    set(findobj(OH,'tag','markersize'),'enable','on')
    mrk=findobj(OH,'tag','markersize=?');
    set(mrk,'enable','on','backgroundcolor',Active,'string',num2str(Ops.markersize))
end

if isfield(Ops,'markercolour') && ~ischar(Ops.markercolour)
    mc=findobj(OH,'tag','usemarkercolour');
    set(mc,'enable','on')
    mc=findobj(OH,'tag','markercolour=?');
    set(mc,'enable','on','backgroundcolor',Ops.markercolour)
end

if isfield(Ops,'markerfillcolour') && ~ischar(Ops.markerfillcolour)
    mc=findobj(OH,'tag','usemarkerfillcolour');
    set(mc,'enable','on','value',1)
    %
    mc=findobj(OH,'tag','markerfillcolour=?');
    set(mc,'enable','on','backgroundcolor',Ops.markerfillcolour)
end

if isfield(Ops,'presentationtype') && isfield(Ops,'thresholds') && ...
        ismember(Ops.presentationtype,{'vector','patches','patches with lines','markers'})
    cclass=findobj(OH,'tag','colclassify');
    set(cclass,'enable','on','value',1)
end

if isfield(Ops,'thresholds')
    set(findobj(OH,'tag','thresholds'),'enable','on')
    c = Ops.thresholds;
    set(findobj(OH,'tag','thresholds=?'),'enable','on','backgroundcolor',Active,'string',vec2str(c,'noones','nobrackets'),'userdata',c)
end

if isfield(Ops,'thresholddistribution')
    thrd=findobj(OH,'tag','threshdistr=?');
    set(thrd,imatch(Choices,Ops,'thresholddistribution'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'colourlimits')
    set(findobj(OH,'tag','climmode'),'enable','on')
    climmode=findobj(OH,'tag','climmode=?');
    if isequal(size(Ops.colourlimits),[1 2])
        set(climmode,'enable','on','backgroundcolor',Active,'value',2)
        set(findobj(OH,'tag','climmax'),'enable','on')
        set(findobj(OH,'tag','climmax=?'),'enable','on','backgroundcolor',Active,'string',Ops.colourlimits(2))
        set(findobj(OH,'tag','climmin'),'enable','on')
        set(findobj(OH,'tag','climmin=?'),'enable','on','backgroundcolor',Active,'string',Ops.colourlimits(1))
        Min=get(findobj(OH,'tag','climmin=?'),'userdata');
        Max=get(findobj(OH,'tag','climmax=?'),'userdata');
    else
        set(climmode,'enable','on','backgroundcolor',Active,'value',1)
    end
end

if isfield(Ops,'symmetriccolourlimits')
    climsymm=findobj(OH,'tag','climsymm');
    set(climsymm,'enable','on','value',Ops.symmetriccolourlimits)
end

if isfield(Ops,'colourmap')
    set(findobj(OH,'tag','colourmap'),'enable','on')
    set(findobj(OH,'tag','colourmapbutton'),'enable','on')
    cmap=findobj(OH,'tag','colourmap=?');
    set(cmap,imatch(Choices,Ops,'colourmap'),'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'colourbar')
    cbar=findobj(OH,'tag','colourbar');
    set(cbar,'enable','on')
    cbarh=findobj(OH,'tag','colbarhorz');
    set(cbarh,'enable','on')
end

if isfield(Ops,'axestype')
    % do something
end

if isfield(Ops,'clippingvalues')
    set(findobj(OH,'tag','clippingvals'),'enable','on')
    c = Ops.clippingvalues;
    set(findobj(OH,'tag','clippingvals=?'),'enable','on','backgroundcolor',Active,'string',clip2str(c),'userdata',c)
end

if isfield(Ops,'clipnans')
    set(findobj(OH,'tag','clipnans'),'enable','on','value',Ops.clipnans)
end

if isfield(Ops,'xclipping')
    set(findobj(OH,'tag','clippingvals'),'enable','on')
    c = Ops.xclipping;
    set(findobj(OH,'tag','xclipping'),'enable','on')
    set(findobj(OH,'tag','xclipping=?'),'enable','on','backgroundcolor',Active,'string',clip2str(c),'userdata',c)
    c = Ops.yclipping;
    set(findobj(OH,'tag','yclipping'),'enable','on')
    set(findobj(OH,'tag','yclipping=?'),'enable','on','backgroundcolor',Active,'string',clip2str(c),'userdata',c)
end

set(findall(OH,'enable','on'),'enable','inactive')


function Str = clip2str(c)
if isstruct(c)
    Str=realset(c);
else
    Str=vec2str(c,'noones','nobrackets');
end


function settings = imatch(Choices,Ops,fld)
settings.string = Choices.(fld);
if ~ischar(Ops.(fld))
    settings.value = 1;
else
    settings.value = ustrcmpi(Ops.(fld),settings.string);
end


function Choices = makechoices(Choices,Ops,fld)
OTHER = {'other option 1','other option 2','other option 3'};
if isfield(Ops,fld)
    Choices.(fld) = [{Ops.(fld)} OTHER];
    if ~ischar(Choices.(fld){1})
        Choices.(fld){1} = '<non-string object>';
    end
end
