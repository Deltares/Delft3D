function ecoplot(cmd)
%ECOPLOT EcoPlot: Case Analysis Tool for Delft3D-WAQ/ECO/SED data.
%   To start the interface type: ecoplot
%
%   See also D3D_QP.

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

mfig = findall(0,'tag','ECO-Plot');
if isempty(mfig)
   mfig = eco_init([pwd filesep]);
   if isempty(mfig)
      return
   end
   updatecases(mfig)
else
   figure(mfig);
end
if nargin==0 | isempty(cmd)
   return
end
switch cmd
   case 'closereq'
      Ini = getappdata(mfig,'Settings');
      inifile('write','ecoplot.ini',Ini)
      %
      delete(mfig);
      mfig = findall(0,'tag','Delft3D-QUICKPLOT');
      if isequal(get(mfig,'visible'),'off')
         d3d_qp('close');
      end
   case 'changepwd'
      Ini = getappdata(mfig,'Settings');
      inifile('write','ecoplot.ini',Ini)
      %
      pathui = findobj(mfig,'tag','PWD');
      cwd = get(pathui,'userdata');
      newdir = uigetdir(cwd,'Select New Work Directory');
      if ischar(newdir)
         cd(newdir)
         if ~isequal(newdir(end),filesep)
            newdir = cat(2,newdir,filesep);
         end
         set(findobj(mfig,'tag','PWD'),'string',newdir,'userdata',newdir)
      end
      updatecases(mfig)
   case 'measurements'
      set(findall(mfig,'tag','Measurements'),'value',get(gcbo,'value'))
   case 'plottype'
      plottype=get(gcbo,'value');
      UI_Items=get(gcbo,'userdata');
      for i=1:length(UI_Items)
         set(UI_Items{i},'visible','off')
      end
      set(UI_Items{plottype},'visible','on')
      updatecaselist(mfig)
   case {'hiscase','mapcase','limitcase','balancecase'}
      updatequantities(mfig)
      updatelocation(mfig)
      canplot(mfig)
   case {'hisquantity','mapquantity','balancequantity'}
      canplot(mfig)
   case {'hislocation','maplayer','limitlocation','balancelocation'}
      canplot(mfig)
   case 'plot'
      doplot(mfig)
   case 'NetTransport'
      Ini = getappdata(mfig,'Settings');
      uic = findobj(mfig,'tag','NetTransport');
      Ini = inifile('set',Ini,'Settings','NetTransport',get(uic,'value'));
      setappdata(mfig,'Settings',Ini)
   case 'ClrAutomatic'
      Ini = getappdata(mfig,'Settings');
      uic = findobj(mfig,'tag','ClrAutomatic');
      qnt = get(uic,'userdata');
      A = inifile('get',Ini,'ColourLimits',qnt,0);
      A(1) = get(uic,'value');
      Ini = inifile('set',Ini,'ColourLimits',qnt,A);
      setappdata(mfig,'Settings',Ini)
      canplot(mfig)
   case {'ClrMin','ClrMax'}
      Ini = getappdata(mfig,'Settings');
      uic = findobj(mfig,'tag','ClrAutomatic');
      qnt = get(uic,'userdata');
      A = inifile('get',Ini,'ColourLimits',qnt,0);
      %
      uicmin = findobj(mfig,'tag','ClrMin');
      q = str2num(get(uicmin,'string'));
      if ~isempty(q)
         A(2) = q(1);
      end
      %
      uicmax = findobj(mfig,'tag','ClrMax');
      q = str2num(get(uicmax,'string'));
      if ~isempty(q)
         A(3) = q(1);
      end
      %
      if A(2)>A(3)
         A(2:3) = fliplr(A(2:3));
      end
      set(uicmin,'string',A(2))
      set(uicmax,'string',A(3))
      %
      Ini = inifile('set',Ini,'ColourLimits',qnt,A);
      setappdata(mfig,'Settings',Ini)
   otherwise
      fprintf('%s not yet implemented.\n',cmd)
end


function updatecases(mfig)
cwd=get(findobj(mfig,'tag','PWD'),'userdata');
try
   Ini = inifile('open','ecoplot.ini');
catch
   Ini = inifile('new');
end
setappdata(mfig,'Settings',Ini)

casedir = inifile('get',Ini,'Cases','CaseSpec','grouped');
if isequal(casedir,'grouped')
   casedir = inifile('get',Ini,'Cases','CaseDir','waq');
   casebase = [cwd casedir filesep];
   cases=dir([casebase '*']);
   for i=1:length(cases)
      cases(i).dir = [casebase cases(i).name filesep];
   end
   %
   observdir = [cwd 'Observations' filesep];
   observ = dir([observdir '*.tim']);
   if isempty(observ)
      observ = '';
   else
      observ = [observdir observ(1).name];
   end
elseif isequal(casedir,'distributed')
   caselist = inifile('get',Ini,'Cases','Case','');
   if ~isempty(caselist)
      if ~iscell(caselist)
         caselist = {caselist};
      end
   end
   for i=1:length(caselist)
      if caselist{i}(2)==':'
         %absolute path
         cases(i).dir = caselist{i};
      else
         % relative path
         cases(i).dir = [cwd caselist{i}];
      end
      if cases(i).dir(end)~=filesep
         cases(i).dir(end+1)=filesep;
      end
      cases(i).name = caselist{i};
      cases(i).isdir = 1;
   end
   %
   observ = inifile('get',Ini,'Cases','Observations','');
else
   ui_message('error','Unknown CaseSpec ''%s'' in ''%s''',casedir,Ini.FileName)
   cases = [];
end
%
i=1;
while i<=length(cases)
   if ~cases(i).isdir | strcmp(cases(i).name,'.') | strcmp(cases(i).name,'..')
      cases(i)=[];
   else
      casedir = cases(i).dir;
      caseid = dir([casedir '*.inp']);
      if isempty(caseid)
         ui_message('error','Directory %s does not contain an *.inp file',cases(i).name)
         cases(i)=[];
      elseif length(caseid)>1
         ui_message('error','Directory %s contains multiple *.inp files',cases(i).name)
         cases(i)=[];
      else
         cases(i).id = [casedir caseid.name(1:end-4)];
         i=i+1;
      end
   end
end
%
% cases.name : directory name
%      .id   : case id
%      .hisfi: history file data
%      .mapfi: map file data
%      .balfi: balance file data
%
EMP = isempty(cases);
cases(1).hisfi = [];
cases(1).mapfi = [];
cases(1).balfi = [];
if EMP
   cases(1,:)=[];
end
%
try
   obs.FI = delwaqtimfile(observ);
   obs.Fields = qpread(obs.FI);
catch
   obs = [];
end
setappdata(mfig,'Observations',obs)
setappdata(mfig,'Cases',cases)
%
updatecaselist(mfig)


function updatecaselist(mfig)
cwd=get(findobj(mfig,'tag','PWD'),'userdata');
%
PlotType = findobj(mfig,'tag','PlotType');
plottypes = get(PlotType,'string');
ptype = get(PlotType,'value');
cases = getappdata(mfig,'Cases');
%
CaseList = findobj(mfig,'tag',[plottypes{ptype} 'Cases']);
%
switch plottypes{ptype}
   case {'History','Limiting Factors'}
      ext = '.his';
      type  = 'his';
   case 'Balance'
      ext = '-bal.his';
      type  = 'bal';
   case 'Map'
      ext = '.map';
      type = 'map';
end
%
hWB = [];
for i=1:length(cases)
   FI = getfield(cases(i),[type 'fi']);
   if isempty(FI)
      if ishandle(hWB)
         waitbar(i/length(cases),hWB,['Scanning case: ' cases(i).name]);
         figure(hWB)
      else
         hWB = waitbar(i/length(cases),'');
         set(findall(hWB,'type','text'),'interpreter','none')
         waitbar(i/length(cases),hWB,['Scanning case: ' cases(i).name]);
      end
      fname = [cases(i).id ext];
      %
      if exist(fname)
         okay = 1;
         gridfile = {};
         if isequal(plottypes{ptype},'Map')
            cco = dir([cases(i).dir '*.cco']);
            if isempty(cco)
               ui_message('error','Directory %s does not contain a *.cco file',cases(i).name)
               okay = 0;
            elseif length(cco)>1
               ui_message('error','Directory %s contains multiple *.cco files',cases(i).name)
               okay = 0;
            else
               gridfile = {[cases(i).dir cco(1).name]};
            end
         end
         if okay
            FI = qpfopen(fname,gridfile{:});
            cases = setfield(cases,{i},[type 'fi'],FI);
            QUANTS = qpread(FI);
            cases = setfield(cases,{i},[type 'quants'],QUANTS);
            if strcmp(type,'his')
               cases(i).limfac = strmatch('total chlorophyll in algae (limiting factors)',{QUANTS.Name},'exact');
               if isempty(cases(i).limfac)
                  cases(i).limfac = 0;
               end
            end
         else
            cases = setfield(cases,{i},[type 'fi'],-1);
         end
      else
         cases = setfield(cases,{i},[type 'fi'],-1);
         if strcmp(type,'his')
            cases(i).limfac = 0;
         end
      end
   end
end
if ishandle(hWB)
   delete(hWB)
end
%
observ = getappdata(mfig,'Observations');
msm = findobj(mfig,'tag','Measurements');
if isempty(observ)
   set(msm,'value',0,'enable','off')
else
   set(msm,'enable','on')
end
%
switch plottypes{ptype}
   case 'History'
      icase = find(cellfun('isclass',{cases.hisfi},'struct'));
      if isempty(icase)
         set(CaseList,'string','no cases found','value',[],'enable','off','backgroundcolor',get(mfig,'color'))
      else
         set(CaseList,'string',{cases(icase).name},'value',[],'userdata',icase,'enable','on','backgroundcolor','w')
      end
   case 'Map'
      icase = find(cellfun('isclass',{cases.mapfi},'struct'));
      if isempty(icase)
         set(CaseList,'string','no cases found','value',[],'enable','off','backgroundcolor',get(mfig,'color'))
      else
         set(CaseList,'string',{cases(icase).name},'value',[],'userdata',icase,'enable','on','backgroundcolor','w')
      end
   case 'Limiting Factors'
      icase = cellfun('isclass',{cases.hisfi},'struct');
      if isempty(icase)
         set(CaseList,'string','no cases found','value',1,'enable','off','backgroundcolor',get(mfig,'color'))
      else
         icase = find(icase & [cases.limfac]);
         set(CaseList,'string',{cases(icase).name},'value',1,'userdata',icase,'enable','on','backgroundcolor','w')
      end
   case 'Balance'
      icase = find(cellfun('isclass',{cases.balfi},'struct'));
      if isempty(icase)
         set(CaseList,'string','no cases found','value',1,'enable','off','backgroundcolor',get(mfig,'color'))
      else
         set(CaseList,'string',{cases(icase).name},'value',1,'userdata',icase,'enable','on','backgroundcolor','w')
      end
end
%
setappdata(mfig,'Cases',cases)
%
updatequantities(mfig)
updatelocation(mfig)
canplot(mfig)


function updatequantities(mfig)
cwd=get(findobj(mfig,'tag','PWD'),'userdata');
%
PlotButton = findobj(mfig,'tag','Plot');
PlotType = findobj(mfig,'tag','PlotType');
%
plottypes = get(PlotType,'string');
ptype = get(PlotType,'value');
%
cases = getappdata(mfig,'Cases');
%
switch plottypes{ptype}
   case {'History','Map','Balance'}
      switch plottypes{ptype}
         case 'History'
            plotstr='his';
            specplot=0;
         case 'Map'
            plotstr='map';
            specplot=0;
         case 'Balance'
            plotstr='bal';
            specplot=1;
      end
      %
      Field = [plotstr 'quants'];
      CaseList = findobj(mfig,'tag',[plottypes{ptype} 'Cases']);
      QuantList = findobj(mfig,'tag',[plottypes{ptype} 'Quantity']);
      %
      value = get(CaseList,'value');
      icase = get(CaseList,'userdata');
      %
      if isempty(icase)
         cases = [];
      else
         cases = cases(icase(value));
      end
      %
      if isempty(cases)
         noquant = 'no case selected';
         quants={};
         quantsSN = {};
      else
         noquant = 'no quantities in common';
         quantI = getfield(cases(1),Field);
         %
         % remove limiting factors, but keep balances
         %
         if specplot
            quantI=quantI([quantI.NVal]==-1);
         else
            quantI([quantI.NVal]==-1)=[];
         end
         %
         quants = {quantI.Name};
         for i=2:length(cases)
            quantI2 = getfield(cases(i),Field);
            imem = ismember(quants,{quantI2.Name});
            quantI = quantI(imem);
            quants = quants(imem);
         end
         %
         quantI = separators(quantI);
         quants = {quantI.Name};
         quantsSN = {quantI.ShortName};
      end
      %
      if isempty(quants)
         set(QuantList,'value',1,'string',noquant, ...
            'enable','off','backgroundcolor',get(mfig,'color'))
         set(PlotButton,'userdata',0)
      else
         iqnt = get(QuantList,'value');
         quants_prev = get(QuantList,'string');
         if iscell(quants_prev)
            qnt = strmatch(quants_prev{iqnt},quants);
            if isempty(qnt)
               qnt = 1;
            else
               qnt = qnt(1);
            end
         else
            qnt = 1;
         end
         set(QuantList,'value',1,'string',quants,'value',qnt, ...
            'enable','on','backgroundcolor','w','userdata',quantsSN)
         set(PlotButton,'userdata',1)
      end
   case 'Limiting Factors'
      set(PlotButton,'userdata',1)
end


function updatelocation(mfig)
cwd=get(findobj(mfig,'tag','PWD'),'userdata');
%
PlotButton = findobj(mfig,'tag','Plot');
PlotType = findobj(mfig,'tag','PlotType');
%
plottypes = get(PlotType,'string');
ptype = get(PlotType,'value');
%
cases = getappdata(mfig,'Cases');
%
switch plottypes{ptype}
   case {'History','Limiting Factors','Balance'}
      switch plottypes{ptype}
         case {'History','Limiting Factors'}
            plotstr='his';
         case 'Balance'
            plotstr='bal';
      end
      %
      Field = [plotstr 'quants'];
      CaseList = findobj(mfig,'tag',[plottypes{ptype} 'Cases']);
      LocList  = findobj(mfig,'tag',[plottypes{ptype} 'Location']);
      %
      value = get(CaseList,'value');
      icase = get(CaseList,'userdata');
      %
      if isempty(icase)
         cases = [];
      else
         cases = cases(icase(value));
      end
      %
      if isempty(cases)
         noloc = 'no case selected';
         locs={};
      else
         noloc = 'no locations in common';
         %
         FL = getfield(cases(1),[plotstr 'fi']);
         HQ = getfield(cases(1),Field);
         locs = sort(qpread(FL,HQ(1),'stations'));
         for i=2:length(cases)
            FL = getfield(cases(i),[plotstr 'fi']);
            HQ = getfield(cases(i),Field);
            locs2 = qpread(FL,HQ(1),'stations');
            locs = intersect(locs,locs2);
         end
      end
      %
      if isempty(locs)
         set(LocList,'value',1,'string',noloc, ...
            'enable','off','backgroundcolor',get(mfig,'color'))
         set(PlotButton,'userdata',0)
      else
         iloc = get(LocList,'value');
         locs_prev = get(LocList,'string');
         if iscell(locs_prev)
            loc = strmatch(locs_prev{iloc},locs);
            if isempty(loc)
               loc = 1;
            else
               loc = loc(1);
            end
         else
            loc = 1;
         end
         set(LocList,'value',1,'string',locs,'value',loc, ...
            'enable','on','backgroundcolor','w')
      end
   case 'Map'
      %
      % Nothing to do until we can select layer
      %
end


function canplot(mfig)
updateoptions(mfig)
cwd=get(findobj(mfig,'tag','PWD'),'userdata');
%
PlotButton = findobj(mfig,'tag','Plot');
PlotType = findobj(mfig,'tag','PlotType');
%
plottypes = get(PlotType,'string');
ptype = get(PlotType,'value');
%
cases = getappdata(mfig,'Cases');
%
if get(PlotButton,'userdata')==0
   set(PlotButton,'enable','off')
   return
end
%
switch plottypes{ptype}
   case {'History','Map','Balance'}
      switch plottypes{ptype}
         case 'History'
            plotstr='his';
         case 'Map'
            plotstr='map';
         case 'Balance'
            plotstr='bal';
      end
      %
      QuantList = findobj(mfig,'tag',[plottypes{ptype} 'Quantity']);
      %
      iqnt = get(QuantList,'value');
      quants = get(QuantList,'string');
      %
      if isempty(strmatch('---',quants(iqnt)))
         set(PlotButton,'enable','on')
      else
         set(PlotButton,'enable','off')
      end
   case 'Limiting Factors'
      set(PlotButton,'enable','on')
end


function updateoptions(mfig)
Ini = getappdata(mfig,'Settings');
%
PlotType = findobj(mfig,'tag','PlotType');
plottypes = get(PlotType,'string');
ptype = get(PlotType,'value');
%
switch plottypes{ptype}
   case 'History'
   case 'Map'
      cases = getappdata(mfig,'Cases');
      %
      CaseList = findobj(mfig,'tag',[plottypes{ptype} 'Cases']);
      QuantList = findobj(mfig,'tag',[plottypes{ptype} 'Quantity']);
      %LocList = findobj(mfig,'tag',[plottypes{ptype} 'Location']);
      %
      value = get(CaseList,'value');
      icase = get(CaseList,'userdata');
      cases = cases(icase(value));
      %
      qnt = '';
      if ~isempty(QuantList)
         iqnt = get(QuantList,'value');
         quants = get(QuantList,'string');
         if iscell(quants)
            qnt = quants{iqnt};
         end
      end
      %
      uiclim = findobj(mfig,'tag','ClrLimit');
      uicaut = findobj(mfig,'tag','ClrAutomatic');
      uicmin = findobj(mfig,'tag','ClrMin');
      uicmax = findobj(mfig,'tag','ClrMax');
      %
      NeedsColours = 0;
      if ~isempty(qnt) & isempty(strmatch('---',qnt))
         iqnt = strmatch(qnt,{cases(1).mapquants.Name},'exact');
         iqnt = iqnt(1);
         NeedsColours = cases(1).mapquants(iqnt).NVal == 1;
      end
      if ~NeedsColours
         set(uiclim,'enable','off')
         set(uicaut,'enable','off','value',0)
         set(uicmin,'enable','off','string',' ','backgroundcolor',get(mfig,'color'))
         set(uicmax,'enable','off','string',' ','backgroundcolor',get(mfig,'color'))
      else
         value = inifile('get',Ini,'ColourLimits',qnt,1);
         set(uiclim,'enable','on')
         set(uicaut,'enable','on','value',value(1),'userdata',qnt)
         if ~value(1)
            if length(value)<3
               value = [0 0 1];
            end
            set(uicmin,'enable','on','string',value(2),'backgroundcolor','w')
            set(uicmax,'enable','on','string',value(3),'backgroundcolor','w')
         else
            set(uicmin,'enable','off','string',' ','backgroundcolor',get(mfig,'color'))
            set(uicmax,'enable','off','string',' ','backgroundcolor',get(mfig,'color'))
         end
      end
   case 'Limiting Factors'
   case 'Balance'
      uic = findobj(mfig,'tag','NetTransport');
      value = inifile('get',Ini,'Settings','NetTransport',0);
      set(uic,'value',value)
end


function doplot(mfig)
Ini = getappdata(mfig,'Settings');
set(mfig,'pointer','watch')
drawnow
hWB=[];
try
   cwd=get(findobj(mfig,'tag','PWD'),'userdata');
   %
   PlotButton = findobj(mfig,'tag','Plot');
   PlotType = findobj(mfig,'tag','PlotType');
   %
   plottypes = get(PlotType,'string');
   ptype = get(PlotType,'value');
   %
   cases = getappdata(mfig,'Cases');
   %
   CaseList = findobj(mfig,'tag',[plottypes{ptype} 'Cases']);
   QuantList = findobj(mfig,'tag',[plottypes{ptype} 'Quantity']);
   LocList = findobj(mfig,'tag',[plottypes{ptype} 'Location']);
   %
   value = get(CaseList,'value');
   icase = get(CaseList,'userdata');
   cases = cases(icase(value));
   %
   qnt = [];
   if ~isempty(QuantList)
      iqnt = get(QuantList,'value');
      quants = get(QuantList,'string');
      quantsSN = get(QuantList,'userdata');
      qnt = quants{iqnt};
      qntSN = quantsSN{iqnt};
   end
   %
   loc = [];
   if ~isempty(LocList)
      iloc = get(LocList,'value');
      locs = get(LocList,'string');
      loc = locs{iloc};
   end
   %
   observ = getappdata(mfig,'Observations');
   msm = findobj(mfig,'tag','Measurements');
   if ~isstruct(observ)
      % observ = [];
   elseif ~get(msm(1),'value')
      observ = [];
   else
      iobserv = strmatch(qntSN,{observ.Fields.Name},'exact');
      if isempty(iobserv)
         observ = [];
      end
   end
   %
   Fig=qp_createfig('quick','');
   switch plottypes{ptype}
      case 'Limiting Factors'
         Ax=axes;
         Qnt = cases.hisquants(cases.limfac);
         %
         PS.FI=cases.hisfi;
         PS.Props=Qnt;
         locs2 = qpread(PS.FI,PS.Props,'stations');
         [locs_int,locs_i,locs2_i]=intersect(locs,locs2);
         %
         FI2 = embed(PS.FI,PS.Props);
         FI2.Data.Selected{2} = locs2_i;
         PS.FI = FI2;
         PS.Props = qpread(FI2);
         %
         PS.Domain=1;
         PS.SubField={};
         PS.Selected={0 iloc [] [] []};
         PS.Parent=Ax;
         PS.Handles=[];
         PS.Stations=locs;
         PS.Ops=qp_state_startup;
         PS.Ops.axestype='LimitingFactorsAxes';
         PS.Ops.colourbar='none';
         PS.Ops.colour='b';
         [hNew,Error,Info]=qp_plot(PS);
         %
         set(get(Ax,'title'),'string',[cases.name ': ' loc])
         qp_updatescroller(hNew,Fig)
      case 'Balance'
         Ax=axes;
         FI = cases.balfi;
         FI.nettransport = inifile('get',Ini,'Settings','NetTransport',0);
         %
         PS.FI=FI;
         iqnt = strmatch(qnt,{cases.balquants.Name},'exact');
         PS.Props=cases.balquants(iqnt);
         locs2 = qpread(PS.FI,PS.Props,'stations');
         [locs_int,locs_i,locs2_i]=intersect(locs,locs2);
         %
         FI2 = embed(PS.FI,PS.Props);
         FI2.Data.Selected{2} = locs2_i;
         PS.FI = FI2;
         PS.Props = qpread(FI2);
         %
         PS.Domain=1;
         PS.SubField={};
         PS.Selected={0 iloc [] [] []};
         PS.Parent=Ax;
         PS.Handles=[];
         PS.Stations=locs;
         PS.Ops=qp_state_startup;
         PS.Ops.axestype='Time-<blocking>';
         PS.Ops.colourbar='none';
         PS.Ops.colour='none';
         [hNew,Error,Info]=qp_plot(PS);
         %
         set(get(Ax,'title'),'string',[cases.name ': ' loc])
         qp_updatescroller(hNew,Fig)
      case 'History'
         Parent = axes;
         Clrs = get(Parent,'ColorOrder');
         iclr=0;
         hWB=[];
         i0 =1;
         if isstruct(observ)
            i0 = 0;
         end
         %
         for i=i0:length(cases)
            if i==0
               nm = 'Measurements';
            else
               nm = cases(i).name;
            end
            if ishandle(hWB)
               waitbar(i/length(cases),hWB,['Plotting case: ' nm]);
               figure(hWB)
            else
               hWB = waitbar(i/length(cases),'');
               set(findall(hWB,'type','text'),'interpreter','none')
               waitbar(i/length(cases),hWB,['Plotting case: ' nm]);
               set(hWB,'handlevis','off')
            end
            iclr=iclr+1;
            if iclr>size(Clrs,1)
               iclr=1;
            end
            %
            if i==0
               PS.FI=observ.FI;
               PS.Props=observ.Fields(iobserv);
               hCs{1}='Measurements';
               %
               locs2 = qpread(PS.FI,PS.Props,'stations');
               [locs_int,locs_i,locs2_i]=intersect(locs,locs2);
               ll = repmat(length(locs2)+1,size(locs));
               ll(locs_i) = locs2_i;
               %
               FI2 = embed(PS.FI,PS.Props);
               FI2.Data.Selected{2} = ll;
               PS.FI = FI2;
               PS.Props = qpread(FI2);
            else
               iqnt = strmatch(qnt,{cases(i).hisquants.Name},'exact');
               PS.FI=cases(i).hisfi;
               PS.Props=cases(i).hisquants(iqnt);
               hCs{i-i0+1}=protectstring(cases(i).name);
               %
               locs2 = qpread(cases(i).hisfi,PS.Props,'stations');
               [locs_int,locs_i,locs2_i]=intersect(locs,locs2);
               %
               FI2 = embed(PS.FI,PS.Props);
               FI2.Data.Selected{2} = locs2_i;
               PS.FI = FI2;
               PS.Props = qpread(FI2);
            end
            PS.Domain=1;
            PS.SubField={};
            PS.Selected={0 iloc [] [] []};
            PS.Parent=Parent;
            PS.Handles=[];
            PS.Stations=locs;
            PS.Ops=qp_state_startup;
            PS.Ops.axestype='Time-Val';
            PS.Ops.plotcoordinate = 'time';
            PS.Ops.colourbar='none';
            PS.Ops.colour=Clrs(iclr,:);
            PS.Ops.clippingvalues=realset('-999');
            if i==0
               PS.Ops.linestyle = 'none';
               PS.Ops.marker = 'o';
               PS.Ops.markerfillcolour = 'none';
            end
            [hNew(i-i0+1),Error,Info]=qp_plot(PS);
         end
         legend(hNew,hCs)
         qp_updatescroller(hNew,Fig)
      case 'Map'
         n = floor(sqrt(length(cases)));
         m = ceil(length(cases)/n);
         hWB=[];
         hNewAll = [];
         for i=1:length(cases)
            if ishandle(hWB)
               waitbar(i/length(cases),hWB,['Plotting case: ' cases(i).name]);
               figure(hWB)
            else
               hWB = waitbar(i/length(cases),'');
               set(findall(hWB,'type','text'),'interpreter','none')
               waitbar(i/length(cases),hWB,['Plotting case: ' cases(i).name]);
               set(hWB,'handlevis','off')
            end
            Parent = subplot(n,m,i);
            %
            iqnt = strmatch(qnt,{cases(i).mapquants.Name},'exact');
            Sz=qpread(cases(i).mapfi,cases(i).mapquants(iqnt),'size');
            PS.FI=cases(i).mapfi;
            PS.Domain=1;
            PS.Props=cases(i).mapquants(iqnt);
            PS.SubField={};
            PS.Selected={Sz(1) [] 0 0 []};
            PS.Parent=Parent;
            PS.Handles=[];
            PS.Stations='';
            PS.Ops=qp_state_startup;
            PS.Ops.axestype='X-Y';
            if PS.Props.NVal == 0
               PS.Ops.colourbar='none';
            else
               CLim = inifile('get',Ini,'ColourLimits',qnt,1);
               if CLim(1)==0
                  % given limits
                  if length(CLim)<3
                     PS.Ops.colourlimits = [0 1];
                  else
                     PS.Ops.colourlimits = CLim(2:3);
                  end
               end
               PS.Ops.presentationtype='patches';
               PS.Ops.thresholds='none';
            end
            PS.Ops.clippingvalues=realset('-999');
            [hNew,Error,Info]=qp_plot(PS);
            hNewAll = cat(1,hNewAll,hNew);
            tt = get(Parent,'title');
            tstr = get(tt,'string');
            %
            if isstruct(observ) & PS.Props.NVal>0
               PS.FI=observ.FI;
               PS.FI.Times = qpread(cases(i).mapfi,cases(i).mapquants(iqnt),'times');
               DTmax = inifile('get',Ini,'Settings','MeasurementDt',max(diff(PS.FI.Times)));
               PS.FI.DTmax = DTmax;
               PS.Domain=1;
               PS.Props=observ.Fields(iobserv);
               PS.SubField={};
               Sz=qpread(PS.FI,PS.Props,'size');
               PS.Selected={Sz(1) 0 [] [] []};
               PS.Parent=Parent;
               PS.Handles=[];
               PS.Stations='';
               PS.Ops=qp_state_startup;
               PS.Ops.axestype='X-Y';
               PS.Ops.presentationtype='markers';
               PS.Ops.marker='o';
               PS.Ops.markerfillcolour='flat';
               PS.Ops.thresholds='none';
               PS.Ops.clippingvalues=realset('-999');
               [hNew,Error,Info]=qp_plot(PS);
               hNewAll = cat(1,hNewAll,hNew);
            end
            %
            if iscell(tstr) && ~isempty(tstr)
               tstr{1} = [protectstring(cases(i).name) ': ' tstr{1}];
               set(tt,'string',tstr)
            end
            %set(get(Parent,'title'),'interpreter','none')
         end
         qp_updatescroller(hNewAll,Fig)
      otherwise
         cases.id
         qnt
         loc
         msgbox('plot not yet implemented.','Error','error','modal')
   end
catch
end
if ishandle(hWB)
   delete(hWB)
end
set(mfig,'pointer','arrow')


function Fig = eco_init(PWD)
d3d_qp('initialize_background')
if isempty(findall(0,'tag','Delft3D-QUICKPLOT'))
   %
   % QUICKPLOT did not initialize correctly, so quit
   %
   Fig=[];
   return
end

width=300;
height=360;
labelwidth=75;
sz = qp_getscreen;
pos = [sz(1)+sz(3)-width-50 sz(2)+sz(4)-height-50 width height];
vers=d3d_qp('version');
Fig = qp_uifigure(['ECO-Plot ' vers],'closereq','ECO-Plot',pos,'ecoplot');

voffset = height-30;
uicontrol('parent',Fig, ...
   'position',[width-100 voffset 90 20], ...
   'style','pushbutton', ...
   'callback','ecoplot changepwd', ...
   'string','Change...')
uicontrol('parent',Fig, ...
   'position',[11 voffset-2 width-121 18], ...
   'style','text', ...
   'string','Current Work Directory', ...
   'horizontalalignment','left')

voffset = voffset-20;
uicontrol('parent',Fig, ...
   'position',[11 voffset width-21 20], ...
   'enable','inactive', ...
   'tag','PWD', ...
   'style','edit', ...
   'string',PWD, ...
   'horizontalalignment','left', ...
   'userdata',PWD)


voffset = voffset-20-25;
uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Plot Type', ...
   'horizontalalignment','left')
PT=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','PlotType', ...
   'style','popupmenu', ...
   'string',{'History','Map','Limiting Factors','Balance'}, ...
   'backgroundcolor','w', ...
   'callback','ecoplot plottype', ...
   'horizontalalignment','left');

voffsetref = voffset-25;
UI_Items={};
%====================================================
% HISTORY
H=[];
voffset = voffsetref-20;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Case(s)', ...
   'horizontalalignment','left');
voffset = voffsetref-100;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 100], ...
   'tag','HistoryCases', ...
   'style','listbox', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'max',2, ...
   'callback','ecoplot hiscase', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'style','checkbox', ...
   'enable','off', ...
   'tag','Measurements', ...
   'string','Include Measurements', ...
   'callback','ecoplot measurements', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Quantity', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','HistoryQuantity', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot hisquantity', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Location', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','HistoryLocation', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot hislocation', ...
   'horizontalalignment','left');
%set(H,'visible','off')
UI_Items{end+1}=H;
%----------------------------------------------------
%MAP
H=[];
voffset = voffsetref-20;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Case(s)', ...
   'horizontalalignment','left');
voffset = voffsetref-100;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 100], ...
   'tag','MapCases', ...
   'style','listbox', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'max',2, ...
   'callback','ecoplot mapcase', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'style','checkbox', ...
   'enable','off', ...
   'tag','Measurements', ...
   'string','Include Measurements', ...
   'callback','ecoplot measurements', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Quantity', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','MapQuantity', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot mapquantity', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Layer', ...
   'enable','off', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','MapLocation', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot maplayer', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Colour Limits', ...
   'tag','ClrLimit', ...
   'enable','off', ...
   'horizontalalignment','left');
w = (width-labelwidth-21-20)/3;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset w 20], ...
   'tag','ClrAutomatic', ...
   'style','checkbox', ...
   'string','Auto', ...
   'value',1, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot ClrAutomatic', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[21+labelwidth+w voffset w 20], ...
   'tag','ClrMin', ...
   'style','edit', ...
   'string',' ', ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot ClrMin', ...
   'tooltip','lower limit colour scale', ...
   'horizontalalignment','right');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[31+labelwidth+2*w voffset w 20], ...
   'tag','ClrMax', ...
   'style','edit', ...
   'string',' ', ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot ClrMax', ...
   'tooltip','upper limit colour scale', ...
   'horizontalalignment','right');
set(H,'visible','off')
UI_Items{end+1}=H;
%----------------------------------------------------
%LIMITING FACTORS
H=[];
voffset = voffsetref-20;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Case', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','Limiting FactorsCases', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot limitcase', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Location', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','Limiting FactorsLocation', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot limitlocation', ...
   'horizontalalignment','left');
set(H,'visible','off')
UI_Items{end+1}=H;
%----------------------------------------------------
%BALANCE
H=[];
voffset = voffsetref-20;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Case', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','BalanceCases', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot balancecase', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Quantity', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','BalanceQuantity', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot balancequantity', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11 voffset-2 labelwidth 18], ...
   'style','text', ...
   'string','Location', ...
   'horizontalalignment','left');
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','BalanceLocation', ...
   'style','popupmenu', ...
   'string',{' '}, ...
   'enable','off', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot balancelocation', ...
   'horizontalalignment','left');

voffset = voffset-25;
H(end+1)=uicontrol('parent',Fig, ...
   'position',[11+labelwidth voffset width-labelwidth-21 20], ...
   'tag','NetTransport', ...
   'style','checkbox', ...
   'string','Net Transport', ...
   'value',0, ...
   'enable','on', ...
   'backgroundcolor',get(Fig,'color'), ...
   'callback','ecoplot NetTransport', ...
   'horizontalalignment','left');
set(H,'visible','off')
UI_Items{end+1}=H;
%====================================================
set(PT,'userdata',UI_Items)

uicontrol('parent',Fig, ...
   'position',[width-100 10 90 20], ...
   'style','pushbutton', ...
   'tag','Plot', ...
   'enable','off', ...
   'callback','ecoplot plot', ...
   'string','Plot')

set(Fig,'visible','on')


function A=protectstring(A)
A = strrep(A,'\','\\');
A = strrep(A,'_','\_');
A = strrep(A,'^','\^');


function FI2 = embed(FI,Props)
FI2.QPF = 1;
FI2.Name = '<user defined variables>';
FI2.Data = Props;
FI2.Data.FileInfo = FI;
FI2.Data.Domain = 1;
FI2.Data.Props = Props;
FI2.Data.Selected = {0 1 [] [] []};
FI2.Data.SubField = {};
FI2.Data.Tri = 0;
FI2.FileType = '<user defined variables>';
FI2.Options = 1;
FI2.Otherargs = [];
