function hNew=plotlimitingfactors(FI,varargin)
%PLOTLIMITINGFACTORS  Create a limiting factors plot.
%   This function creates a limiting factors plot for Chlorophyll in
%   algae.
%   PLOTLIMITINGFACTORS(FileInfo,AX,Location)
%   where FileInfo is a structure obtained from a QPFOPEN call, AX
%   specifies the axes in which the limiting factors should be plot, and
%   Location specified the station (either specified by name or number)
%   for which the plot should be made.
%
%   See also QPFOPEN.

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

T_=1;
ST_=2;
if nargin<3
   error('Not enough input arguments.')
elseif nargin>3
   [options,err]=procargs(varargin, ...
      {'Name'  ,'HasDefault','Default','List'}, ...
      {'parent'  ,0  ,[]     ,[]
      'location',0  ,[]     ,[]
      'color'   ,1  ,'none' ,[]             });
end
Location=options.location;
Parent=options.parent;

try
   Quants = qpread(FI);
catch
   error('Error reading data. Check syntax.')
end
QuantNames = {Quants.Name};
Chlfa = strmatch('total chlorophyll in algae',QuantNames,'exact');
if isempty(Chlfa)
   Chlfa = strmatch('Limit Chlo',QuantNames,'exact');
end
if isempty(Chlfa)
   error('Cannot find total Chlorophyll in algae (Limit Chlo) in data file.')
end

if Quants(Chlfa).DimFlag(T_)
    Times = Location{1};
    Location = Location(2:end);
else
    Times = 0;
end
if Quants(Chlfa).DimFlag(ST_)
   if iscell(Location)
      Location = Location{1};
   end
   Loc=qpread(FI,Quants(Chlfa),'stations');
   if ischar(Location)
      LocationStr=Location;
      Location = strmatch(LocationStr,Loc);
   else
      LocationStr=Loc{Location};
   end
   Location = {Location};
else
   LocationStr=sprintf('(%i,%i)',Location{:});
end

DataChlfa = qpread(FI,Quants(Chlfa),'data',Times,Location{:});
H=line(DataChlfa.Time,DataChlfa.Val,'parent',Parent,'color',options.color);
if nargout>0
   hNew=H;
end
tick(Parent,'x','autodate')
set(get(Parent,'title'),'string',LocationStr,'interpreter','none')
set(get(Parent,'ylabel'),'string',['total chlorophyll in algae (',Quants(Chlfa).Units,') \rightarrow'])

if ~isappdata(Parent,'LimitingFactorsAxes')
   units = get(Parent,'units');
   pos = get(Parent,'position');
   newpos = pos+0.2*pos(4)*[0 1 0 -1];
   set(Parent,'position',newpos,'xticklabel','','box','on')
   newpos = pos; newpos(4)=newpos(4)*0.2;
   Ax2=axes('units',units,'position',newpos,'xlim',get(Parent,'xlim'));

   Fig=get(Parent,'parent');
   Obj=allchild(Fig);
   inew = find(Obj==Ax2);
   Obj(inew)=[];
   ipar = find(Obj==Parent);
   Obj = cat(1,Obj(1:ipar),Ax2,Obj(ipar+1:end));
   set(Fig,'children',Obj)
   setappdata(Parent,'LimitingFactorsAxes',Ax2)
   setappdata(Ax2,'LimitingFactorsAxes',Parent)
else
   set(Parent,'xticklabel','')
   Ax2=getappdata(Parent,'LimitingFactorsAxes');
   delete(allchild(Ax2))
end
limits = {
   'limiting factor mortality'  'Limit mor'
   'limiting factor growth'     'Limit gro'
   'limiting factor silicate'   'Limit sil'
   'limiting factor phosphorus' 'Limit pho'
   'limiting factor nitrogen'   'Limit nit'
   'limiting factor energy'     'Limit e'};
limits_used = {};
offset=0;
NTim = length(DataChlfa.Time);
ExpandVal = [2:NTim;2:NTim];
ExpandTime = [1:NTim-1;2:NTim;2:NTim;1:NTim-1];
ExpandedTime = DataChlfa.Time(ExpandTime);
for i=1:size(limits,1)
   limiti=strmatch(limits{i,1},QuantNames,'exact');
   if isempty(limiti)
      limiti=strmatch(limits{i,2},QuantNames,'exact');
   end
   if ~isempty(limiti)
      limits_used{end+1} = limits{i,1}(17:end);
      offset=offset+1;
      DataLimit = qpread(FI,Quants(limiti),'data',0,Location{:});
      DataLimit.Val(DataLimit.Val==0)=NaN;
      DataLimit.Val(~isnan(DataLimit.Val))=offset;
      %line(DataLimit.Time,DataLimit.Val,'marker','*','linestyle','none','parent',Ax2);
      patch(ExpandedTime,cat(1,DataLimit.Val(ExpandVal)-0.25,DataLimit.Val(ExpandVal)+0.25),i,'edgecolor','none','parent',Ax2);
   end
end
set(Ax2,'ylim',[0 offset+1],'ytick',1:offset,'yticklabel',limits_used,'box','on')
tick(Ax2,'x','autodate')
setappdata(Ax2,'YLim',[0 offset+1])
set(get(Ax2,'xlabel'),'string','time \rightarrow')
