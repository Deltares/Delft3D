function DATA = qpcrosst(varargin)
%QPCROSST Cross-section of data: time-dependent track.
%   DATA = QPCROSST(FILE,DOMAINNR,FIELD,SUBFIELD,TB,XB,YB) reads the data
%   for the specified quantity FIELD from the specified FILE for the
%   DOMAINNR and SUBFIELD, and slice the data along the path (XB,YB) where
%   XB and YB are the x- and y-coordinates corresponding to the times
%   specified in the vector TB. DOMAINNR and SUBFIELD are required for some
%   files and quantities but can be skipped for others.
%
%   Example
%      times = qpread(FI,'velocity','times');
%      tMin = min(times);
%      tMax = max(times);
%      data2dv = qpcrosst(FI,'velocity',[tMin tMax],[0 35000],[15000 15000]);
%   
%   See also QPFILE, QPREAD, VSLICE, QPCROSS.

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

if nargin<5
   error('Not enough input arguments.')
end
tb = varargin{end-2};
xb = varargin{end-1};
yb = varargin{end};
if ~isequal(size(xb),size(yb)) | ~isnumeric(xb) | ~isnumeric(yb) | (~isequal(size(xb),[1 length(xb)]) & ~isequal(size(xb),[length(xb) 1]))
   error('Invalid or inconsistent data specified for coordinates (XB,YB) of line.')
end
xb = xb(:);
yb = yb(:);
tb = tb(:);
if ~isempty(tb) & (~isequal(size(tb),size(xb)) | ~isnumeric(tb))
   error('Dimension of time array TB doesn''t match dimension of coordinate arrays (XB,YB).')
end
quantspec = varargin(1:end-3);
%
%   DATA = QPCROSS(FILE,FIELD,TB,XB,YB)
%   DATA = QPCROSS(FILE,FIELD,SUBFIELD,TB,XB,YB)
%   DATA = QPCROSS(FILE,DOMAINNR,FIELD,SUBFIELD,TB,XB,YB)
%
FILE = quantspec{1};
if isnumeric(quantspec{2}) & isequal(size(quantspec{2}),[1 1])
   domain = quantspec{2};
   iFIELD = 3;
else
   domain = 0;
   iFIELD = 2;
end
FIELD = quantspec{iFIELD};
subfld = quantspec(iFIELD+1:end);
%
T = qpread(FILE,domain,FIELD,'times');
G = qpread(FILE,domain,FIELD,'grid',subfld{:},1);
%
Icross = arbcross(G.X,G.Y,xb,yb);
%
db = pathdistance(xb,yb);
d = pathdistance(Icross.x(:,1),Icross.y(:,1));
t = interp1(db,tb,d);
%
DATA1 = [];
first = 1;
for it = 1:length(T)-1
   within_dt = t>=T(it) & t<=T(it+1);
   if ~any(within_dt)
      continue
      DATA1 = [];
   end
   a2 = (t(within_dt)-T(it))/(T(it+1)-T(it));
   a1 = 1 - a2;
   if isempty(DATA1)
      DATA1 = qpread(FILE,domain,FIELD,'griddata',subfld{:},it);
      DATA1 = vslice(DATA1,'XY',Icross);
      if first
         comp = components(DATA1);
      end
   end
   DATA2 = qpread(FILE,domain,FIELD,'griddata',subfld{:},it+1);
   DATA2 = vslice(DATA2,'XY',Icross);
   %
   if first
      DATA = DATA1;
      for c = 1:length(comp)
         DATA = setfield(DATA,comp{c},repmat(NaN,size(getfield(DATA,comp{c}))));
      end
      first = 0;
   end
   for c = 1:length(comp)
      sz = size(DATA.X);
      for k = 1:prod(sz(2:end))
         data  = getfield(DATA,comp{c});
         data1 = getfield(DATA1,comp{c});
         data2 = getfield(DATA2,comp{c});
         data(within_dt,k) = a1.*data1(within_dt,k) + a2.*data2(within_dt,k);
         DATA = setfield(DATA,comp{c},data);
      end
   end
   %
   DATA1 = DATA2;
end
%
if first
   DATA = qpread(FILE,domain,FIELD,'griddata',subfld{:},1);
   DATA = vslice(DATA,'XY',Icross);
   comp = components(DATA);
   for c = 1:length(comp)
      DATA = setfield(DATA,comp{c},repmat(NaN,size(getfield(DATA,comp{c}))));
   end
end
%
DATA.Time = t;


function comp = components(D)
comp = {'Z' 'Val' 'XComp' 'YComp' 'ZComp'};
for c = length(comp):-1:1
   if ~isfield(D,comp{c})
      comp(c) = [];
   end
end
