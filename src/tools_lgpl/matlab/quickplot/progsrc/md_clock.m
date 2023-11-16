function md_clock(Hclock,varargin)
%MD_CLOCK  Create a clock or calendar.
%    MD_CLOCK(AX,TYPE,TIME) creates a clock in the axes AX. Currently
%    supported clock types are
%         'analog clock'
%         'digital clock'
%         'calendar page'
%    The TIME can be specified as a MATLAB serial date number or as a time
%    vector. The default date/time is "now". Repeat the call to update the
%    clock. In subsequent calls the TYPE argument may be skipped
%    MD_CLOCK(AX,TIME).
%
%    Example
%       figure
%       AClock=subplot(3,2,[1 3]);
%       DClock=subplot(3,2,5);
%       Calendar=subplot(1,2,2);
%       md_clock(Calendar,'calendar page',now)
%       for i=1:60
%          md_clock(AClock,'analog clock',now)
%          md_clock(DClock,'digital clock',now)
%          pause(1)
%       end

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
   error('Too few input arguments: missing axes handle')
elseif ~ishandle(Hclock)
   error('First argument is not a valid axes handle')
end

B=findobj(Hclock,'tag','BackgroundPlateClock');
if isempty(B)
   if nargin<2
      error('Too few input arguments: missing clock type')
   else
      clocktype=varargin{1};
      otherargs=varargin(2:end);
   end
   if ~ischar(clocktype)
      error('Second argument is not a valid clock type string')
   else
      clocktype=lower(clocktype);
   end
   %
   [time,otherargs] = gettime(clocktype,otherargs);
   %
   if ~isempty(otherargs)
      error('More input arguments than expected.')
   end
   %
   switch clocktype
      case 'analog clock'
         B=create_analogclock(Hclock,otherargs);
      case 'digital clock'
         B=create_digitalclock(Hclock,otherargs);
      case 'calendar page'
         B=create_calendarpage(Hclock,otherargs);
      otherwise
         error('Unknown clock type: %s',clocktype)
   end
   set(B,'tag','BackgroundPlateClock')
   setappdata(B,'ClockType',lower(clocktype))
else
   clocktype=getappdata(B,'ClockType');
   otherargs=varargin;
   if nargin>=2 & isequal(lower(varargin{1}),clocktype)
      otherargs=varargin(2:end);
   end
   %
   [time,otherargs] = gettime(clocktype,otherargs);
   %
   if ~isempty(otherargs)
      error('More input arguments than expected or clock type mismatch.')
   end
end
%
switch clocktype
   case 'analog clock'
      update_analogclock(Hclock,B,time)
   case 'digital clock'
      update_digitalclock(Hclock,B,time)
   case 'calendar page'
      update_calendarpage(Hclock,B,time)
   otherwise
      error('Unknown clock type: %s',clocktype)
end


function [time,argo]=gettime(clocktype,argi)
ntim=length(argi);
for i=1:ntim
   if ischar(argi{i})
      ntim=i-1;
      break
   end
end
switch ntim
   case 0
      time = now;
   case 1
      time = argi{1};
      switch length(time)
         case 1 %datenum
         case 3 % [Y M D]  or  [h m s]
            switch clocktype
               case 'calendar page' % Y M D
                  time = datenum(time(1),time(2),time(3),time(12),time(0),time(0));
               otherwise % h m s
                  time = datenum(time(0),time(1),time(1),time(1),time(2),time(3));
            end
         case 6 % datevec: [Y M D h m s]
            time = datenum(time(1),time(2),time(3),time(4),time(5),time(6));
         otherwise
            error('Invalid input for date/time')
      end
   case 3 % Y M D   or   h m s
      switch clocktype
         case 'calendar page' % Y M D
            time = datenum(argi{:},12,0,0);
         otherwise % h m s
            time = datenum(0,1,1,argi{:});
      end
   case 6 % Y M D h m s
      time = datenum(argi{:});
   otherwise
      error('Cannot determine date/time specified, check syntax')
end
argo = argi(ntim+1:end);


%--------------------------------------------------------------------------
% ANALOG CLOCK
%--------------------------------------------------------------------------
function B=create_analogclock(Hclock,otherargs)
set(Hclock,'visible','off','dataaspectratio',[1 1 1],'xlim',[-1 1],'ylim',[-1 1]);

% background plate
X=sin(0:.1:(2*pi));
Y=cos(0:.1:(2*pi));
B=patch(X,Y,-ones(size(X)),'facecolor','w','parent',Hclock,'clipping','off');

% hour ticks
XH=sin((1:12)*(2*pi)/12);
YH=cos((1:12)*(2*pi)/12);
HT=line([.9*XH; .85*XH],[.9*YH;.85*YH],'color','k','parent',Hclock,'clipping','off');

%hands and indicators
SH=line(0.8*[0 0],0.8*[0 1], ...
   'color','r', ...
   'parent',Hclock, ...
   'clipping','off', ...
   'tag','SecondsHand');
MH=line(0.9*[0 0],0.9*[0 1], ...
   'color','k', ...
   'parent',Hclock, ...
   'clipping','off', ...
   'tag','MinutesHand');
HH=line(0.5*[0 0],0.5*[0 1], ...
   'color','k', ...
   'linewidth',1.5, ...
   'parent',Hclock, ...
   'clipping','off', ...
   'tag','HoursHand');
AM=text(0.5,0,'AM', ...
   'color','k', ...
   'fontunits','normalized', ...
   'fontsize',.1, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'parent',Hclock, ...
   'clipping','off', ...
   'tag','AM/PMIndicator');

ClockInfo.TimeOffset=0.0;
setappdata(B,'ClockInfo',ClockInfo)


function update_analogclock(Hclock,B,time)
SH=findobj(Hclock,'tag','SecondsHand');
MH=findobj(Hclock,'tag','MinutesHand');
HH=findobj(Hclock,'tag','HoursHand');
AM=findobj(Hclock,'tag','AM/PMIndicator');

ClockInfo=getappdata(B,'ClockInfo');
timeoffset=ClockInfo.TimeOffset;

time=time+timeoffset;
[year,month,day,hour,minute,second]=datevec(time);
set(SH,'xdata',0.8*[0 sin(second*2*pi/60)],'ydata',0.8*[0 cos(second*2*pi/60)])
set(MH,'xdata',0.9*[0 sin((minute+second/60)*2*pi/60)],'ydata',0.9*[0 cos((minute+second/60)*2*pi/60)])
set(HH,'xdata',0.5*[0 sin((hour+minute/60+second/3600)*2*pi/12)],'ydata',0.5*[0 cos((hour+minute/60+second/3600)*2*pi/12)])
if hour<12
   set(AM,'string','AM')
else
   set(AM,'string','PM')
end


%--------------------------------------------------------------------------
% DIGITAL CLOCK
%--------------------------------------------------------------------------
function B=create_digitalclock(Hclock,otherargs)
set(Hclock,'visible','off','dataaspectratio',[1 3 1],'xlim',[-1 1],'ylim',[-1 1]);

% background plate
B=patch([-1 1 1 -1],[-1 -1 1 1],-ones(1,4), ...
   'facecolor','k','parent',Hclock,'clipping','off');

fontsize=0.5;
%numbers and indicators
ST=text(0.6,0,'00', ...
   'color','r', ...
   'parent',Hclock, ...
   'fontunits','normalized', ...
   'fontsize',fontsize, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'clipping','off', ...
   'tag','SecondsText');
SEP1=text(0.3,0,':', ...
   'color','r', ...
   'parent',Hclock, ...
   'fontunits','normalized', ...
   'fontsize',fontsize, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'clipping','off', ...
   'tag','Separator 1');
MT=text(0,0,'00', ...
   'color','r', ...
   'fontunits','normalized', ...
   'fontsize',fontsize, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'parent',Hclock, ...
   'clipping','off', ...
   'tag','MinutesText');
SEP2=text(-0.3,0,':', ...
   'color','r', ...
   'parent',Hclock, ...
   'fontunits','normalized', ...
   'fontsize',fontsize, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'clipping','off', ...
   'tag','Separator 2');
HT=text(-0.6,0,'00', ...
   'color','r', ...
   'fontunits','normalized', ...
   'fontsize',fontsize, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'parent',Hclock, ...
   'clipping','off', ...
   'tag','HoursText');


function update_digitalclock(Hclock,B,time)
ST=findobj(Hclock,'tag','SecondsText');
MT=findobj(Hclock,'tag','MinutesText');
HT=findobj(Hclock,'tag','HoursText');
%SEP1=findobj(Hclock,'tag','Separator 1');
%SEP2=findobj(Hclock,'tag','Separator 2');

[year,month,day,hour,minute,second]=datevec(time);
second=floor(second);
set(ST,'string',sprintf('%2.2i',second));
set(MT,'string',sprintf('%2.2i',minute));
if hour<10
   set(HT,'string',sprintf('  %i',hour));
else
   set(HT,'string',sprintf('%2i',hour));
end


%--------------------------------------------------------------------------
% CALENDAR PAGE
%--------------------------------------------------------------------------
function B=create_calendarpage(Hclock,otherargs)
set(Hclock,'visible','off','dataaspectratio',[1 1 1],'xlim',[-1 1],'ylim',[-1 1]);

% background plate
B=patch([-1 -1 1 1],[-1 1 1 -1],-ones(1,4), ...
   'facecolor','w','parent',Hclock,'clipping','off');

%indicators
WD=text(0,.6,'WD', ...
   'color','k', ...
   'fontunits','normalized', ...
   'fontsize',0.1, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'tag','Weekday', ...
   'parent',Hclock, ...
   'clipping','off');
D=text(0,.2,'D', ...
   'color',[1 0 0], ...
   'fontunits','normalized', ...
   'fontsize',0.25, ...
   'fontweight','bold', ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'tag','Day', ...
   'parent',Hclock, ...
   'clipping','off');
M=text(0,-0.2,'M', ...
   'color','k', ...
   'fontunits','normalized', ...
   'fontsize',0.15, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'tag','Month', ...
   'parent',Hclock, ...
   'clipping','off');
Y=text(0,-.6,'Y', ...
   'color','k', ...
   'fontunits','normalized', ...
   'fontsize',0.15, ...
   'horizontalalignment','center', ...
   'verticalalignment','middle', ...
   'tag','Year', ...
   'parent',Hclock, ...
   'clipping','off');


function update_calendarpage(Hclock,B,time)
WD=findobj(Hclock,'tag','Weekday');
D =findobj(Hclock,'tag','Day');
M =findobj(Hclock,'tag','Month');
Y =findobj(Hclock,'tag','Year');

daystr={'Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'};
monthstr={'January','February','March','April','May','June','July','August','September','October','November','December'};

[year,month,day,hour,minute,second]=datevec(time);
set(WD,'string',daystr{weekday(time)})
set(D,'string',int2str(day))
month = max(month,1); % month returns 0 for 0 <= time < 1
set(M,'string',monthstr{month})
set(Y,'string',int2str(year))


