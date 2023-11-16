function Changed=limitresize(MinSize,MaxSize)
%LIMITRESIZE Constrained resize.
%
%   LIMITSIZE(MinimumSize,MaximumSize)
%   To be called from resize function to limit window size to
%   range between minimum and maximum size. MinimumSize and
%   MaximumSize should be 1x2 matrices representing the minimum
%   and maximum width/height of the figure. No checking on input
%   arguments performed.

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

%
% Should always be called from resize function callback, so use
% callback handle as figure handle.
%
Fig=gcbf;
Pos=get(Fig,'position');
PLoc=get(0,'pointerlocation');
OldPLoc=PLoc;
LRPWP='LimitResizePreviousWindowPosition';
LocChanged=0;
if isappdata(Fig,LRPWP)
    PrevPos=getappdata(Fig,LRPWP);
else
    PrevPos=[-1 -1 -1 -1];
end
if sum(cat(2,Pos(1:2)==PrevPos(1:2),Pos(1:2)+Pos(3:4)==PrevPos(1:2)+PrevPos(3:4)))>=2
    if ~isempty(MinSize)
        if Pos(3)<MinSize(1)
            if Pos(1)==PrevPos(1)
                PLoc(1)=Pos(1)+MinSize(1);
            else
                PMS=Pos(3)-MinSize(1);
                PLoc(1)=Pos(1)+PMS;
                Pos(1)=Pos(1)+PMS;
            end
            Pos(3)=MinSize(1);
        end
        if Pos(4)<MinSize(2)
            PMS=Pos(4)-MinSize(2);
            if Pos(2)==PrevPos(2)
                PLoc(2)=max(Pos(2)+MinSize(2)+20,PLoc(2)-PMS);
            else
                PLoc(2)=Pos(2)+PMS;
                Pos(2)=Pos(2)+PMS;
            end
            Pos(4)=MinSize(2);
        end
    end
    if ~isempty(MaxSize)
        if Pos(3)>MaxSize(1)
            if Pos(1)==PrevPos(1)
                PLoc(1)=Pos(1)+MaxSize(1);
            else
                PMS=Pos(3)-MaxSize(1);
                PLoc(1)=Pos(1)+PMS;
                Pos(1)=Pos(1)+PMS;
            end
            Pos(3)=MaxSize(1);
        end
        if Pos(4)>MaxSize(2)
            PMS=Pos(4)-MaxSize(2);
            if Pos(2)==PrevPos(2)
                PLoc(2)=PLoc(2)-PMS;
            else
                PMS=Pos(4)-MaxSize(2);
                PLoc(2)=Pos(2)+PMS;
                Pos(2)=Pos(2)+PMS;
            end
            Pos(4)=MaxSize(2);
        end
    end
    set(Fig,'position',Pos);
    if ~isequal(PLoc,OldPLoc)
        set(0,'pointerlocation',PLoc);
        LocChanged=1;
    end
end
setappdata(Fig,LRPWP,Pos)
if nargout==1
    Changed=LocChanged;
end
