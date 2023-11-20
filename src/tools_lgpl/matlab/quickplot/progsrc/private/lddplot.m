function h=lddplot(PCR,AxesHandle)
%LDDPLOT Plot local drainage direction for PC-Raster LDD data file.
%   LDDPLOT(PCR,AxesHandle)
%   where PCR is a PC-Raster LDD data file structure, and
%   AxesHandle is an optional axes handle to be plotted in.

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

if ~strcmp(PCR.PCRType,'ldd')
    error('PC Raster LDD file required.')
end
if isequal(PCR.YDir,'from bottom to top')
    sgny=-1;
else
    sgny=1;
end
if nargin==1
    AxesHandle=gca;
end

%xc=repmat(PCR.Offset(1)+(0:PCR.Size(2))*PCR.CellSize(1),PCR.Size(1)+1,1);
%yc=repmat((PCR.Offset(2)+sgny*(0:PCR.Size(1))*PCR.CellSize(2))',1,PCR.Size(2)+1);
%drawgrid(xc,yc)
%set(gca,'da',[1 1 1])

x=repmat(PCR.Offset(1)+((1:PCR.Size(2))-0.5)*PCR.CellSize(1),PCR.Size(1),1);
y=repmat((PCR.Offset(2)+sgny*((1:PCR.Size(1))-0.5)*PCR.CellSize(2))',1,PCR.Size(2));

x5=x(PCR.Data==5);
y5=y(PCR.Data==5);
nrows=PCR.Size(1);
tovector=[-nrows+1 1 nrows+1 -nrows 0 nrows -nrows-1 -1 nrows-1]';
i=find(PCR.Data>0);
i(:,2)=i+tovector(PCR.Data(i));
xx=[x(i) repmat(NaN,size(i,1),1)]';
%xx(2,:)=mean(xx([1 2],:));
yy=[y(i) repmat(NaN,size(i,1),1)]';
%yy(2,:)=mean(yy([1 2],:));
hLoc(1)=line(xx(:),yy(:),'parent',AxesHandle);
hLoc(2)=line(x5,y5,'marker','.','linestyle','none','parent',AxesHandle);
%plot(xx(:),yy(:),'k',x5,y5,'k.',x(i(:,1)),y(i(:,1)),'ro')
if nargin==1
    set(gca,'da',[1 1 1])
end
if nargout>0
    h=hLoc;
end
