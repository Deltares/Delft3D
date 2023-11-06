%MAKE_COLORMAPS
%   Script to create a plot of all colour maps in the colormaps directory.

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

cd progsrc/private
d=dir('../colormaps/*.clrmap');
Nd=length(d);
set(gcf,'color','w')
for i=1:Nd
    S=clrmap('read',['../colormaps/',d(i).name]);
    axes('position',[0.01+0.98*(i-1)*1.2/(Nd+(Nd-1)*0.2) 0.01 0.98/(Nd+(Nd-1)*0.2) 0.98])
    if isfield(S,'AlternatingColors') & S.AlternatingColors
        NCol=size(S.Colors,1);
        image(repmat(reshape(clrmap(S,NCol),[NCol 1 3]),[1 3]))
    else
        image(repmat(reshape(clrmap(S),[64 1 3]),[1 3]))
    end
    box on
    set(gca,'xtick',[],'ytick',[])
end
cd ../..
