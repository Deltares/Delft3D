function POout = plotoptions(A,plotstyle)
%PLOTOPTIONS Obtain plot options for selected object and plot style.

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

PS = plotstyles(A);
PSN = {PS.Name};
iPS = ustrcmpi(plotstyle,PSN);
if iPS<0
    error('Plot style %s not available for the data provided.',plotstyle)
end
PS = PS(iPS);
PO = PS.Function('getoptions');

if nargout==0
    if isempty(PO)
        fprintf('No options available for %s.\n',PS.Name);
    else
        fprintf('Available options for %s:\n',PS.Name);
        PON = {PO.Name};
        Str = sprintf('   %%-%is : %%s',max(cellfun('length',PON)));
        for i = 1:length(PO)
            fprintf(Str,PO(i).Name,PO(i).Type)
            if strcmp(PO(i).Type,'list')
                List = sprintf('''%s'',',PO(i).List{:});
                fprintf(' {%s}',List(1:end-1))
            end
            fprintf('\n')
        end
    end
else
    POout = PO;
end
