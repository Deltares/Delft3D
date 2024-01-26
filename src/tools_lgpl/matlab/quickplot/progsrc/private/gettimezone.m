function [TZshift,TZstr]=gettimezone(FI,domain,Props)
%GETTIMEZONE Default implementation for timezone.

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

if ischar(FI)
    switch FI
        case 'supported'
            [TZshift,TZstr] = gettables;
        otherwise
            [TZshift,TZstr] = gettables;
            i = ustrcmpi(FI,TZstr);
            if i<0
                TZshift = NaN;
                TZstr   = FI;
            else
                TZshift = TZshift(i);
                TZstr = TZstr{i};
            end
    end
else
    TZshift = NaN;
    TZstr    = '';
end

function [TZshift,TZstr] = gettables
table = {...
    'PST'	-8
    'PDT'	-7
    'MST'	-7
    'MDT'	-6
    'CST'	-6
    'CDT'	-5
    'EST'	-5
    'EDT'	-4
    'GMT'   0
    'WET'	0
    'UTC'	0
    'WEDT'	1
    'CET'	1
    'CEDT'	2
    'EET'	2
    'EEDT'	3
    'WAT'	1
    'CAT'	2
    'EAT'	3
    'AWST'	8
    'AWDT'	9
    'ACST'	9.5
    'ACDT'	10.5
    'AEST'	10
    'AEDT'	11};
tzones = [-12 -11 -10 -9.5 -9 -8 -7 -6 -5 -4.5 -4 -3.5 -3 -2 -1 1 2 3 3.5 4 4.5 5 5.5 5.75 6 6.5 7 8 8.75 9 9.5 10 10.5 11 11.5 12 12.75 13 14]';
TZstr = table(:,1);
TZshift = cat(1,table{:,2},tzones);
TZstr{length(TZshift)} = '';
for i = length(TZshift):-1:1
    Si = abs(TZshift(i));
    if round(Si)==Si
        shift = sprintf('%2.2d',Si);
    else
        shift = sprintf('%2.2d:%2.2d',floor(Si),round((Si-floor(Si))*60));
    end
    if TZshift(i)>0
        UTCshift = ['UTC+' shift];
    elseif TZshift(i)<0
        UTCshift = ['UTC-' shift];
    else % 0
        UTCshift = 'UTC±00';
    end
    if isempty(TZstr{i})
        TZstr{i} = UTCshift;
    elseif ~strcmp(TZstr{i},'UTC')
        TZstr{i} = [TZstr{i} ' (' UTCshift ')'];
    end
end
[TZshift,Reorder] = sort(TZshift);
TZstr = TZstr(Reorder);
