function Str = dimprint(Dim,Selection)
%DIMPRINT Convert dimension structure into string for printing.

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

if isempty(Dim)
    Str = '<missing information>';
else
    if nargin>1
        Values = Dim.Values(Selection);
    else
        Values = Dim.Values;
    end
    if isequal(Dim.Type,'discrete-time')
        switch length(Values)
            case 0
                Str = '<none>';
            case 1
                Str = sprintf('%s',datestr(Values,0));
            case 2
                Str = sprintf('%s and %s',datestr(Values(1),0),datestr(Values(2),0));
            otherwise
                minD = min(Values);
                maxD = max(Values);
                if floor(minD)==floor(maxD)
                    Str = sprintf('%i points in time between %s and %s on %s', ...
                        length(Values),datestr(minD,13),datestr(maxD,13),datestr(maxD,1));
                else
                    Str = sprintf('%i points in time between %s and %s', ...
                        length(Values),datestr(minD,0),datestr(maxD,0));
                end
        end
    else
        if ~isempty(Dim.Unit)
            unit = sprintf(' %s',Dim.Unit);
        else
            unit = '';
        end
        if iscellstr(Values)
            switch length(Values)
                case 0
                    Str = '<none>';
                otherwise
                    Str = sprintf('%s, ',Values{:});
                    Str(end-1:end) = [];
            end
        elseif any(isnan(Values))
            switch length(Values)
                case 0
                    Str = '<none>';
                case 1
                    Str = sprintf('%i <value not specified>\n',length(Values));
                otherwise
                    Str = sprintf('%i <values not specified>',length(Values));
            end
        else
            switch length(Values)
                case 0
                    Str = '<none>';
                case 1
                    Str = sprintf('%g%s',Values,unit);
                case 2
                    Str = sprintf('%g%s and %g%s',Values(1),unit,Values(2),unit);
                otherwise
                    minV = min(Values);
                    maxV = max(Values);
                    if isequal(Values,minV:maxV)
                        Str = sprintf('%g%s to %g%s',minV,unit,maxV,unit);
                    else
                        Str = sprintf('%i values in range %g%s to %g%s', ...
                            length(Values),minV,unit,maxV,unit);
                    end
            end
        end
    end
end
