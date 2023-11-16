function font=qp_fontsettings(fontstr)
%QP_FONTSETTINGS Convert between INI file font settings and font structures.

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

flds={'Angle','Name','Units','Size','Weight'};
font='';
if ischar(fontstr)
    %
    % Define a font structure using fontstr as prefix and using the settings
    % in the INI file managed by qp_settings for the various properties.
    % Example: fontstr can be 'DefaultUicontrolFont'
    %
    font=[];
    for i=1:length(flds)
        str=flds{i};
        font = setfield(font,[fontstr str],qp_settings(['UIFont' str]));
    end
    %if isequal('DefaultUicontrolFont',fontstr)
    %   font.DefaultUicontrolForegroundColor='w';
    %   font.DefaultUicontrolBackgroundColor='b';
    %end
else
    %
    % Store the various properties/fields of the font structure in the INI
    % file managed by qp_settings.
    %
    for i=1:length(flds)
        str=flds{i};
        qp_settings(['UIFont' str],getfield(fontstr,['Font' str]))
    end
end
