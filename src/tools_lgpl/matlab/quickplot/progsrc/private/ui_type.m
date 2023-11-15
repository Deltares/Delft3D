function varargout=ui_type(varargin)
%UI_TYPE Simple selection dialog.
%   [SelectedType,SelectedNr]=UI_TYPE(Types)
%   creates a dialog in which the user can select one of the type
%   strings specified in the cell string array Types. The selected type
%   string is returned as SelectedType, its number in the list is
%   returned as SelectedNr.
%
%   Default type can be specified as an additional input arguments:
%   ...=UI_TYPEANDNAME(Types,DefaultType)
%
%   The dialog name/title is by default empty. It can be set by
%   specifying the keyword WINDOWTITLE and the title:
%   ...=UI_TYPEANDNAME(...,'windowtitle',Title)
%
%   See also UI_TYPEANDNAME

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

[varargout{1:max(1,nargout)}]=ui_typeandname(varargin{:},'specifyname','off');
