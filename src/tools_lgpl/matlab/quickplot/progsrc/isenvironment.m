function OK = ISENVIRONMENT(NAME)
%ISENVIRONMENT  Checks the code evaluation environment.
%   NAME = ISENVIRONMENT returns the name of the environment in which the
%   code is running.
%
%   ISENVIRONMENT(NAME) returns true if the operating environment matches
%   the specified environment.
%
%   The following environments are supported:
%      MATLAB, Octave, Freemat
%
%   See also VER, VERSION, VERSTRING.

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

if exist('OCTAVE_VERSION')
   Name = 'Octave';
elseif exist('verstring') && ~isempty(strfind(verstring,'FreeMat'))
   Name = 'FreeMat';
else
   Name = 'MATLAB';
end

if nargin==0
   OK = Name;
else
   OK = strcmpi(NAME,Name);
end
