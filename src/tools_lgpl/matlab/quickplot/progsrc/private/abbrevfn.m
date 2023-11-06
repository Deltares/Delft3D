function shortname=abbrevfn(longname,maxlength)
%ABBREVFN Abbreviate filename.
%   SHORTNAME=ABBREVFN(LONGNAME,MAXLEN) removes as many subdirectory
%   sections as are necessary to make filename shorter than the specified
%   maximum length MAXLEN. The default value for MAXLEN is 32.

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

if nargin<2
   maxlength=32;
end

if length(longname)<maxlength
   shortname=longname;
   return
end

if strmatch('http://',longname) | strmatch('https://',longname) | strmatch('ftp://',longname)
   I=strfind(longname,'/');
   %
   % always keep hostname: so, start with http://host/...
   %
   ikeep = 3;
   Iikeep = I(ikeep);
elseif strcmp(computer,'PCWIN') % d:\...\dir\file
   I=strfind(longname,filesep);
   if isempty(I)
      I=strfind(longname,'/');
   end
   %
   % always keep drive: so, start with d:\...
   %
   ikeep = 1;
   if length(I)>3 & I(1)==1 & I(2)==2
      %
      % keep hostname: so, start with \\host\
      %
      ikeep = 3;
   end
   Iikeep = I(ikeep);
else
   I=strfind(longname,filesep);
   %
   % don't keep anything at beginning
   %
   ikeep = 0;
   Iikeep = 0;
end

%
% don't count closing slash
%
if ~isempty(I) & I(end)==length(longname)
   I(end)=[];
end
%
% don't insert '...' if that makes the name longer
%
if length(I)<=ikeep | (length(I)==ikeep+1 & I(ikeep+1)-Iikeep<=4)
   shortname=longname;
else
   L=Iikeep+length(longname)-I+3;
   i=min(find(L<maxlength));
   if isempty(i)
      i=length(L);
   end
   shortname=[longname(1:Iikeep) '...' longname(I(i):end)];
end
