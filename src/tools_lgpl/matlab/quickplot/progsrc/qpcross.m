function DATA = qpcross(varargin)
%QPCROSS Cross-section of data: one point in time.
%   DATA = QPCROSS(FILE,DOMAINNR,FIELD,SUBFIELD,T,XB,YB) reads the data
%   for the specified quantity FIELD from the specified FILE for the
%   DOMAINNR and SUBFIELD for the indicated time T, and slice the data
%   along the line (XB,YB) where XB and YB are vectors of x- and
%   y-coordinates. DOMAINNR and SUBFIELD are required for some files and
%   quantities but can be skipped for others.
%
%   This function is equivalent to the following two statements
%      DATA = QPREAD(FILE,DOMAINNR,FIELD,'griddata',SUBFIELD,T)
%      DATA = VSLICE(DATA,'XY',[XB YB])
%   if XB and YB are column vectors.
%
%   Example
%      times = qpread(FI,'velocity','size');
%      tLast = length(times);
%      data2dv = qpcross(FI,'velocity',tLast,[0 35000],[15000 15000]);
%   
%   See also QPFILE, QPREAD, VSLICE, QPCROSST.

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

if nargin<4 % DOMAINNR and SUBFIELD are only required for some FIELDs
    error('Not enough input arguments.')
end
xb = varargin{end-1};
yb = varargin{end};
if ~isequal(size(xb),size(yb)) | ~isnumeric(xb) | ~isnumeric(yb) | (~isequal(size(xb),[1 length(xb)]) & ~isequal(size(xb),[length(xb) 1]))
    error('Invalid or inconsistent data specified for coordinates (XB,YB) of line.')
end
quantspec = varargin(1:end-2);
%
FILE = quantspec{1};
domains = qpread(FILE,'domains');
if ~isempty(domains)
   DOMAINNR = quantspec(2);
   FIELD = quantspec{3};
   OTHER = quantspec(4:end);
else
   DOMAINNR = {};
   FIELD = quantspec{2};
   OTHER = quantspec(3:end);
end
DATA = qpread(FILE,DOMAINNR{:},FIELD,'griddata',OTHER{:});
DATA = vslice(DATA,'XY',[xb(:) yb(:)]);
