function Data = golder(cmd,varargin)
%GOLDER Read/write a Golder data file.
%   S = GOLDER('read',FILENAME) opens a GOLDER data file and reads the 3D
%   data sets contained in it. The returned structure S has fields called
%   Names and Values.
%
%   GOLDER('write',FILENAME,NAME1,ARRAY1,NAME2,ARRAY2,...) writes the
%   arrays ARRAY1,ARRAY2,... to an ASCII file named FILENAME in Golder data
%   format. The names NAME1, NAME2, ... will be added as data labels. The
%   data should be three-dimensional and all arrays should have the same
%   dimensions.
%
%   Example
%      % write data file
%      golder('write','golder.txt','ones',ones(3,4,5),'zeros',zeros(3,4,5));
%      % read data file
%      D = golder('read','golder.txt');
%

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

switch lower(cmd)
   case 'read'
      Data = readgolder(varargin{:});
   case 'write'
      writegolder(varargin{:});
   otherwise
      error('unknown command %s',cmd)
end


function Data = readgolder(FileName)
fid = fopen(FileName,'r','n','US-ASCII');
Line = fgetl(fid);
if length(Line)<7 | ~strcmp(Line(1:7),'# Cell[')
   fclose(fid);
   error('Golder file should start with ''# Cell[''.')
end
Quotes = strfind(Line,'"');
nquants = length(Quotes)/2;
Data.Names = cell(1,nquants);
Data.Values = cell(1,nquants);
%
j = 0;
for i=1:2:length(Quotes)
   j = j+1;
   Data.Names{j} = Line(Quotes(i)+1:Quotes(i+1)-1);
end
%
data = fscanf(fid,[' Cell[%i_%i_%i]' repmat(' %f',1,nquants)],[nquants+3 inf]);
fclose(fid);
%
I = data(1,:);
J = data(2,:);
K = data(3,:);
Dims = [max(I) max(J) max(K)]+1;
I = I+1+J*Dims(1)+K*Dims(1)*Dims(2);
for i = 1:nquants
   values = repmat(NaN,Dims);
   values(I) = data(3+i,:);
   Data.Values{i} = values;
end

function writegolder(FileName,varargin)
if nargin<3
   error('not enough input arguments: at least one 3d data field with label required.')
end
%
ndatafields = length(varargin);
if round(ndatafields/2)*2~=ndatafields
   error('invalid list of input arguments: incomplete label/data field pair.')
end
ndatafields = ndatafields/2;
%
j = 0;
Name = cell(1,length(varargin)/2);
for i = 1:2:length(varargin)
   j = j+1;
   if ~ischar(varargin{i})
      error('argument %i is an invalid label.',i+2)
   else
      Name{j} = varargin{i};
   end
   %
   if i==1
      Dims = size(varargin{i+1});
      if length(Dims)~=3
         error('dimensionality error: data field must be three dimensional.')
      end
      data = zeros(3+ndatafields,prod(Dims));
      %
      data(1,:) = repmat((1:Dims(1))-1,1,Dims(2)*Dims(3)); %I
      J = repmat((1:Dims(2))-1,Dims(1),Dims(3));
      data(2,:) = J(:)';
      K = repmat((1:Dims(3))-1,Dims(1)*Dims(2),1);
      data(3,:) = K(:)';
   else
      if ~isequal(size(varargin{i+1}),Dims)
         error('dimension mismatch: all data field should have the same dimensions.')
      end
   end
   %
   data(j+3,:) = varargin{i+1}(:)';
end
fid = fopen(FileName,'w','n','US-ASCII');
fprintf(fid,['# Cell[I_J_K]' repmat(' "%s"',1,ndatafields),'\n'],Name{:});
fprintf(fid,['Cell[%i_%i_%i]' repmat(' %f',1,ndatafields) ,'\n'],data);
fclose(fid);
