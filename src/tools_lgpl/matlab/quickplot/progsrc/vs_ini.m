function VS=vs_ini(varargin)
%VS_INI Creates a NEFIS file.
%   NFStruct = VS_INI('DataFile','DefFile') creates NEFIS definition and
%   data files. The file is stored in a platform independent format.
%
%   NFStruct = VS_INI('File') creates a NEFIS file containing both
%   definition and data. The file is stored in a platform independent format.
%
%   Optional arguments:
%    * 'version',VERSION
%      Specify whether a NEFIS 4 or NEFIS 5 file will be created. NEFIS 5
%      files can grow bigger than 4GB but are not compatible with somewhat
%      older programs. The default file format is NEFIS 4.
%    * 'byteorder',BYTEORDER
%      The byte order may be specified as 'b' (platform dependent) or 'n'
%      (neutral = big-endian) for NEFIS 4 files. The byte order may be
%      specified as 'b' (big-endian) or 'l' (little-endian) for NEFIS 5
%      files.
%
%   Example
%      NFS1 = vs_ini('myNefis.daf','version',5)
%      %Creates a new combined data/definition NEFIS 5 file.
%
%   See also VS_USE, VS_COPY, VS_DEF, VS_PUT.

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

version = 4;
byteorder = '';
onefile = 1;
vs_debug = 0;

i = 1;
file = 0;
while i <= nargin
   if ischar(varargin{i})
      switch lower(varargin{i})
         case 'version'
            i = i+1;
            version = varargin{i};
         case 'byteorder'
            i = i+1;
            byteorder = varargin{i};
         case 'debug'
            vs_debug = 1;
         otherwise
            file = file+1;
            switch file
               case 1
                  data_file = varargin{i};
               case 2
                  def_file = varargin{i};
                  onefile = 0;
               otherwise
                  error('Unexpected function argument %i',i)
            end
      end
   end
   i = i+1;
end

% Since the file is still small we can deal with it using signed pointers
% instead of unsigned pointers. 
HashTable=-ones(1,997);

if ~ischar(byteorder)
   error('Invalid byte order specifier, should be string valued.');
end

if isequal(version,4) | isequal(version,'v4') | isequal(version,'4')
   version = 4;
   versionstring = 'Version 4.00.00';
   AddressType = 'int32';
   if isempty(byteorder)
      byteorder = 'N';
   end
   switch lower(byteorder)
      case 'b'
         Format = {};
      case 'n'
         Format = {'b'};
      otherwise
         error('Invalid byteorder specifier for version 4, should be n (neutral) or b (binary, native).');
   end
elseif isequal(version,5) | isequal(version,'v5') | isequal(version,'5')
   version = 5;
   versionstring = '5.00.00';
   AddressType = 'int64';
   if isempty(byteorder)
      byteorder = 'L';
   end
   switch lower(byteorder)
      case 'b'
         Format = {'b'};
      case 'l'
         Format = {'l'};
      otherwise
         error('Invalid byteorder specifier for version 5, should be l or b.');
   end
else
   error('Invalid version specifier, should be 4 or 5.');
end

company='Deltares';
%[C,MAXSIZE,ENDIAN] = computer;

if onefile
   % =====================================================
   % Start writing the dat/def file
   % =====================================================
   fidat=fopen(data_file,'w',Format{:});
   if vs_debug
      fprintf(vs_debug,'Combined data/definition file: %s\n',data_file);
      fprintf(vs_debug,'Opened using filehandle: %i.\n\n',fidat);
   end

   Header = [company ', NEFIS Definition and Data File; ' versionstring];
   Header(128)=upper(byteorder);

   fwrite(fidat,Header,'char');                   % NEFIS defdat file header
   fwrite(fidat,0,AddressType);                   % Length of NEFIS data file

   fwrite(fidat,HashTable,AddressType);           % Write ELEMENT DEF hash table
   fwrite(fidat,HashTable,AddressType);           % Write CELL DEF hash table
   fwrite(fidat,HashTable,AddressType);           % Write GROUP DEF hash table
   fwrite(fidat,HashTable,AddressType);           % Write GROUP DATA hash table

   Size=ftell(fidat);
   fseek(fidat,128,-1);
   fwrite(fidat,Size,AddressType);
   fclose(fidat);
   VSX=vs_use(data_file,'quiet');
else
   % =====================================================
   % Start writing the dat file
   % =====================================================
   fidat=fopen(data_file,'w',Format{:});
   if vs_debug
      fprintf(vs_debug,'Data file              : %s\n',data_file);
      fprintf(vs_debug,'opened using filehandle: %i.\n',fidat);
   end

   %Header='NEFIS HP-UX Versie 1.00 DATA FILE';
   Header = [company ', NEFIS Data File; ' versionstring];
   Header(60)=upper(byteorder);
   fwrite(fidat,Header,'char');                   % NEFIS data file header
   fwrite(fidat,0,AddressType);                   % Length of NEFIS data file

   fwrite(fidat,HashTable,AddressType);           % Write GROUP DATA hash table

   Size=ftell(fidat);
   fseek(fidat,60,-1);
   fwrite(fidat,Size,AddressType);
   fclose(fidat);

   % =====================================================
   % Start writing the def file
   % =====================================================
   fidef=fopen(def_file,'w',Format{:});
   if vs_debug
      fprintf(vs_debug,'Definition file        : %s\n',def_file);
      fprintf(vs_debug,'opened using filehandle: %i.\n\n',fidef);
   end

   %Header='NEFIS HP-UX Versie 1.00 DEFN FILE';
   Header = [company ', NEFIS Definition File; ' versionstring];
   Header(60)=upper(byteorder);
   fwrite(fidef,Header,'char');                   % NEFIS definition file header
   fwrite(fidat,0,AddressType);                   % Length of NEFIS definition file

   fwrite(fidef,HashTable,AddressType);           % Write ELEMENT DEF hash table
   fwrite(fidef,HashTable,AddressType);           % Write CELL DEF hash table
   fwrite(fidef,HashTable,AddressType);           % Write GROUP DEF hash table

   Size=ftell(fidef);
   fseek(fidef,60,-1);
   fwrite(fidef,Size,AddressType);
   fclose(fidef);
   VSX=vs_use(data_file,def_file,'quiet');
end

if nargout>0
   VS=VSX;
end
