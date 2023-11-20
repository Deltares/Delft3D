function VSout=vs_use(varargin)
%VS_USE Initiates the use of a NEFIS file.
%   NFStruct=VS_USE('filename') scans the file and returns a VS structure
%   containing important metadata of the NEFIS file that is being opened.
%   The contents of NFStruct is used by other routines to access the real
%   data in the file.
%
%   NFStruct=VS_USE(...,'quiet') reads the specified file without showing a
%   wait bar.
%
%   NFStruct=VS_USE(...,'debug') writes a debug information to a file while
%   scanning the file.
%
%   See also QPFOPEN, QPREAD, VS_DISP, VS_GET, VS_LET, VS_DIFF, VS_FIND,
%      VS_TYPE.

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

persistent VSKeep
if ~isstandalone
   mlock
end

ViewSelect.FileType='NEFIS';
ViewSelect.SubType='';
ViewSelect.FileName='';
ViewSelect.DatExt='';
ViewSelect.DefExt='';
ViewSelect.Format='b';
ViewSelect.AddressType='uint64';
ViewSelect.GrpDat=[];
ViewSelect.GrpDef=[];
ViewSelect.CelDef=[];
ViewSelect.ElmDef=[];

if (nargin==1) & isstruct(varargin{1}) % vs_use(Struct) % to refresh 'lastread'
   INP=varargin{1};
   if ~isequal(size(INP),[1 1])
      error('Scalar structure expected.')
   end
   FN=fieldnames(INP);
   ExpFN={'FileName','DatExt','DefExt','GrpDat','GrpDef','CelDef','ElmDef','Format'};
   diffFN=setdiff(ExpFN,FN);
   if ~(isempty(diffFN) | isequal(diffFN,{''})) % anything missing?
      error('Missing field: %s.',diffFN{1})
   end
   VSKeep=INP;
   if nargout>0
      VSout=VSKeep;
   end
   return
end

INP=varargin;
showwaitbar=isenvironment('MATLAB');
for i=1:length(INP)
   if isequal(INP{i},'quiet')
      showwaitbar=0;
      INP(i)=[];
      break
   end
end
vs_debug=0;
for i=1:length(INP)
   if isequal(INP{i},'debug')
      vs_debug=1;
      INP(i)=[];
      break
   end
end
without_def=0;
for i=1:length(INP)
   if isequal(INP{i},'nodef')
      without_def=1;
      INP(i)=[];
      break
   end
end
data_file='';
def_file='';
switch length(INP)
   case 0
      filename='?';
   case 1
      filename=INP{1};
   case 2
      filename=INP{1};
      data_file=INP{1};
      def_file=INP{2};
   otherwise
      error('Too many or invalid input arguments.')
end

if ~ischar(filename),
   error('Invalid filename')
elseif strcmp(filename,'munlock')
   munlock
   if nargout>0
      VSout=0;
   end
   return
elseif strcmp(filename,'lastread')
   if nargout>0
      VSout=VSKeep;
   else
      if isstruct(VSKeep)
         fprintf('%s%s\n',VSKeep.FileName,VSKeep.DatExt);
      else
         fprintf('None opened so far.\n');
      end
   end
   return
end

FilenameAsked=0;
if isempty(filename) | strcmp(filename,'?') % Was a question mark specified?
   FilenameAsked=1;
   [fname,fpath]=uigetfile('*.*','Select data file');
   if ~ischar(fname) % cancel
      if nargout>0
         VSout=[];
      end
      return
   end
   filename=[fpath,fname];
end

if isempty(data_file) & isempty(def_file)
   if ~isempty(filename)
      filename=absolutefile(filename);
   end
   %
   try
      onefile=1;
      data_file = filename;
      def_file = filename;
      [fidat,Header,ByteOrder,AddressType,AddressBytes,HeaderLength]=OpenDataFile(filename,1,0);
      if fidat>0
         fclose(fidat);
      else
         error('Not one single combined datdef NEFIS file, now try a split approach.')
      end
      filename='';
   catch
      % The specified file by itself is not a valid NEFIS file.
      %
      % Has the file an extension?
      [p,f,file_ext]=fileparts(filename);
      filename = fullfile(p,f);
      %
      onefile=0;
      switch lower(file_ext)
         case {'.dat','.def'}
            data_file = [filename transfercase(file_ext,'.dat')];
            def_file = [filename transfercase(file_ext,'.def')];
         case {'.wdi','.wdo'}
            data_file = [filename file_ext];
            def_file = [filename transfercase(file_ext,'.wdf')];
         case {'.wdf'}
            wdi_file = [filename transfercase(file_ext,'.wdi')];
            wdo_file = [filename transfercase(file_ext,'.wdo')];
            if exist(wdi_file,'file') & ~exist(wdo_file,'file')
               data_file = wdi_file;
            else
               data_file = wdo_file;
            end
            def_file = [filename transfercase(file_ext,'.wdf')];
         case '' % backward compatibility with *.mat files of *.dat,*.def combinations
            data_file = [filename '.dat'];
            def_file = [filename '.def'];
         otherwise
            if (length(file_ext)==4) & (lower(file_ext(3))=='d') & ...
                  ( (lower(file_ext(4))=='a') | (lower(file_ext(4))=='f') )
               % mda/mdf, rda/rdf, etc.
               data_file = [filename transfercase(file_ext,['.' file_ext(2) 'da'])];
               def_file = [filename transfercase(file_ext,['.' file_ext(2) 'df'])];
            elseif FilenameAsked
               data_file=absolutefile([filename file_ext]);
               [fname,fpath]=uigetfile('*.*','Select definition file');
               if ~ischar(fname) % cancel
                  if nargout>0
                     VSout=[];
                  end
                  return
               end
               filename=[fpath,fname];
               def_file=absolutefile(filename);
               onefile=strcmp(data_file,def_file);
            else
               error('Unable to open specified file as NEFIS file.')
            end
      end
      filename='';
   end
elseif isempty(data_file) | isempty(def_file) % one specified
   error('Empty data filename or definition filename specified');
else % data and definition files specified
   data_file=absolutefile(data_file);
   def_file=absolutefile(def_file);
   onefile=strcmp(data_file,def_file);
   filename='';
end

if ~exist(data_file,'file')
   error('Required data file does not exist. Check name:\n%s',data_file)
elseif ~exist(def_file,'file') && ~without_def
   error('Required definition file does not exist. Check name:\n%s',def_file)
end

% -----------------------------------------------------
% Reading the dat and def files
% -----------------------------------------------------
VS=ViewSelect;
%
% first make sure that fidef, fidat and hWaitBar exist
% for the catch block.
%
fidef=[];
fidat=[];
hWaitBar=[];
%
% then enter the try/catch block
%
try
   if vs_debug
      vs_debug=fopen([tempdir 'vs_use.dbg'],'w');
      if vs_debug<=0
         vs_debug=0;
         warning(sprintf('Cannot open debug file: %svs_use.dbg.',tempdir));
      else
         fprintf(1,'Writing to debug file: %svs_use.dbg ...\n',tempdir);
      end
      YesNoStr={'No' 'Yes'};
   end
   
   % =====================================================
   % Start reading the dat file
   % =====================================================
   if vs_debug
      if onefile
         fprintf(vs_debug,'Combined data/definition file: %s\n',data_file);
      else
         fprintf(vs_debug,'Data file      : %s\n',data_file);
         fprintf(vs_debug,'Definition file: %s\n',def_file);
      end
   end
   [fidat,Header,Format,AddressType,AddressBytes,HeaderLength]=OpenDataFile(data_file,onefile,vs_debug);
   
   % Format of data/definition file:
   %
   % header string:
   % WL|DelftHydraulics, NEFIS Definition and Data File; Version 4.00.014
   % file format (N/B/?)
   % file size
   % GROUP hash table
   
   % Format of data file:
   %
   % header string: NEFIS HP-UX Versie 1.00 DATA FILE
   % file format (N/B/?)
   % file size
   % GROUP hash table
   % data
   
   if showwaitbar
      hWaitBar = waitbar(0,'Please wait while reading ...');
      set(hWaitBar,'closerequestfcn','');
   end
   
   if onefile
      if vs_debug
         fprintf(vs_debug,'Jumping to address: %i.\n',HeaderLength+3*AddressBytes*997);
      end
      % Skip ELEMENT, CELL, GROUP hash tables
      if fseek(fidat,HeaderLength+3*AddressBytes*997,-1)<0
         error('Out-of-file while jumping to DATA hash table.')
      end
   end
   
   %% reading GROUP data records
   
   % -----------------------------------------------------
   % Read the dat file: data GROUPs
   % -----------------------------------------------------
   if vs_debug
      fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
      fprintf(vs_debug,'Reading group data hash table ...\n');
      fprintf(vs_debug,'-------------------------------------------------------\n');
   end
   [NonEmptyBuck,Success,NFail]=ReadHashTableGetNonEmptyBuckets(fidat,AddressType,vs_debug); % scan the GROUP hash table
   
   if ~Success & isempty(NonEmptyBuck) & NFail>0
      error(' ')
   end
   
   % Link    : AddressType : Offset of next record in same BUCKET
   % Size    : AddressType : Size of GROUP record in data file
   % RecType : AddressBytes char      : Record type '   4' or '   5' (variable dim.)
   % Name    : 16 char     : Group data name
   % DefName : 16 char     : Group definition name
   % IANames : 5*16 char   : Integer attribute names
   % IAValue : 5*int32     : Integer values
   % RANames : 5*16 char   : Floating-point attribute names
   % RAValue : 5*float32   : Floating-point values
   % SANames : 5*16 char   : String attribute names
   % SAValue : 5*16 char   : Value strings
   
   for i=1:length(NonEmptyBuck)
      if showwaitbar
         waitbar(0.1+i/length(NonEmptyBuck)*0.1);
      end
      if vs_debug
         fprintf(vs_debug,'Jumping to group data record %i at %u ...\n',i,NonEmptyBuck(i));
      end
      fseek(fidat,NonEmptyBuck(i),-1);               % Go to GROUP data record i
      Link=fread(fidat,[1 1],AddressType);           % Offset of next record in same BUCKET, unsigned address
      Size=fread(fidat,[1 1],AddressType);           % Size of GROUP record in data file
      X=fread(fidat,AddressBytes,'uchar');           % Code of current record (should be '   4' or '   5')
      VS.GrpDat(i).Offset=NonEmptyBuck(i);
      VS.GrpDat(i).VarDim=(X(end)=='5');             % Indicator of variable size GROUP
      VS.GrpDat(i).Name=deblank(char(fread(fidat,[1 16],'uchar')));
      VS.GrpDat(i).DefName=deblank(char(fread(fidat,[1 16],'uchar')));
      % GROUP name, GROUP def. name
      VS.GrpDat(i).DefIndex=0;
      if vs_debug
         fprintf(vs_debug,'Record size          : %u bytes\n',Size);
         fprintf(vs_debug,'Var. dim. indicator  : ''%s'', interpreted as %s(%i).\n', ...
            char(X),YesNoStr{VS.GrpDat(i).VarDim+1},VS.GrpDat(i).VarDim);
         fprintf(vs_debug,'Group data name      : %s\n',VS.GrpDat(i).Name);
         fprintf(vs_debug,'Group definition name: %s\n',VS.GrpDat(i).DefName);
      end
      VS.GrpDat(i).IANames=char(transpose(fread(fidat,[16 5],'uchar')));
      % Integer attribute names
      VS.GrpDat(i).IAValue=fread(fidat,[1 5],'int32');
      % Integer attribute values
      if vs_debug
         fprintf(vs_debug,'Integer attribute names and values:\n');
         for vsdb=1:5
            fprintf(vs_debug,'''%s''   : %i\n', ...
               VS.GrpDat(i).IANames(vsdb,:),VS.GrpDat(i).IAValue(vsdb));
         end
      end
      VS.GrpDat(i).RANames=char(transpose(fread(fidat,[16 5],'uchar')));
      % Real attribute names
      VS.GrpDat(i).RAValue=fread(fidat,[1 5],'float32');
      % Real attribute values
      if vs_debug
         fprintf(vs_debug,'Real attribute names and values:\n');
         for vsdb=1:5
            fprintf(vs_debug,'''%s''   : %g\n', ...
               VS.GrpDat(i).RANames(vsdb,:),VS.GrpDat(i).RAValue(vsdb));
         end
      end
      VS.GrpDat(i).SANames=char(transpose(fread(fidat,[16 5],'uchar')));
      % String attribute names
      VS.GrpDat(i).SAValue=char(transpose(fread(fidat,[16 5],'uchar')));
      % String attribute values
      if vs_debug
         fprintf(vs_debug,'String attribute names and values:\n');
         for vsdb=1:5
            fprintf(vs_debug,'''%s''   : ''%s''\n', ...
               VS.GrpDat(i).SANames(vsdb,:),VS.GrpDat(i).SAValue(vsdb,:));
         end
      end
      if VS.GrpDat(i).VarDim
         DataSize=fread(fidat,1,AddressType); % Size of cell
      else
         DataSize=Size-392-3*AddressBytes;
      end
      VS.GrpDat(i).DataSize = DataSize;
      if vs_debug
         fprintf(vs_debug,'Size data block      : %u bytes\n',DataSize);
         fprintf(vs_debug,'\n');
      end
   end
   
   if without_def
       % definition file shouldn't be read (most likely because it doesn't
       % exist) as per request of the caller
       if vs_debug
           fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
           fprintf(vs_debug,'"nodef" specified. Skip reading the definition file ...\n');
           fprintf(vs_debug,'-------------------------------------------------------\n');
       end
   else
       % =====================================================
       % Read the def file
       % =====================================================
       % Format of definition file:
       %
       % header string: NEFIS HP-UX Versie 1.00 DEFN FILE
       % file size
       % ELEMENT hash table
       % CELL hash table
       % GROUP hash table
       % definitions
       
       if onefile
           fidef=fidat;
       else
           fidef=fopen(def_file,'r',Format);
           if vs_debug
               fprintf(vs_debug,'\n\nData file      : %s.\n',data_file);
               fprintf(vs_debug,'Definition file: %s.\n',def_file);
               fprintf(vs_debug,'Definition file opened using filehandle: %i.\n\n',fidef);
           end
           Header=char(fread(fidef,[1 60],'uchar'));         % NEFIS DEFINITION FILE HEADER
           Lfile=fread(fidef,1,AddressType);                 % Length of NEFIS definition file
           if vs_debug
               fprintf(vs_debug,'File header:\n%s\n',deblank(Header(1:(end-1))));
               fprintf(vs_debug,'Neutral file format indicator: %s.\n',Header(end));
               fprintf(vs_debug,'File length indicated: %u bytes\n',Lfile);
           end
       end
       
       % -----------------------------------------------------
       % Read the def file: GROUP definition
       % -----------------------------------------------------
       loc = HeaderLength+2*AddressBytes*997;
       if vs_debug
           fprintf(vs_debug,'Jumping to address: %u.\n',loc);
       end
       % Skip ELEMENT and CELL hash tables
       if fseek(fidef,loc,-1)<0
           error('Out-of-file while jumping to GROUP DEFINITION hash table.')
       end
       if vs_debug
           fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
           fprintf(vs_debug,'Reading group definition hash table ...\n');
           fprintf(vs_debug,'-------------------------------------------------------\n');
       end
       [NonEmptyBuck,Success,NFail]=ReadHashTableGetNonEmptyBuckets(fidef,AddressType,vs_debug);
       if ~Success & isempty(NonEmptyBuck) & NFail>0
           error(' ');
       end
       NonEmptyBuck=sort(NonEmptyBuck(:));              % Sort offsets
       
       % Link    : AddressType : Offset of next record in same BUCKET
       % Size    : AddressType : Size of GROUP definition record in definition file
       % RecType : AddressBytes char      : Record type '   3'
       % DefName : 16 char     : Group definition name
       % CelName : 16 char     : Cell name
       % NDim    : uint32      : Number of dimensions <= 5
       % SizeDim : 5 uint32    : Dimension sizes (SizeDim==0 for var. dim.)
       % OrderDim: 5 uint32    : Order of the dimensions on disk
       
       for i=1:length(NonEmptyBuck)
           if showwaitbar
               waitbar(0.3+(i/length(NonEmptyBuck))*0.2);
           end
           if vs_debug
               fprintf(vs_debug,'Jumping to group definition record %i at %u ...\n',i,NonEmptyBuck(i));
           end
           fseek(fidef,NonEmptyBuck(i),-1);                % Go to GROUP definition record i
           VS.GrpDef(i).Offset=NonEmptyBuck(i);            % Offset of the GROUP definition record on definition file
           Link=fread(fidef,[1 1],AddressType);            % Offset of next record in same BUCKET, unsigned address
           Size=fread(fidef,[1 1],AddressType);            % Size of current record
           Code=fread(fidef,[1 AddressBytes],'uchar');     % Code of current record (should be '   3')
           VS.GrpDef(i).Name=deblank(char(fread(fidef,[1 16],'uchar')));
           % Group definition name
           VS.GrpDef(i).CelName=deblank(char(fread(fidef,[1 16],'uchar')));
           % Associated cell name
           VS.GrpDef(i).CelIndex=0;
           if vs_debug
               fprintf(vs_debug,'Record size          : %i\n',Size);
               fprintf(vs_debug,'Record code          : ''%s''\n',char(Code));
               fprintf(vs_debug,'Group definition name: %s\n',VS.GrpDef(i).Name);
               fprintf(vs_debug,'Cell name            : %s\n',VS.GrpDef(i).CelName);
           end
           Dimens=fread(fidef,[1 11],'uint32');           % Number of dimensions, dimension sizes and order of the
           % dimensions on disc
           NDim=Dimens(1);                                % Number of dimensions
           vdim=find(Dimens(1+(1:NDim))==0);
           if vs_debug
               fprintf(vs_debug,'Number of dimensions : %i\n',NDim);
           end
           if NDim==0 % If number of dimensions is zero change it to one.
               NDim=1;
               Dimens(1)=1;
               if vs_debug
                   fprintf(vs_debug,'Corrected number of dimensions: %i\n',NDim);
               end
           end
           VS.GrpDef(i).SizeDim=Dimens(2:(1+NDim));       % Dimension sizes
           VS.GrpDef(i).OrderDim=Dimens(7:(6+NDim));      % Order of the dimensions on disk
           
           j=1;
           Found=0;
           while j<=length(VS.GrpDat) % Match GROUP definition name to stored info
               if strcmp(VS.GrpDat(j).DefName,VS.GrpDef(i).Name)
                   VS.GrpDat(j).DefIndex=i;
                   VS.GrpDat(j).SizeDim=Dimens(2:(1+NDim));   % Dimension sizes
                   VS.GrpDat(j).OrderDim=Dimens(7:(6+NDim));  % Order of the dimensions on disk
                   
                   if ~isempty(vdim) % Check for variable dimension
                       % Variable dimension(s) found
                       if ~VS.GrpDat(j).VarDim
                           % Variable dimension(s) found, but not specified in data file.
                           if vs_debug
                               fprintf(vs_debug,'WARNING: The group has a dimension of zero size.\n');
                               fprintf(vs_debug,'\n');
                           end
                           fprintf(1,'WARNING:\nGROUP ''%s'' has a dimension of zero size.\n',VS.GrpDef(i).Name);
                       elseif ~isequal(size(vdim),[1 1])
                           % More than one variable dimension.
                           if vs_debug
                               fprintf(vs_debug,'WARNING: The group has more than one variable dimension.\n');
                               fprintf(vs_debug,'\n');
                           end
                           fprintf(1,'WARNING:\nGROUP ''%s'' has more than one variable dimension.\n',VS.GrpDef(i).Name);
                           VS.GrpDat(j).VarDim=-1;
                       else
                           % One variable dimension
                           VS.GrpDat(j).VarDim=vdim;              % store index of variable dimension
                           if vs_debug
                               if onefile
                                   fprintf(vs_debug,'Scanning file to determine, size of dimension %i ...\n',vdim);
                               else
                                   fprintf(vs_debug,'Scanning data file to determine, size of dimension %i ...\n',vdim);
                               end
                           end
                           %
                           % Determine variable dimension size by scanning the
                           % pointer list in the data file. The pointer list starts
                           % immediately after the data group record (type '  5').
                           % The length of the data group is 392+4*AddressBytes.
                           %
                           dim=0;
                           loc = VS.GrpDat(j).Offset+392+4*AddressBytes;
                           fseek(fidat,loc,-1);
                           if vs_debug
                               fprintf(vs_debug,'Jumping to start of table 1 at %u ...\n',loc);
                           end
                           NByte=3;
                           switch AddressType
                               case 'uint32'
                                   Nil=2^32-1;
                               case 'uint64'
                                   Nil=2^64-1;
                           end
                           while NByte>=0
                               PointerList=fread(fidat,[1 256],AddressType);
                               k=max(find(PointerList~=Nil));
                               if vs_debug
                                   fprintf(vs_debug,'Byte %i (link=1,no link=0):\n',NByte+1);
                                   fprintf(vs_debug,[repmat(' %1i',[1 32]) '\n'],PointerList~=Nil);
                                   if isempty(k)
                                       fprintf(vs_debug,' => no reference.\n');
                                   else
                                       fprintf(vs_debug,' => max Byte %i = %i: offset = %1.0f.\n',NByte+1,k-1,PointerList(k));
                                   end
                               end
                               %fprintf(1,'NB=%i k=%i %s\n',[NByte,k],VS.GrpDat(j).Name);
                               if isempty(k)
                                   break
                               end
                               if NByte>0
                                   dim=dim+(k-1)*256^NByte;
                                   fseek(fidat,PointerList(k),-1);
                               else % NByte==0 % PointerList(1)==Nil, First Offset=...(2), Second Offset=...(3)
                                   dim=dim+k-1;
                               end
                               NByte=NByte-1;
                           end
                           VS.GrpDat(j).SizeDim(vdim)=dim;
                           Dimens(1+vdim)=dim;
                       end
                   else
                       if VS.GrpDat(j).VarDim
                           % No variable dimension(s) found, but specified in data file.
                           if vs_debug
                               fprintf(vs_debug,'WARNING: Variable dimension of GROUP ''%s'' not found.\n',VS.GrpDef(i).Name);
                               fprintf(vs_debug,'\n');
                           end
                           fprintf(1,'WARNING:\nVariable dimension of GROUP ''%s'' not found.\n',VS.GrpDef(i).Name);
                           VS.GrpDat(j).VarDim=-1;
                       end
                   end
                   if vs_debug
                       fprintf(vs_debug,'Dimensions           : [');
                       fprintf(vs_debug,' %i',Dimens(1+(1:NDim)));
                       fprintf(vs_debug,' ]');
                       fprintf(vs_debug,' %i',Dimens(1+((NDim+1):5)));
                       fprintf(vs_debug,'\nStored order         : [');
                       fprintf(vs_debug,' %i',Dimens(6+(1:NDim)));
                       fprintf(vs_debug,' ]');
                       fprintf(vs_debug,' %i',Dimens(6+((NDim+1):5)));
                       fprintf(vs_debug,'\n');
                   end
                   Found=1;
               end
               j=j+1;
           end
           if ~Found
               if vs_debug
                   fprintf(vs_debug,'WARNING: This group definition name does not correspond to any data group.\n');
                   fprintf(vs_debug,'\n');
               end
               fprintf(1,'WARNING:\nGROUP definition name ''%s'' does not correspond to any GROUP.\n',VS.GrpDef(i).Name);
           end
           if vs_debug
               fprintf(vs_debug,'\n');
           end
       end
       
       for i=1:length(VS.GrpDat)
           if ~isfield(VS.GrpDat(i),'DefIndex')
               if vs_debug
                   fprintf(vs_debug,'WARNING: The group %s is not defined.\n',VS.GrpDat(i).Name);
                   fprintf(vs_debug,'\n');
               end
               fprintf(1,'WARNING:\nGROUP ''%s'' not defined.\n',VS.GrpDat(i).Name);
           end
       end
       
       % -----------------------------------------------------
       % Read the def file: CELL definition
       % -----------------------------------------------------
       loc = HeaderLength + AddressBytes*997;
       if vs_debug
           fprintf(vs_debug,'Jumping to address: %u.\n',loc);
       end
       % Skip ELEMENT hash table
       if fseek(fidef,loc,-1)<0
           error('Out-of-file while jumping to CELL DEFINITION hash table.')
       end
       if vs_debug
           fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
           fprintf(vs_debug,'Reading cell definition hash table ...\n');
           fprintf(vs_debug,'-------------------------------------------------------\n');
       end
       [NonEmptyBuck,Success,NFail]=ReadHashTableGetNonEmptyBuckets(fidef,AddressType,vs_debug);
       if ~Success & isempty(NonEmptyBuck) & NFail>0
           error(' ');
       end
       NonEmptyBuck=sort(NonEmptyBuck(:));                 % Sort offsets
       
       % Link    : AddressType : Offset of next record in same BUCKET
       % Size    : AddressType : Size of CELL record in definition file
       % RecType : AddressBytes char      : Record type '   2'
       % CelName : 16 char     : Cell name
       % CellSize: AddressType : Cell Size
       % NumElm  : uint32      : Number of ELEMENTs per CELL
       % Elements: NumElm*16 char : Element names
       
       TotalNumElm=0;                                   % Total number of elements counter to zero
       for i=1:length(NonEmptyBuck)
           if showwaitbar
               waitbar(0.6+i/length(NonEmptyBuck)*0.05);
           end
           if vs_debug
               fprintf(vs_debug,'Jumping to cell definition record %i at %i ...\n',i,NonEmptyBuck(i));
           end
           fseek(fidef,NonEmptyBuck(i),-1);                % Go to CELL definition record i
           Link=fread(fidef,[1 1],AddressType);            % Offset of next record in same BUCKET, unsigned address
           Size=fread(fidef,[1 1],AddressType);            % Size of current record (variable)
           Code=fread(fidef,[1 AddressBytes],'uchar');     % Code of current record (should be '   2')
           VS.CelDef(i).Offset=NonEmptyBuck(i);
           VS.CelDef(i).Name=deblank(char(fread(fidef,[1 16],'uchar')));
           % Read CELL name
           CellSize=fread(fidef,[1 1],AddressType);       % Size of data per CELL
           VS.CelDef(i).CellSize = CellSize;
           NumElm=fread(fidef,[1 1],'uint32');            % Number of ELEMENTs per CELL
           VS.CelDef(i).Elm=zeros(1,NumElm);
           if vs_debug
               fprintf(vs_debug,'Record size       : %u bytes\n',Size);
               fprintf(vs_debug,'Record code       : ''%s''\n',char(Code));
               fprintf(vs_debug,'Cell name         : %s\n',VS.CelDef(i).Name);
               fprintf(vs_debug,'Number of elements: %i\n',NumElm);
               fprintf(vs_debug,'Size data block   : %u bytes\n',CellSize);
               for igdf = 1:length(VS.GrpDef)
                   if strcmp(VS.GrpDef(igdf).CelName, VS.CelDef(i).Name)
                       for igd = 1:length(VS.GrpDat)
                           if strcmp(VS.GrpDat(igd).DefName, VS.GrpDef(igdf).Name)
                               DataSize = VS.GrpDat(igd).DataSize;
                               fprintf(vs_debug,'- Data group %s size: %u bytes ', VS.GrpDat(igd).Name, DataSize);
                               if DataSize == CellSize
                                   fprintf(vs_debug,'(match)\n');
                               elseif DataSize == 4*ceil(CellSize/4)
                                   fprintf(vs_debug,'(matching: buffered to multiple of 4 bytes)\n');
                               elseif DataSize > CellSize
                                   fprintf(vs_debug,'(WARNING: more data than cell defines)\n');
                               else
                                   fprintf(vs_debug,'(WARNING: less data than cell defines)\n');
                               end
                           end
                       end
                   end
               end
           end
           
           j=1;
           Found=0;
           while j<=length(VS.GrpDef) % Match CELL name to stored info
               if strcmp(VS.GrpDef(j).CelName,VS.CelDef(i).Name)
                   VS.GrpDef(j).CelIndex=i;
                   TotalNumElm=TotalNumElm+NumElm;              % Increase total number of elements
                   Found=1;
               end
               j=j+1;
           end
           if ~Found
               if vs_debug
                   fprintf(vs_debug,'WARNING: This cell is not used in any group definition.\n');
                   fprintf(vs_debug,'\n');
               end
               fprintf(1,'WARNING:\nCELL name ''%s'' does not correspond to any GROUP.\n',VS.CelDef(i).Name);
           end
           if vs_debug
               fprintf(vs_debug,'\n');
           end
       end
       for i=1:length(VS.GrpDef)
           if ~isfield(VS.GrpDef(i),'CelIndex')
               if vs_debug
                   fprintf(vs_debug,'WARNING: The cell ''%s'' not defined.\n',VS.GrpDef(i).CelName);
                   fprintf(vs_debug,'\n');
               end
               fprintf(1,'WARNING:\nCELL ''%s'' not defined.\n',VS.GrpDef(i).CelName);
           end
       end
       if vs_debug
           fprintf(vs_debug,'The data contains %i elements.\n',TotalNumElm);
       end
       
       % -----------------------------------------------------
       % Read the def file: ELEMENT definition
       % -----------------------------------------------------
       loc = HeaderLength;
       if vs_debug
           fprintf(vs_debug,'Jumping to address: %u.\n',loc);
       end
       % Skip header
       if fseek(fidef,loc,-1)<0
           error('Out-of-file while jumping to ELEMENT DEFINITION hash table.')
       end
       
       if vs_debug
           fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
           fprintf(vs_debug,'Reading element definition hash table ...\n');
           fprintf(vs_debug,'-------------------------------------------------------\n');
       end
       [NonEmptyBuck,Success,NFail]=ReadHashTableGetNonEmptyBuckets(fidef,AddressType,vs_debug);
       if ~Success & isempty(NonEmptyBuck) & NFail>0
           error(' ');
       end
       NonEmptyBuck=sort(NonEmptyBuck(:));              % Sort offsets
       
       % Link    : AddressType : Offset of next record in same BUCKET
       % Size    : AddressType : Size of ELEMENT record in definition file
       % RecType : AddressBytes char      : Record type '   1'
       % Name    : 16 char     : Element name
       % Type    : 8 char      : Element type
       % ElmSize : AddressType : Size of element
       % ValSize : uint32      : Size of single value
       % Descript: 96 char     : Description
       % NDim    : uint32      : Number of dimensions
       % SizeDim : 5 uint32    : Dimension sizes
       
       for i=1:length(NonEmptyBuck)
           if showwaitbar
               waitbar(0.65+i/length(NonEmptyBuck)*0.2);
           end
           if vs_debug
               fprintf(vs_debug,'Jumping to element definition record %u at %i ...\n',i,NonEmptyBuck(i));
           end
           fseek(fidef,NonEmptyBuck(i),-1);                % Go to ELEMENT definition record i
           Link=fread(fidef,[1 1],AddressType);            % Offset of next record in same BUCKET, unsigned address
           Size=fread(fidef,[1 1],AddressType);            % Size of current record
           Code=fread(fidef,[1 AddressBytes],'uchar');     % Code of current record (should be '   1')
           Name=deblank(char(fread(fidef,[1 16],'uchar')));% Read ELEMENT name
           Type=char(fread(fidef,[1 8],'uchar'));          % Read variable type
           if vs_debug
               fprintf(vs_debug,'Record size         : %u bytes\n',Size);
               fprintf(vs_debug,'Record code         : ''%s''\n',char(Code));
               fprintf(vs_debug,'Element name        : %s\n',strrep(Name,char(0),' '));
               fprintf(vs_debug,'Element type        : %s\n',Type);
           end
           switch Type
               case 'CHARACTE'
                   Type=1;
               case 'COMPLEX '
                   Type=2;
               case 'INTEGER '
                   Type=3;
               case 'LOGICAL '
                   Type=4;
               case 'REAL    '
                   Type=5;
               otherwise
                   if vs_debug
                       fprintf(vs_debug,'WARNING: Unsupported elemented type!\n');
                   end
                   fprintf(1,'WARNING:\nELEMENT ''%s'' is of unknown type: ''%s''.\n',Name,deblank(Type));
           end
           SizeElm=fread(fidef,[1 1],AddressType);         % Size of ELEMENT
           SizeVal=fread(fidef,[1 1],'uint32');            % Size of single value
           Descript=char(fread(fidef,[1 96],'uchar'));     % Quantity, unit and description of ELEMENT
           Dimens=fread(fidef,[1 6],'uint32');             % Number of dimensions, dimension sizes
           if vs_debug
               fprintf(vs_debug,'Element size        : %u bytes\n',SizeElm);
               fprintf(vs_debug,'Size of datatype    : %u bytes\n',SizeVal);
               fprintf(vs_debug,'Element quantity    : ''%s''\n',Descript(1:16));
               fprintf(vs_debug,'Element unit        : ''%s''\n',Descript(17:32));
               fprintf(vs_debug,'Element description : ''%s''\n',Descript(33:96));
               fprintf(vs_debug,'Number of dimensions: %i\n',Dimens(1));
           end
           if Dimens(1)==0
               Dimens(1)=1;
               if vs_debug
                   fprintf(vs_debug,'Corrected number of dimensions: %i\n',Dimens(1));
               end
           end % Change any zero dimension to one
           if vs_debug
               fprintf(vs_debug,'Dimensions:         : [');
               fprintf(vs_debug,' %i',Dimens(2:(1+Dimens(1))));
               fprintf(vs_debug,' ]');
               fprintf(vs_debug,' %i',Dimens((1+Dimens(1)+1):6));
               fprintf(vs_debug,'\n\n');
           end
           
           VS.ElmDef(i).Offset=NonEmptyBuck(i);
           VS.ElmDef(i).Name=Name;
           VS.ElmDef(i).Type=Type;
           VS.ElmDef(i).SizeVal=SizeVal;
           VS.ElmDef(i).SizeElm=SizeElm;
           VS.ElmDef(i).Quantity=deblank(Descript(1:16));
           VS.ElmDef(i).Units=deblank(Descript(17:32));
           VS.ElmDef(i).Description=deblank(Descript(33:96));
           NDim=Dimens(1);
           VS.ElmDef(i).Size=Dimens(2:(1+NDim));
       end
       
       if vs_debug
           fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
           fprintf(vs_debug,'Reading elements from cell definition ...\n');
           fprintf(vs_debug,'-------------------------------------------------------\n');
       end
       if isempty(VS.ElmDef)
           ElmNames={};
       else
           ElmNames={VS.ElmDef(:).Name};
       end
       for i=1:length(VS.CelDef)
           if showwaitbar
               waitbar(0.85+i/length(VS.CelDef)*0.1);
           end
           %
           % Element names are listed at the end of the cell definition records.
           % The length of the bare cell definition record is 16+3*AddressBytes.
           %
           loc = VS.CelDef(i).Offset+20+4*AddressBytes;
           fseek(fidef,loc,-1);
           if vs_debug
               fprintf(vs_debug,'Jumping to elements of cell %i (%s) at %u ...\n',i,VS.CelDef(i).Name,loc);
           end
           for k=1:length(VS.CelDef(i).Elm)
               Name=deblank(char(fread(fidef,[1 16],'uchar'))); % Read ELEMENT name from CELL record
               j=strmatch(Name,ElmNames,'exact');
               if isempty(j)
                   if vs_debug
                       fprintf(vs_debug,'WARNING: The element ''%s'' is not defined.\n',Name);
                       fprintf(vs_debug,'\n');
                   end
                   fprintf(1,'WARNING:\nELEMENT ''%s'' is not defined.\n',Name);
               else
                   if length(j)>1
                       if vs_debug
                           fprintf(vs_debug,'WARNING: Multiple definitions of ELEMENT ''%s''.\n',Name);
                       end
                       fprintf(1,'WARNING:\nMultiple definitions of ELEMENT ''%s''.\n',Name);
                       j=j(1);
                   end
                   VS.CelDef(i).Elm(k)=j;
                   kf = VS.CelDef(i).Elm(1:k-1)==VS.CelDef(i).Elm(k);
                   if any(kf)
                       kf = find(kf);
                       duplicate = sprintf(' WARNING: this element duplicates element %i of this cell.',kf(1));
                       fprintf(1,'WARNING:\nElement ''%s'' duplicated in cell ''%s''.\n',Name,VS.CelDef(i).Name);
                   else
                       duplicate = '';
                   end
                   if vs_debug
                       fprintf(vs_debug,'Element %2i: %3i (%s)%s\n',k,j,Name,duplicate);
                   end
               end
           end
       end
       
       if ~onefile
           fclose(fidef);
       end
   end
   fclose(fidat);
   
   if vs_debug
      fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
      if onefile
         fprintf(vs_debug,'Successfully finished reading nefis file.\n');
      else
         fprintf(vs_debug,'Successfully finished reading nefis files.\n');
      end
      fprintf(vs_debug,'-------------------------------------------------------\n');
   end
   if showwaitbar
      waitbar(1.0);
   end
   ViewSelect=VS;
   ViewSelect.FileName=filename;
   ViewSelect.DatExt=data_file;
   ViewSelect.DefExt=def_file;
   ViewSelect.Format=Format;
   ViewSelect.AddressType=AddressType;
   ViewSelect.SubType=vs_type(ViewSelect);
   VSKeep=ViewSelect;
   if showwaitbar
      delete(hWaitBar);
   end
   if vs_debug
      fclose(vs_debug);
   end
catch
   if ~isempty(fidat) & ~isempty(fopen(fidat))
      fclose(fidat);
   end
   if ~isempty(fidef) & ~isempty(fopen(fidef))
      fclose(fidef);
   end
   if vs_debug
      fclose(vs_debug);
   end
   if showwaitbar & ishandle(hWaitBar) % delete waitbar if exists
      delete(hWaitBar);
   end
   Str=lasterr;
   if ~strcmp(Str,' ')
      error(lasterr) % error out
   end
   if nargout>0
      VSout=[];
   end
   return
end
if nargout>0
   VSout=ViewSelect;
end


function [NonEmptyBuck,Success,NFail]=ReadHashTableGetNonEmptyBuckets(fidat,AddressType,vs_debug)
Success=1;
NFail=0;
switch AddressType
   case 'uint32'
      Nil=2^32-1;
      AddressBytes=4;
   case 'uint64'
      Nil=2^64-1;
      AddressBytes=8;
end
HashTable=fread(fidat,[1 997],AddressType);         % Read GROUP hash table
if vs_debug
   for i=0:99
      if i==99
         nAddresses=7;
      else
         nAddresses=10;
      end
      sc  = repmat({'       -1'},1,nAddresses);
      HTline = HashTable(i*10+(1:nAddresses));
      sc(HTline~=Nil) = {' %8u'};
      sc{nAddresses+1} = '\n';
      LineFormat = strcat(sc{:});
      fprintf(vs_debug,LineFormat,HTline(HTline~=Nil));
   end
   fprintf(vs_debug,'\n\n');
end
Buck=find(HashTable~=Nil);
NonEmptyBuck=HashTable(Buck)';      % Non-empty buckets of hash table
i=1;
while i<=length(NonEmptyBuck)
   if vs_debug
      fprintf(vs_debug,'Checking link %i to address %u ...',i,NonEmptyBuck(i));
   end
   if fseek(fidat,NonEmptyBuck(i),-1)<0
      if vs_debug
         fprintf(vs_debug,'ERROR\nCannot move to location %u.\n', NonEmptyBuck(i));
      end
      %
      % This used to be a severe error, but I think that we could just continue
      %
      Success=0;
      NFail=NFail+1;
      %
      NonEmptyBuck(i)=[];
      %
      % no increase of i needed
      %
   else
      Link=fread(fidat,[1 1],AddressType);
      if isempty(Link)
         if vs_debug
            fprintf(vs_debug,' ERROR\nCannot read link field.\n');
         end
         fprintf(1,'Cannot read link field at location : %u.\n',NonEmptyBuck(i));
      else
         if vs_debug
            dummy=fread(fidat,1,AddressType); % record size
            dummy=fread(fidat,AddressBytes,'uchar'); % field type
            Label=char(fread(fidat,[1 16],'uchar'));
            Nr=mod(sum(prod(reshape(char(Label)+1,[4 4]))),997)+1;
            if Nr~=Buck(i)
               fprintf(vs_debug,'\nWARNING: Bucket check failed for ''%s''\nRecord expected in bucket %i but encountered in %i.\n',deblank(Label),Nr,Buck(i));
            else
               fprintf(vs_debug,' (Bucket check) OK\n');
            end
         end
         if Link~=Nil % Check buckets on more than one entry
            NonEmptyBuck=[NonEmptyBuck; Link];
            if vs_debug
               fprintf(vs_debug,'New link detected to address %u.\n',Link);
               Buck=[Buck Buck(i)];
            end
         end
      end
      i=i+1;
   end
end
if vs_debug
   if Success
      fprintf(vs_debug,'All links checked successfully.\n\n\n');
   else
      fprintf(vs_debug,'Normal end of link checking, although not all links\nwere checked successfully.\n\n\n');
   end
end


function [fidat,Header,ByteOrder,AddressType,AddressBytes,HeaderLength]=OpenDataFile(data_file,onefile,vs_debug)
if onefile
   HeaderLength=128; % NEFIS defdat file header
else
   HeaderLength=60;  % NEFIS data file header
end
%
fidat = fopen(data_file,'r','b');
if fidat<0
    error('Unable to open file: %s',data_file)
end
Header=char(fread(fidat,[1 HeaderLength],'uchar'));
%
% Possible file sizes
%
eoh = ftell(fidat);
BigE8_Lfile = fread(fidat,1,'uint64','b');
fseek(fidat,eoh,-1);
LitE8_Lfile = fread(fidat,1,'uint64','l');
fseek(fidat,eoh,-1);
BigE4_Lfile = fread(fidat,1,'uint32','b');
fseek(fidat,eoh,-1);
LitE4_Lfile = fread(fidat,1,'uint32','l');
%
fseek(fidat,0,1); % go to EOF
FileSize = ftell(fidat);
%
if vs_debug
   if onefile
      fprintf(vs_debug,'Opened using filehandle: %i.\n\n',fidat);
   else
      fprintf(vs_debug,'Data file opened using filehandle: %i.\n\n',fidat);
   end
   fprintf(vs_debug,'File header:\n%s\n',deblank(Header(1:(end-1))));
   fprintf(vs_debug,'File format indicator    : %s.\n',Header(end));
   fprintf(vs_debug,'Actual file length is      : %u bytes\n\n',FileSize);
   fprintf(vs_debug,'File length big-endian    (8 bytes): %u bytes\n',BigE8_Lfile);
   fprintf(vs_debug,'File length little-endian (8 bytes): %u bytes\n',LitE8_Lfile);
   fprintf(vs_debug,'File length big-endian    (4 bytes): %u bytes\n',BigE4_Lfile);
   fprintf(vs_debug,'File length little-endian (4 bytes): %u bytes\n\n',LitE4_Lfile);
end
%
% Possible hash tables
%
fseek(fidat,eoh+8,-1);
BigE8_Hash = fread(fidat,997,'uint64','b');
fseek(fidat,eoh+8,-1);
LitE8_Hash = fread(fidat,997,'uint64','l');
fseek(fidat,eoh+4,-1);
BigE4_Hash = fread(fidat,997,'uint32','b');
fseek(fidat,eoh+4,-1);
LitE4_Hash = fread(fidat,997,'uint32','l');
fclose(fidat);
%
Miss4 = 2^32-1;
Miss8 = 2^64-1;
opt = zeros(1,4);
opt(1) = all(BigE8_Hash(BigE8_Hash<Miss8)<=FileSize) & length(BigE8_Hash)==997;
opt(2) = all(LitE8_Hash(LitE8_Hash<Miss8)<=FileSize) & length(LitE8_Hash)==997;
opt(3) = all(BigE4_Hash(BigE4_Hash<Miss4)<=FileSize);
opt(4) = all(LitE4_Hash(LitE4_Hash<Miss4)<=FileSize);
if sum(opt)>1
    if vs_debug
       fprintf(vs_debug,'Hash table inconclusive, possible options: \n');
       if opt(1)
           fprintf(vs_debug,'  Big endian    - 8 bytes\n');
       end
       if opt(2)
           fprintf(vs_debug,'  Little endian - 8 bytes\n');
       end
       if opt(3)
           fprintf(vs_debug,'  Big endian    - 4 bytes\n');
       end
       if opt(4)
           fprintf(vs_debug,'  Little endian - 4 bytes\n');
       end
    end
    opt = find(opt);
    opts = opt(1);
    if vs_debug
        fprintf(vs_debug,'  Assuming first option.\n');
    end
elseif sum(opt)==1
    opts = find(opt);
else
    opts = -1;
end
switch opts
    case 1
        AddressType = 'uint64';
        AddressBytes = 8;
        ByteOrder = 'b';
    case 2
        AddressType = 'uint64';
        AddressBytes = 8;
        ByteOrder = 'l';
    case 3
        AddressType = 'uint32';
        AddressBytes = 4;
        ByteOrder = 'b';
    case 4
        AddressType = 'uint32';
        AddressBytes = 4;
        ByteOrder = 'l';
    otherwise
        if vs_debug
            fprintf(vs_debug,'Hash table not valid for any of the formats.\n');
        end
        AddressBytes = -1;
        ByteOrder = '';
end
%
if vs_debug & AddressBytes~=-1 & sum(opt)==1
   switch ByteOrder
      case 'b'
         BO = 'big-endian';
      case 'l'
         BO = 'little-endian';
   end
   fprintf(vs_debug,'Hash table suggests formatting as %s (%i bytes).\n', ...
      BO,AddressBytes);
end
%
if onefile
   if isempty(strfind(lower(Header),'definition and data'))
      msg = 'Header does not include string "definition and data".';
      if vs_debug
         fprintf(vs_debug,msg);
      end
      error(msg)
   end
else
   if isempty(strfind(lower(Header),'data'))
      if isempty(strfind(lower(Header),'defn'))
         msg = 'Trying to open definition file as data file.';
      else
         msg = 'Header does not include string "data".';
      end
      if vs_debug
         fprintf(vs_debug,msg);
      end
      error(msg)
   end
end
%
if ~isempty(strfind(Header,'Versie 1.')) | ~isempty(strfind(Header,'File; Version 4.'))
   if AddressBytes==4
      if vs_debug
         fprintf(vs_debug,'Header version agrees with address size: 4 bytes.\n');
      end
   else
      if vs_debug
         if AddressBytes==-1
            fprintf(vs_debug,'File header version indicates address size: 4 bytes.\n');
         else
            fprintf(vs_debug,'File header version overrules address size: 4 bytes.\n');
         end
      end
      AddressType = 'uint32';
      AddressBytes = 4;
   end
elseif ~isempty(strfind(Header,'File; 5.'))
   if AddressBytes==8
      if vs_debug
         fprintf(vs_debug,'Header version agrees with address size: 8 bytes.\n');
      end
   else
      if vs_debug
         if AddressBytes==-1
            fprintf(vs_debug,'File header version indicates address size: 8 bytes.\n');
         else
            fprintf(vs_debug,'File header version overrules address size: 8 bytes.\n');
         end
      end
      AddressType = 'uint64';
      AddressBytes = 8;
   end
end
%
if AddressBytes == 4
   switch Header(end)
      case 'N'
         if vs_debug
            switch ByteOrder
               case 'l'
                  fprintf(vs_debug,'Format flag ''N'' overrules ordering: big-endian.\n');
               case 'b'
                  fprintf(vs_debug,'Format flag ''N'' agrees with big-endian ordering.\n');
               otherwise
                  fprintf(vs_debug,'Format flag ''N'' indicates big-endian ordering.\n');
            end
         end
         ByteOrder = 'b';
         %
         if FileSize>BigE4_Lfile & vs_debug
            fprintf(vs_debug,'Warning: File (%u) larger than file size stored in file (%u).', ...
               FileSize,BigE4_Lfile);
         end
      otherwise
         % "Binary" storage: can be either big or little endian
         switch ByteOrder
            case 'l'
               if vs_debug
                  fprintf(vs_debug,'Format flag inconclusive, following hash-table check: little-endian.\n');
               end
            case 'b'
               if vs_debug
                  fprintf(vs_debug,'Format flag inconclusive, following hash-table check: big-endian.\n');
               end
            otherwise
               if vs_debug
                  fprintf(vs_debug,'Format flag inconclusive, hash-table check inconclusive.\n');
                  fprintf(vs_debug,'Comparing file sizes ...\n');
               end
               if FileSize==BigE4_Lfile
                  ByteOrder = 'b';
                  if vs_debug
                     fprintf(vs_debug,'File size matches: big-endian value.\n');
                  end
               elseif FileSize==LitE4_Lfile
                  ByteOrder = 'l';
                  if vs_debug
                     fprintf(vs_debug,'File size matches: little-endian\n');
                  end
               elseif abs(FileSize-BigE4_Lfile)<abs(FileSize-LitE4_Lfile)
                  AddressBytes = -1;
                  %ByteOrder = 'b';
                  %if vs_debug
                  %   fprintf(vs_debug,'Most likely byte order: big-endian\n');
                  %   if FileSize>BigE4_Lfile
                  %      fprintf(vs_debug,'Warning: File (%u) larger than file size stored in file (%u).', ...
                  %         FileSize,BigE4_Lfile);
                  %   end
                  %end
               else
                  AddressBytes = -1;
                  %ByteOrder = 'l';
                  %if vs_debug
                  %   fprintf(vs_debug,'Most likely byte order: little-endian\n');
                  %   if FileSize>LitE4_Lfile
                  %      fprintf(vs_debug,'Warning: File (%u) larger than file size stored in file (%u).', ...
                  %         FileSize,LitE4_Lfile);
                  %   end
                  %end
               end
         end
   end
elseif AddressBytes == 8
   switch Header(end)
      case 'L'
         if vs_debug
            switch ByteOrder
               case 'l'
                  fprintf(vs_debug,'Format flag ''L'' agrees with little-endian ordering.\n');
               case 'b'
                  fprintf(vs_debug,'Format flag ''L'' overrules ordering: little-endian.\n');
               otherwise
                  fprintf(vs_debug,'Format flag ''L'' indicates little-endian ordering.\n');
            end
         end
         ByteOrder = 'l';
         %
         if FileSize>LitE8_Lfile & vs_debug
            fprintf(vs_debug,'Warning: File (%u) larger than file size stored in file (%u).', ...
               FileSize,LitE8_Lfile);
         end
      case 'B'
         if vs_debug
            switch ByteOrder
               case 'b'
                  fprintf(vs_debug,'Format flag ''B'' agrees with big-endian ordering.\n');
               case 'l'
                  fprintf(vs_debug,'Format flag ''B'' overrules ordering: big-endian.\n');
               otherwise
                  fprintf(vs_debug,'Format flag ''B'' indicates big-endian ordering.\n');
            end
         end
         ByteOrder = 'b';
         %
         if FileSize>BigE8_Lfile & vs_debug
            fprintf(vs_debug,'Warning: File (%u) larger than file size stored in file (%u).', ...
               FileSize,BigE8_Lfile);
         end
      otherwise
         error('Invalid file format indicator (expected L or B).')
   end
end
if AddressBytes>0
   HeaderLength = HeaderLength+AddressBytes;
   %
   fidat = fopen(data_file,'r',ByteOrder);
   fseek(fidat,HeaderLength,-1); % OK, go back to location after header and file length
else
   fidat = -1;
end


function filename=absolutefile(FN)
filename=FN;
if isunix
   if isempty(filename) | ~(isequal(filename(1),'/') | isequal(filename(1),'~'))
      filename=[pwd '/' filename];
   elseif isequal(filename(1),'~')
      % Matlab doesn't like ~ references
      % if it is the user's home then replace ~ by getenv('HOME')
      % if it is someoneelse's home: get from unix shell replace
      if isequal(filename(2),'/')
         filename=[getenv('HOME') filename(2:end)];
      else
         i=strfind(filename,'/');
         if isempty(i)
            i=length(filename)+1;
         end
         home=filename(1:(i(1)-1));
         [s,home]=unix(['cd ',home,char(10),'pwd']);
         if s>0
            error('Error interpreting: %s.',filename(1:(i(1)-1)))
         else
            filename=[home(1:end-1) filename(i(1):end)];
         end
      end
   end
else % PCWIN
   if (length(filename)<2) | (~isequal(filename(1:2),'\\') & ~isequal(filename(2),':')),
      filename=[pwd '\' filename];
   end
end


function Str2 = transfercase(Str1,Str2)
%TRANSFERCASE tranfer upper/lowercase from argument 1 to argument 2
Str2 = Str2+Str1-lower(Str1);
