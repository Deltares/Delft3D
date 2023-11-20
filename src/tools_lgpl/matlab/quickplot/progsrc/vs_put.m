function [VSNEW,ErrMsg]=vs_put(varargin)
%VS_PUT Write data to a NEFIS file.
%   NewNFStruct = VS_PUT(NFStruct, ...
%      'GroupName',GroupIndex,'ElementName',ElementIndex,Data)
%
%   NewNFStruct = VS_PUT(NFStruct, 'GroupName', [], ...
%      AttributeName1,Value1,AttributeName2,Value2, ...)
%
%   Example 1
%      F = vs_use('trim-xxx.dat','trim-xxx.def');
%      vs_put(F,'map-series',{1:5},'S1',{1:30 1:20},X);
%      % Save the contents of the 5x30x20 matrix X containing the data of
%      % the first 5 time steps (dimension of map-series group) and first
%      % 30x20 values of the water level stored in element S1 of group
%      % map-series.
%
%   Example 2
%      F = vs_use('trim-xxx.dat','trim-xxx.def');
%      vs_put(F,'map-series',[],'MyLabel','Some text','MyValue',123);
%      % Save one character and one integer attribute to the map-series
%      group.
%
%   See also VS_USE, VS_INI, VS_DEF.

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

% if ~all dims of Elm, read -> write
% if ~all dims of Grp, var.grp dim may be extended: based on dim spec!
% if all dims of Grp, var.grp dim may be extended: based on Data!
% size(Data) should match spec/nonspec. sizes (except var.dim.).
% var.dim.size(Data) should equal to dim spec if var.dim spec
% var.dim.size(Data) should be at least current var.dim.size.

ErrMsg='';
defaultVS=0;
INP=varargin;
showwaitbar=isenvironment('MATLAB');
if (length(INP)>0) & isequal(INP{end},'quiet')
   showwaitbar=0;
   INP(end)=[];
end
VS=[];
for i=1:length(INP)
   if isstruct(INP{i})
      VS=INP{i};
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
vs_warning=1;
for i=1:length(INP)
   if isequal(INP{i},'nowarn')
      vs_warning=0;
      INP(i)=[];
      break
   end
end
if ~isstruct(VS)
   defaultVS=1;
   VS=vs_use('lastread');
end

if ~isfield(VS,'AddressType')
   VS.AddressType = 'uint32';
end

if ~isstruct(VS)
   ErrMsg='No NEFIS file specified.';
elseif length(INP)<2
   ErrMsg='Invalid combination input arguments.';
else
   if nargout>0,
      VSNEW=VS;
   end
   GName=INP{1};
   gdat=strmatch(GName,{VS.GrpDat(:).Name},'exact');
   if gdat<0
      ErrMsg=sprintf('Group %s not found.',GName);
      return
   end
   GIndex=INP{2};

   if isequal(GIndex,[]) % NewNFStruct=vs_put(NFStruct, 'GroupName', [], ...
      %                                         AttributeName1,Value1,AttributeName2,Value2, ...)
      [VS,ErrMsg]=ChangeAttributes(VS,gdat,INP(3:end));
   else %                  NewNFStruct=vs_put(NFStruct, ...
      %                                         'GroupName',GroupIndex,'ElementName',ElementIndex,Data)
      %                                         'GroupName','ElementName',ElementIndex,Data)
      %                                         'GroupName',GroupIndex,'ElementName',Data)
      %                                         'GroupName','ElementName',Data)
      if ischar(INP{2})
         GIndex={};
         i=2;
      else
         i=3;
      end
      EName=INP{i};
      if i+1==length(INP) % last input argument is Data
         EIndex={};
         Data=INP{i+1};
      elseif i+2<length(INP) % too many input arguments
         ErrMsg='Too many or invalid input arguments.';
      else
         EIndex=INP{i+1};
         Data=INP{i+2};
      end
      if isempty(ErrMsg)
         [VS,ErrMsg]=WriteData(VS,GName,GIndex,EName,EIndex,Data,vs_debug,vs_warning);
      end
   end
end
if ~isempty(ErrMsg) & nargout<2
   error(ErrMsg)
end
% if the default lastread structure was used update the lastread structure
if defaultVS & isempty(ErrMsg)
   vs_use(VS);
end
if nargout>0
   VSNEW=VS;
end


function [VSNEW,ErrMsg]=ChangeAttributes(VS,gdat,ATTR)
VSNEW=VS;
ErrMsg='';
Attribs=cat(1,cellstr(VS.GrpDat(gdat).IANames), ...
   cellstr(VS.GrpDat(gdat).RANames), ...
   cellstr(VS.GrpDat(gdat).SANames));
if length(ATTR)==0 % if no attributes are specified
   % nothing to do ...
   return
else
   for i=1:2:length(ATTR)-1,
      if ~ischar(ATTR{i}) | size(ATTR{i},1)~=1
         ErrMsg=sprintf('Invalid attribute name %i',(i+1)/2);
         return
      end
      ATTR{i}=deblank(ATTR{i});
      if isempty(ATTR{i})
         ErrMsg=sprintf('Attribute name %i is blank.',(i+1)/2);
         return
      elseif length(ATTR{i})>16
         ErrMsg=sprintf('Attribute name %i (%s)is too long.',(i+1)/2,ATTR{i});
         return
      end
      ati=strmatch(ATTR{i},Attribs,'exact');
      if ~isempty(ati) % attribute with same name exists
         if isempty(ATTR{i+1}) & isnumeric(ATTR{i+1}) % delete attribute
            Attribs{ati}='';
            if ati>10 % string
               VS.GrpDat(gdat).SAValue(ati-10,:)=' ';
            elseif ati>5 % real
               VS.GrpDat(gdat).RAValue(ati-5)=0;
            else % integer
               VS.GrpDat(gdat).IAValue(ati)=0;
            end
         else % assign a new value
            if ischar(ATTR{i+1}) % trying to assign a new string
               if ati<11
                  ErrMsg=sprintf('Cannot assign ''%s'' to numeric attribute %s.',ATTR{i+1},ATTR{i});
                  return
               end
               Str=sprintf('%-16s',ATTR{i+1}); Str=Str(1:16); % string might be truncated!
               VS.GrpDat(gdat).SAValue(ati-10,:)=Str;
            elseif ~isequal(size(ATTR{i+1}),[1 1]) % invalid value
               ErrMsg=sprintf('Invalid attribute value %i.',i);
               return
            elseif ATTR{i+1}~=round(ATTR{i+1}) % floating point
               if ati>10
                  ErrMsg=sprintf('Cannot assign %f to string attribute %s.',ATTR{i+1},ATTR{i});
                  return
               end
               if ati<6
                  ErrMsg=sprintf('Cannot assign %f to integer attribute %s.',ATTR{i+1},ATTR{i});
                  return
               end
               VS.GrpDat(gdat).RAValue(ati-5,:)=ATTR{i+1};
            else % integer
               if ati>10
                  ErrMsg=sprintf('Cannot assign %i to string attribute %s.',ATTR{i+1},ATTR{i});
                  return
               end
               if ati>5
                  VS.GrpDat(gdat).RAValue(ati-5,:)=ATTR{i+1};
               else
                  VS.GrpDat(gdat).IAValue(ati,:)=ATTR{i+1};
               end
            end
         end
      else % create a new attribute
         if ischar(ATTR{i+1}) % trying to assign a new string
            for ati=11:15
               if isempty(Attribs{ati})
                  break
               end
            end
            if ~isempty(Attribs{ati})
               ErrMsg=sprintf('No space for new string attribute %s',ATTR{i});
               return
            end
            Attribs{ati}=ATTR{i};
            Str=sprintf('%-16s',ATTR{i+1}); Str=Str(1:16); % string might be truncated!
            VS.GrpDat(gdat).SAValue(ati-10,:)=Str;
         elseif ~isequal(size(ATTR{i+1}),[1 1]) % invalid value
            ErrMsg=sprintf('Invalid attribute value %i.',i);
            return
         elseif ATTR{i+1}~=round(ATTR{i+1}) % floating point
            for ati=6:10
               if isempty(Attribs{ati})
                  break
               end
            end
            if ~isempty(Attribs{ati})
               ErrMsg=sprintf('No space for new real attribute %s',ATTR{i});
               return
            end
            Attribs{ati}=ATTR{i};
            VS.GrpDat(gdat).RAValue(ati-5,:)=ATTR{i+1};
         else % integer
            for ati=1:5
               if isempty(Attribs{ati})
                  break
               end
            end
            if ~isempty(Attribs{ati})
               ErrMsg=sprintf('No space for new integer attribute %s',ATTR{i});
               return
            end
            Attribs{ati}=ATTR{i};
            VS.GrpDat(gdat).IAValue(ati,:)=ATTR{i+1};
         end
      end
   end
end

%Fill in the names ...
AN=str2mat(Attribs(1:5));
AN(:,size(AN,2)+1:16)=' ';
VS.GrpDat(gdat).IANames=AN;
AN=str2mat(Attribs(6:10));
AN(:,size(AN,2)+1:16)=' ';
VS.GrpDat(gdat).RANames=AN;
AN=str2mat(Attribs(11:15));
AN(:,size(AN,2)+1:16)=' ';
VS.GrpDat(gdat).SANames=AN;

%Everything ready, so let's write it to the file
fid=fopen([VSNEW.FileName VSNEW.DatExt],'r+',VSNEW.Format);
if fid<0,
   ErrMsg='Error opening data file.';
   return
end
switch VS.AddressType
   case 'uint32'
      AddressBytes=4;
   case 'uint64'
      AddressBytes=8;
end
OK=fseek(fid,VS.GrpDat(gdat).Offset+4+2*AddressBytes,-1);
Name=char(fread(fid,[1 16],'uchar'));
DefName=char(fread(fid,[1 16],'uchar'));
if ~isequal(deblank(Name),VS.GrpDat(gdat).Name) | ~isequal(deblank(DefName),VS.GrpDat(gdat).DefName)
   fclose(fid);
   ErrMsg='File information in memory does not match file.';
   return
end
fseek(fid,ftell(fid),-1);
fwrite(fid,transpose(VS.GrpDat(gdat).IANames),'uchar');
fwrite(fid,VS.GrpDat(gdat).IAValue,'int32');
fwrite(fid,transpose(VS.GrpDat(gdat).RANames),'uchar');
fwrite(fid,VS.GrpDat(gdat).RAValue,'float32');
fwrite(fid,transpose(VS.GrpDat(gdat).SANames),'uchar');
fwrite(fid,transpose(VS.GrpDat(gdat).SAValue),'uchar');
fclose(fid);
VSNEW=VS;


function [VSNEW,ErrMsg]=WriteData(VS,gName,gIndex,eName,eIndex,X,vs_debug,vs_warning)
VSNEW=VS;
ErrMsg='';
onefile=strcmp(VS.DefExt,VS.DatExt);
gInfo=vs_disp(VS,gName,[]);
if ~isstruct(gInfo)
   ErrMsg='Invalid group name.';
   return
end
eInfo=vs_disp(VS,gName,eName);
if ~isstruct(eInfo)
   ErrMsg='Invalid element name.';
   return
elseif iscell(gIndex) & isempty(gIndex)
   gIndex(1:gInfo.NDim)={0};
elseif ~iscell(gIndex) | ~isequal(size(gIndex),[1 gInfo.NDim]),
   ErrMsg=sprintf('Group index should be [1 %i] cell array.',gInfo.NDim);
   return
end
for i=1:gInfo.NDim,
   if ~isequal(round(gIndex{i}),gIndex{i})
      ErrMsg='Group indices should be integers.';
      return
   elseif isequal(gIndex{i},0)
      gIndex{i}=1:gInfo.SizeDim(i);
   elseif gInfo.VarDim==i
      if any(gIndex{i}(:)<1),% | any(gIndex{i}>gInfo.SizeDim(i)),
         ErrMsg=sprintf('Group index %i should lie between 1 and %i.',i,gInfo.SizeDim(i));
         return
      end
   else % gInfo.VarDim~=0,
      if any(gIndex{i}(:)<1),
         ErrMsg='Group index should be larger than 1.';
         return
      end
   end
   %  if ~isequal(size(gIndex{i}),[1 1]),
   %    ErrMsg='Multiple group indices not yet supported.';
   %    return
   %  end;
end
if iscell(eIndex) & isempty(eIndex)
   eIndex(1:eInfo.NDim)={0};
elseif ~iscell(eIndex) | ~isequal(size(eIndex),[1 eInfo.NDim])
   ErrMsg=sprintf('Element index should be [1 %i] cell array.',eInfo.NDim);
   return
end
for i=1:eInfo.NDim
   if ~isequal(round(eIndex{i}),eIndex{i}),
      ErrMsg='Element indices should be integers.';
      return
   elseif isequal(eIndex{i},0)
      eIndex{i}=1:eInfo.SizeDim(i);
   elseif any(eIndex{i}(:)<1) | any(eIndex{i}>eInfo.SizeDim(i)),
      ErrMsg=sprintf('Element index %i should lie between 1 and %i.',i,eInfo.SizeDim(i));
      return
   end
end

id=strmatch(gInfo.DefName,{VS.GrpDef.Name},'exact');
% index of group definition
ic=VS.GrpDef(id).CelIndex;    % index of cell definition
ElmsInCell=VS.CelDef(ic).Elm; % elements in cell

TotSize=0;
for i=ElmsInCell
   TotSize=TotSize+VS.ElmDef(i).SizeElm;
end
TotSize=TotSize*prod(max(1,VS.GrpDef(id).SizeDim));

% currently only one cell and the whole element are selected
jn=0;
jnm=1;
i=strmatch(gName,{VS.GrpDat(:).Name},'exact');
j=strmatch(eName,{VS.ElmDef(:).Name},'exact');
eName={eName};
eIndex={eIndex};
showwaitbar=0;
write='write';
Writing='Writing';
Fwrite='fwrite';
written='written';
READ=0;
X={eName{1};X};

% count the number of group indices to be returned
gDimen=gInfo.SizeDim;
for k=length(gIndex):-1:1
   % remove double values and sort
   gIndex{k}=unique(gIndex{k});
   if (max(gIndex{k})>gDimen(k)) & (k~=gInfo.VarDim)
      ErrMsg=sprintf('Maximum (%i) group index %i larger than actual dimension (%i).',max(gIndex{k}),k,gDimen(k));
      return
   end
   gNIndex(k)=length(gIndex{k});
end
ReadNGrp=prod(gNIndex);

% count the number of element entries to be returned
FullElm=1;
for j1=1:jnm
   eDimen{j1}=VS.ElmDef(j(j1)).Size;
   eNIndex{j1}=[];
   for k=length(eIndex{j1}):-1:1
      % remove double values and sort
      eIndex{j1}{k}=unique(eIndex{j1}{k});
      if max(eIndex{j1}{k})>eDimen{j1}(k)
         ErrMsg=sprintf('Maximum (%i) element index %i larger than actual dimension (%i).',max(eIndex{j1}{k}),k,eDimen{j1}(k));
         return
      end
      eNIndex{j1}(k)=length(eIndex{j1}{k});
   end
   if isempty(eNIndex{j1})
      eNIndex{j1}=0;
   end
   if isempty(eDimen{j1})
      eSize(j1)=0;
   else
      eSize(j1)=prod(eDimen{j1});
   end
   if eInfo.TypeVal==1,
      NValPerElm=eInfo.NByteVal;
   else
      NValPerElm=[];
   end
   % determine scalar index for element selection
   % match data dimensions ignoring unit dimensions
   szX=size(X{2,j1});
   szXreq=stripsize(cat(2,gNIndex,eNIndex{j1},NValPerElm));
   if gInfo.VarDim
      if length(szX)>=gInfo.VarDim
         szXreq(gInfo.VarDim)=szX(gInfo.VarDim);
      end
   end
   if ~isequal(szX,szXreq)
      if isequal(szX(szX~=1),szXreq(szXreq~=1))
         X{2,j1}=reshape(X{2,j1},szXreq);
      elseif gInfo.VarDim
         szXreq(gInfo.VarDim)=1;
         if isequal(szX(szX~=1),szXreq(szXreq~=1))
            X{2,j1}=reshape(X{2,j1},szXreq);
         end
      end
   end
   szX=size(X{2,j1});
   if ~isequal(szX,szXreq)
      ErrMsg1=deblank(sprintf('%i ',szX));
      ErrMsg2=deblank(sprintf('%i ',szXreq));
      ErrMsg=sprintf('Size of write data (%s) does not match indicated indices (%s).',ErrMsg1,ErrMsg2);
      return
   elseif ~isequal(eNIndex{j1},eDimen{j1})
      FullElm=0;
      Temp=zeros(eDimen{j1});
      Temp(eIndex{j1}{:})=1;
      eIndex1{j1}=find(Temp(:));
   else
      eIndex1{j1}=1:eSize(j1);
   end
end

P256=[1 256 256^2 256^3];
AddressType = VS.AddressType;
switch AddressType
   case 'uint32'
      AddressBytes=4;
      Nil=2^32-1;
   case 'uint64'
      AddressBytes=8;
      Nil=2^64-1;
end
VarDimUpdate=0;
try
   if vs_debug
      vs_debug=fopen([tempdir 'vs_put.dbg'],'w');
      if vs_debug<=0
         vs_debug=0;
         if vs_warning,
            warning(sprintf('Cannot open debug file: %svs_put.dbg.',tempdir));
         end
      else
         fprintf(1,'Writing to debug file %i: %svs_put.dbg ...\n',vs_debug,tempdir);
      end
   end

   fidat=0; % make sure that the file handle is defined in case of a crash

   if showwaitbar
      if jn>0  % all elements
         hWaitBar = waitbar(0,sprintf('%s %s. Please wait ...',Writing,gName));
      else % one element
         hWaitBar = waitbar(0,sprintf('%s %s. Please wait ...',Writing,eName{1}));
      end
      set(hWaitBar,'closerequestfcn','');
   end

   % open data file
   data_file=[VS.FileName,VS.DatExt];
   fidat=fopen(data_file,'r+',VS.Format);

   if fidat<0
      error('Cannot not open file: %s.',data_file)
   end

   if vs_debug
      fprintf(vs_debug,'Data file: %s\n',data_file);
      fprintf(vs_debug,'Opened using filehandle: %i.\n\n',fidat);
      fprintf(vs_debug,'%s ...\nGroup    %i              : %s.\n',Writing,i,deblank(gName));
      for k=1:length(gIndex)
         fprintf(vs_debug,'  Index  %i (max:%6i) : %s\n',k,gDimen(k),Local_vec2str(gIndex{k}));
      end
      for j1=1:jnm
         fprintf(vs_debug,'Element %2i              : %s.\n',j1,deblank(eName{j1}));
         for k=1:length(eIndex{j1})
            fprintf(vs_debug,'  Index  %i              : %s\n',k,Local_vec2str(eIndex{j1}{k}));
         end
      end
   end

   % determine whether there is a variable dimension
   VD=gInfo.VarDim;
   % determine the number of variable dimension
   if VD
      if isempty(gIndex{VD})
         gIndex{VD}=1:szX(VD);
      end
      VDIndex=gIndex{VD};
      gDimen(VD)=1;
      VDOffset=prod(gNIndex(1:(VD-1)));
      gIndex{VD}=1;
      VarDimCntMax=length(VDIndex);
      if vs_debug
         fprintf(vs_debug,'This group has a variable dimension (%i) of size %i.\n',VD,VarDimCntMax);
      end
   else
      VDOffset=0;
      VarDimCntMax=1;
      if vs_debug
         fprintf(vs_debug,'This group has no variable dimension.\n');
      end
   end

   % get the size and ordering of the dimensions in the data file
   gOrder=gInfo.OrderDim;

   % reorder the dimensions for data acces
   if VD % make sure that VD comes last
      if gOrder(VD)~=1
         gOrder(VD)=max(gOrder)+1;
      end
      VDOffset=1;
   end
   [gOrder,Reorder]=sort(gOrder);          % Reorder
   gDimen=gDimen(Reorder);
   gIndex=gIndex(Reorder);
   gNIndex=gNIndex(Reorder);
   %[Dummy,UnReorder]=sort(Reorder);
   % permute data if necessary
   if ~isequal(Reorder,1:length(Reorder))
      if vs_debug
         fprintf(vs_debug,'Reordering data / permuting data dimensions ...\n');
      end
      for j1=1:jnm
         X{2,j1}=permute(X{2,j1},[Reorder length(Reorder)+1:ndims(X{2,j1})]);
      end
   end

   % precompute element offset within cell
   if jn==0 % one element
      ElmOffset=0;
      for k=ElmsInCell,
         if k==j
            break
         end                % Sum element sizes until loadable element is encountered
         ElmOffset=ElmOffset+VS.ElmDef(k).SizeElm;
      end
      if vs_debug
         fprintf(vs_debug,'Element offset within data set : %i\n\n',ElmOffset);
      end
      ElmOffsetCopy=ElmOffset;              % Save copy for reading of multiple groups in group optimized mode
   else % all elements
      ElmOffset=zeros(1,jnm);
      for j1=1:jnm-1
         ElmOffset(j1+1)=ElmOffset(j1)+VS.ElmDef(ElmsInCell(j1)).SizeElm;
      end
      if vs_debug
         fprintf(vs_debug,'Element offsets within data set:\n');
         fprintf(vs_debug,'   %7i\n',ElmOffset);
         fprintf(vs_debug,'\n');
      end
      ElmOffsetCopy=ElmOffset;              % Save copy for reading of multiple groups in group optimized mode
      ElmOffset=0;
   end

   % preload NBytesPerCell, DataType, ElmBytes, ValBytes
   NBytesPerCell=sum([VS.ElmDef(ElmsInCell).SizeElm]);
   DataType=[VS.ElmDef(j).Type];
   ElmBytes=[VS.ElmDef(j).SizeElm];
   ValBytes=[VS.ElmDef(j).SizeVal];
   RdType=cell(1,jnm);
   for j1=1:jnm
      switch DataType(j1),
         case 1     % CHARACTE
            RdType{j1}='uchar';
         case 2 % COMPLEX
            switch ValBytes(j1),
               case 8 % number of bytes equals 8 (4+4)
                  RdType{j1}='float32';
               case 16 % number of bytes equals 16 (8+8)
                  RdType{j1}='float64';
               otherwise
                  error('Unable to %s COMPLEX data.',write)
            end
         case 3 % INTEGER
            switch ValBytes(j1)
               case 4 % number of bytes equals 4
                  RdType{j1}='int32';
               case 2 % number of bytes equals 2
                  RdType{j1}='int16';
               otherwise
                  error('Unable to %s INTEGER data.',write)
            end
         case 4 % LOGICAL
            switch ValBytes(j1)
               case 4 % number of bytes equals 4
                  RdType{j1}='int32';
               case 2 % number of bytes equals 2
                  RdType{j1}='int16';
               otherwise
                  error('Unable to %s LOGICAL data.',write)
            end
         case 5 % REAL
            switch ValBytes(j1)
               case 4 % number of bytes equals 4
                  RdType{j1}='float32';
               case 8 % number of bytes equals 8
                  RdType{j1}='float64';
               otherwise
                  error('Unable to %s REAL data.',write)
            end
         otherwise
            Str=sprintf(1,'Unexpected type number %i for %s.\n',DataType(j1),VS.ElmDef(j(j1)).Name);
            error(Str);
      end
      %Initialize dimension for reading ...
      switch DataType(j1)
         case 1     % CHARACTE
            rDimen{j1}=[ValBytes(j1) eSize(j1)];
         case 2
            rDimen{j1}=[2 eSize(j1)];
         case {3,4,5}
            rDimen{j1}=[1 eSize(j1)];
      end
   end

   if vs_debug
      for j1=1:jnm
         fprintf(vs_debug,'Element %i (%s) contains ',j1,deblank(eName{j1}));
         if rDimen{j1}(2)==1
            fprintf(vs_debug,'1 value of type: %s',RdType{j1});
         else
            fprintf(vs_debug,'%i values of type: %s',rDimen{j1}(2),RdType{j1});
         end
         if ValBytes(j1)==1
            fprintf(vs_debug,' (1 byte)\n');
         else
            fprintf(vs_debug,' (%i bytes)\n',ValBytes(j1));
         end
      end
   end

   %Determine wether all group dimensions related to cell
   %are completely read/written.
   ReadAllGrpCellDims=1;
   for k=1:length(gIndex)
      SZgIndex(1,k)=length(gIndex{k});
      if SZgIndex(k)~=gDimen(k)
         ReadAllGrpCellDims=0;
      end % SZgIndex(k) can only be gDimen(k) if gIndex{k}=1:gDimen(k) since all doubles were removed
   end

   NRCells=prod(SZgIndex);
   GroupOptimized = ReadAllGrpCellDims & (NRCells>1);
   % if not all cells are read/written, then no group optimization possible yet
   % if just one cell then no group optimization necessary (and if jn>0 undesirable: extra fseek commands)
   if GroupOptimized
      CelOffset=0;            % dummy offset
      CelSkip=0;              % dummy skip
      %NRCells=prod(SZgIndex);
      NRCellsPerRead=NRCells; % all cells (per VD) will be read at once
      if vs_debug
         fprintf(vs_debug,'Group optimized %s used.\n',lower(Writing));
         if NRCellsPerRead>1
            fprintf(vs_debug,'%s %i cells per %s statement.\n',Writing,Fwrite);
         else
            fprintf(vs_debug,'%s 1 cell per %s statement.\n',Writing,Fwrite);
         end
      end

      % Change RdType and rDimen to include cell dimensions
      for j1=1:jnm
         RdType{j1}=sprintf('%i*%s',prod(rDimen{j1}),RdType{j1});
         rDimen{j1}(2)=rDimen{j1}(2)*NRCells;
      end
      if vs_debug
         for j1=1:jnm
            fprintf(vs_debug,'%s format adjusted to: %ix%i values using format %s for %s.\n',Writing,rDimen{j1},RdType{j1},X{1,j1});
         end
         fprintf(vs_debug,'\n');
      end
   else
      CPfw=cumprod([1 SZgIndex]);
      CPbw=fliplr(cumprod([1 fliplr(SZgIndex)]));
      CelOffset=zeros(prod(SZgIndex),length(gIndex));
      for k=1:length(gIndex)
         CelOffset(:,k)=repmat(reshape(repmat(gIndex{k},CPfw(k),1),SZgIndex(k)*CPfw(k),1),CPbw(k+1),1);
      end
      SkipCellMask=[1 cumprod(gDimen(1:(end-1)))]*NBytesPerCell;
      CelOffset=(CelOffset-1)*(transpose(SkipCellMask));
      NRCells=length(CelOffset); % count the number of cells that will be read/write (per VD)
      NRCellsPerRead=1;          % one cell per fread/fwrite statement

      if vs_debug
         fprintf(vs_debug,'Element optimized %s used.\n',lower(Writing));
         if VD
            fprintf(vs_debug,'Per variable dimension index ');
         end
         if NRCells==1
            fprintf(vs_debug,'1 cell will be %s.\nThe offset of this cell within the group is ',written);
         else
            fprintf(vs_debug,'%i cells will be %s.\nThe offsets of these cells within the group are:\n',NRCells,written);
         end
         fprintf(vs_debug,[repmat(' %7i',[1 10]) '\n'],CelOffset);
         fprintf(vs_debug,'\n\n');
      end

      % determine the number of bytes to skip between fread/fwrite statements
      if jn==0 % one element
         CelSkip=[0; diff(CelOffset)-ElmBytes(1)]; % prod(eDimen{1})*ValBytes(1)
      else % all elements
         % approximation only, since exact number of bytes varies per element.
         CelSkip=[0; diff(CelOffset)-NBytesPerCell];
      end
   end

   if showwaitbar
      Alpha2=NRCells*VarDimCntMax;
      Alpha3=0.02*Alpha2;
      if Alpha2>0
         Alpha2=1/Alpha2;
      end
      Alpha4=Alpha3;
   end

   % determine the appropriate offset where the cell data is stored
   % default the data record follows GROUP record in data file
   Offset=VS.GrpDat(i).Offset+392+3*AddressBytes;
   % for each value of the variable dimension
   % if there is no variable dimension, VarDimCntMax should have been set to 1
   LastVD=-ones(1,4);
   if VD
      PntListOffset=zeros(1,4);
      PntListOffset(4)=Offset+AddressBytes;
      fseek(fidat,PntListOffset(4),-1);     % Start of first pointer table
      PointerList(4,:)=fread(fidat,[1 256],AddressType);
      if vs_debug
         fprintf(vs_debug,'Pointer list 4 at offset %u:\n',PntListOffset(4));
         fprintf(vs_debug,[repmat(' %7u',[1 10]) '\n'],PointerList(4,:));
         fprintf(vs_debug,'\n\n');
      end
   end
   for VarDimCnt=1:VarDimCntMax
      % If there is a variable dimension adapt the offset
      if VD
         if vs_debug
            fprintf(vs_debug,'Determine offset of data group\nfor variable dimension index %i (%i of %i) ...\n',VDIndex(VarDimCnt),VarDimCnt,VarDimCntMax);
         end
         Temp=VDIndex(VarDimCnt);
         VDByte=floor(Temp./P256);
         VDByte=VDByte-256*floor(VDByte/256);
         if vs_debug
            fprintf(vs_debug,'Byte representation of %i : %i %i %i %i\n',VDIndex(VarDimCnt),VDByte);
         end
         for bt=[4 3 2]
            if LastVD(bt)~=VDByte(bt)
               bt1=bt-1;
               PntListOffset(bt1)=PointerList(bt,VDByte(bt)+1);
               if PntListOffset(bt1)==Nil
                  PointerList(bt1,:)=repmat(Nil,1,256);
               else
                  fseek(fidat,PntListOffset(bt1),-1);
                  PointerList(bt1,:)=fread(fidat,[1 256],AddressType);
               end
               LastVD(bt)=VDByte(bt); % copy VDByte into LastVD
               LastVD(bt1)=Nil;       % make sure that PointerList(bt-2) is reloaded
               if vs_debug
                  if PntListOffset(bt1)==Nil
                     fprintf(vs_debug,'Pointer list %i unavailable: substituted by all minus ones.\n',bt1);
                  else
                     fprintf(vs_debug,'Pointer list %i at offset %u:\n',bt1,PntListOffset(bt1));
                     fprintf(vs_debug,[repmat(' %7u',[1 10]) '\n'],PointerList(bt1,:));
                  end
                  fprintf(vs_debug,'\n\n');
               end
            end
         end
         Offset=PointerList(1,VDByte(1)+1);
         if vs_debug
            fprintf(vs_debug,'The offset of data group %i is %i.\n',VDIndex(VarDimCnt),Offset);
         end
         if ~READ & (Offset==Nil)
            if vs_debug
               fprintf(vs_debug,'Rechecking pointers ...\n\n');
            end
            for bt=[4 3 2]
               bt1=bt-1;
               PntListOffset(bt1)=PointerList(bt,VDByte(bt)+1);
               if PntListOffset(bt1)==Nil
                  OK=fseek(fidat,0,1); % goto end of file
                  if OK<0
                     error('Trying to allocate space for writing... error jumping to end of file.');
                  end
                  PntListOffset(bt1)=ftell(fidat);
                  if PntListOffset(bt1)>=Nil
                     error('Trying to allocate space for writing... error file too large!');
                  end
                  fwrite(fidat,-ones(1,256),AddressType(2:end));
                  PointerList(bt,VDByte(bt)+1)=PntListOffset(bt1);
                  fseek(fidat,PntListOffset(bt),-1);
                  fwrite(fidat,PointerList(bt,:),AddressType);
                  if vs_debug
                     fprintf(vs_debug,'Pointer list %i value %i changed to %u.\n',bt,VDByte(bt)+1,PntListOffset(bt1));
                     fprintf(vs_debug,'Pointer list %i created at offset %u.\n\n',bt1,PntListOffset(bt1));
                  end
               else
                  fseek(fidat,PntListOffset(bt1),-1);
                  PointerList(bt1,:)=fread(fidat,[1 256],AddressType);
                  if vs_debug
                     fprintf(vs_debug,'Pointer list %i at offset %u:\n',bt1,PntListOffset(bt1));
                     fprintf(vs_debug,[repmat(' %7u',[1 10]) '\n'],PointerList(bt1,:));
                     fprintf(vs_debug,'\n\n');
                  end
               end
            end
            Offset=PointerList(1,VDByte(1)+1);
            if Offset==Nil
               OK=fseek(fidat,0,1); % goto end of file
               if OK<0
                  error('Trying to allocate space for writing... error jumping to end of file.');
               end
               Offset=ftell(fidat);
               if Offset>=Nil
                  error('Trying to allocate space for writing... error file too large!.');
               end
               N=fwrite(fidat,0,'int8',TotSize-1);
               if N~=1
                  error('Trying to allocate space for writing... error adding space at end of file.');
               end
               PointerList(1,VDByte(1)+1)=Offset;
               fseek(fidat,PntListOffset(1),-1);
               fwrite(fidat,PointerList(1,:),AddressType);
               VarDimUpdate=1;
               if vs_debug
                  fprintf(vs_debug,'Pointer list %i value %i changed to %u.\n',bt,VDByte(bt)+1,PntListOffset(bt1));
                  fprintf(vs_debug,'%i bytes of space allocated at offset %u.\n\n',N,Offset);
               end
            else
               % Miraculously we have found the requested pointer!
               if vs_debug
                  fprintf(vs_debug,'WARNING: Second attempt in finding the offset succeeded!');
                  fprintf(vs_debug,'The offset of data group %i is %u.\n',VDIndex(VarDimCnt),Offset);
               end
            end
         end
      end
      if GroupOptimized
         VDCellOffset=(VarDimCnt-1)*NRCellsPerRead;
      else
         VDCellOffset=(VarDimCnt-1)*VDOffset;
      end

      Alpha1=(VarDimCnt-1)*NRCells;

      if VD & (Offset==Nil) & READ
         % reading from a pointer that has not been set is not possible
         % zeros returned by default
      else
         if GroupOptimized
            status=0;
         else
            if jn>0
               % one large jump to start of first element
               status=fseek(fidat,Offset+CelOffset(1),-1);
               if vs_debug
                  fprintf(vs_debug,'  Jumping to start of first element at %i.\n',Offset+CelOffset(1));
               end
            else
               % one large jump directly to start of element
               status=fseek(fidat,Offset+CelOffset(1)+ElmOffset,-1);
               if vs_debug
                  fprintf(vs_debug,'  Jumping to start of element at %i.\n',Offset+CelOffset(1)+ElmOffset);
               end
            end
         end
         if status<0
            if vs_warning
               warning(Message(0,vs_debug, ...
                  VarDimCnt,Offset,NBytesPerCell,fidat));
            end
         else
            for Cell=1:length(CelOffset)
               if GroupOptimized
                  IdxCell=1:NRCellsPerRead;
               else
                  IdxCell=Cell;
               end
               if showwaitbar % can be placed outside Cell loop to speed up reading of small groups/cells
                  NewRefresh=Alpha1+Cell; % <-- in that case replace Cell by NrCells
                  if NewRefresh>Alpha4
                     waitbar(NewRefresh*Alpha2);
                     Alpha4=NewRefresh+Alpha3;
                  end
               end

               % Go to the appropriate offset for reading
               if CelSkip(Cell) %>0
                  if vs_debug
                     fprintf(vs_debug,'Skipping %i bytes ...\n',CelSkip(Cell));
                  end
                  %fread(fidat,CelSkip(Cell),'int8'); % <-- ruins data!? testcase:
                  %  Nfs=vs_put(Nfs,'F_SURFACE',{0 0 1},'Fij',ones(50,20,1),'debug')
                  %  Nfs=vs_put(Nfs,'F_SURFACE',{11:15 11:15 1},'Fij',zeros(5,5,1),'debug')
                  %  when read, the matrix contains NaN for the skipped values!
                  fseek(fidat,CelSkip(Cell),0);
               end

               for j1=1:jnm
                  p=eSize(j1);
                  if GroupOptimized
                     status=fseek(fidat,Offset+ElmOffsetCopy(j1),-1);
                     if vs_debug
                        fprintf(vs_debug,'  Jumping to start of element %i in cell 1 at %u.\n',j1,ftell(fidat));
                     end
                  else
                     status=1;
                  end
                  % Read data into Temp
                  Temp=[];
                  if READ | ~FullElm
                     if status<0
                        warning(Message(0,vs_debug, ...
                           VarDimCnt,Offset,NBytesPerCell,fidat));
                        Temp=[];
                        NRead=0;
                     else
                        if vs_debug
                           fprintf(vs_debug,'  Reading %s data at: %u.\n',RdType{j1},ftell(fidat));
                        end
                        if ~READ
                           RdOffset=ftell(fidat);
                        end
                        if GroupOptimized
                           [Temp,NRead]=fread(fidat,rDimen{j1},RdType{j1},NBytesPerCell-ElmBytes(j1));
                        else
                           [Temp,NRead]=fread(fidat,rDimen{j1},RdType{j1});
                        end
                        if vs_debug
                           fprintf(vs_debug,'  Reading ended at %u.\n',ftell(fidat));
                        end
                     end
                  end
                  % Extract data to be written from X into Temp
                  if ~READ & ~isempty(X{2,j1})
                     if ~FullElm
                        % Check number of values read
                        if NRead~=prod(rDimen{j1}), %prod(size(Temp))~=prod(rDimen{j1})
                           error('Insufficient data while reading before write')
                        end
                     end
                     switch DataType(j1)
                        case 1     % CHARACTE
                           NValPerElm=rDimen{j1}(1);
                        case 2 % COMPLEX
                           NValPerElm=1;
                        case {3,4,5} % INTEGER, LOGICAL, REAL
                           NValPerElm=1;
                     end
                     if p*NValPerElm==1
                        Temp=X{2,j1}(VDCellOffset+IdxCell);
                     elseif NValPerElm*NRCellsPerRead==1
                        Temp(1,eIndex1{j1})=X{2,j1}(VDCellOffset+IdxCell,:);
                     else
                        T2=X{2,j1}(VDCellOffset+IdxCell,:);
                        T2=reshape(T2,[NRCellsPerRead p NValPerElm]);
                        T2=permute(T2,[3 2 1]); %Characters ElmDims{1:5} Cells
                        if ~FullElm,
                           Temp=reshape(Temp,[NValPerElm p NRCellsPerRead]);
                           Temp(:,eIndex1{j1},:)=T2;
                        else
                           Temp=T2;
                        end
                     end
                     switch DataType(j1)
                        case 2 % COMPLEX
                           Temp(2,:)=imag(Temp(1,:));
                           Temp(1,:)=real(Temp(1,:));
                     end
                  end

                  if ~FullElm
                     status=fseek(fidat,RdOffset,-1);
                     if vs_debug
                        fprintf(vs_debug,'  Jumping back start of element %i at %u.\n',j1,RdOffset);
                        if status>=0
                           fprintf(vs_debug,'  Writing %s data ...\n',RdType{j1});
                        end
                     end
                     if status<0
                        error(Message(0,vs_debug, ...
                           VarDimCnt,Offset,NBytesPerCell,fidat));
                     end
                  end
                  if GroupOptimized
                     if vs_debug
                        fprintf(vs_debug,'  Jumping back %i bytes before fwrite with skip.\n',NBytesPerCell-ElmBytes(j1));
                     end
                     fseek(fidat,-NBytesPerCell+ElmBytes(j1),0); % fwrite skips and writes, fread reads and skips
                     NRead=fwrite(fidat,Temp,RdType{j1},NBytesPerCell-ElmBytes(j1));
                  else
                     NRead=fwrite(fidat,Temp,RdType{j1});
                  end
                  if vs_debug
                     fprintf(vs_debug,'  Writing ended at %u.\n',ftell(fidat));
                  end
                  % Check number of values read/written
                  if NRead~=prod(rDimen{j1}) %prod(size(Temp))~=prod(rDimen{j1})
                     error('Not all %i values could be written to the file.',prod(rDimen{j1}))
                  end
               end
            end
         end
         if vs_debug
            fprintf(vs_debug,'\n');
         end
      end
   end
   if vs_debug
      fprintf(vs_debug,'Updating file size ...\n');
   end
   try
      UpdateFileSize(VS,fidat,onefile)
   catch
      if vs_debug
         fprintf(vs_debug,'Update failed !\n');
      end
   end
   if vs_debug
      fprintf(vs_debug,'\ndone.\n');
   end
   fclose(fidat);
   if showwaitbar
      delete(hWaitBar);
      drawnow;
   end
   Succes=1;
   if vs_debug
      fprintf(vs_debug,'\n\n-------------------------------------------------------\n');
      fprintf(vs_debug,'Successfully finished %s nefis file.\n',lower(Writing));
      fprintf(vs_debug,'-------------------------------------------------------\n');
   end
   if vs_debug
      fclose(vs_debug);
   end
catch
   try
      UpdateFileSize(VS,fidat,onefile)
   catch
   end
   if vs_debug
      fclose(vs_debug);
   end
   if fidat>0 % close file if open
      fclose(fidat);
   end
   if showwaitbar & ishandle(hWaitBar) % delete waitbar if exists
      delete(hWaitBar);
   end
   error(lasterr); % error out
end
if ~READ & VarDimUpdate
   VSNEW.GrpDat(i).SizeDim(VD)=max(max(VDIndex),VSNEW.GrpDat(i).SizeDim(VD));
end


function Str=Local_vec2str(OrigVec)
% VEC2STR creates a string of the vector
if isempty(OrigVec)
   Str='[]';
elseif length(OrigVec)==1
   Str=sprintf('[ %g ]',OrigVec);
else
   FiniteVec=OrigVec;
   FiniteVec(~isfinite(FiniteVec))=NaN;
   % handle finite values
   B=diff([FiniteVec FiniteVec(end)]);
   C=abs((B(1:end-1)-B(2:end))./max(max(B(2:end),B(1:end-1)),eps))>1e-5 | isnan(B(1:end-1));
   D=[1 find(C)+1];
   E=[FiniteVec(D);B(D);FiniteVec(D)+B(D).*(diff([D length(FiniteVec)+1])-1);diff([D length(FiniteVec)+1])];
   F=find((abs((E(1,2:end)-E(3,1:end-1)-E(2,1:end-1)))<1e-5) & (E(4,2:end)==1) & (E(4,1:end-1)~=1));
   E(3,F)=E(1,1+F);
   E(4,F)=E(4,F)+1;
   E(:,1+F)=[];
   F=find((abs((E(1,2:end)-E(3,1:end-1)-E(2,1:end-1)))<1e-5) & (E(4,2:end)==2));
   E(3,F)=E(1,1+F);
   E(4,F)=E(4,F)+1;
   E(4,1+F)=E(4,1+F)-1;
   E(1,1+F)=E(3,1+F);
   % handle NaNs and infinites
   E(1,~isfinite(E(1,:)))=OrigVec(~isfinite(OrigVec));
   W=E(1,:);
   W(isfinite(W))=0;
   W(isnan(W))=1;
   W(W==inf)=2;
   W(W==-inf)=3;
   lengthW=diff(find(diff([-1 W -1])));
   startW=cumsum([1 lengthW(1:end-1)]);
   valueW=W(startW);
   startW=startW(valueW>0);
   lengthW=lengthW(valueW>0);
   E(4,startW)=lengthW;
   E(:,setdiff(find(W),startW))=[];
   E(2,~isfinite(E(1,:)))=0;
   E(3,~isfinite(E(1,:)))=E(1,~isfinite(E(1,:)));
   % create string
   Str='[';
   for i=1:size(E,2)
      if E(4,i)==1
         Str=[Str sprintf(' %g',E(1,i))];
      elseif E(4,i)==2
         Str=[Str sprintf(' %g %g',E([1 3],i))];
      elseif (E(2,i)==0) | isnan(E(2,i))
         if E(4,i)>3
            if E(1,i)==0
               Str=[Str sprintf(' zeros(1,%i)',E(4,i))];
            else
               Str=[Str sprintf(' %g*ones(1,%i)',E(1,i),E(4,i))];
            end
         else
            Str=[Str sprintf(' %g',E(1,i)*ones(1,E(4,i)))];
         end
      else
         if E(2,i)==1
            Str=[Str sprintf(' %g:%g',E([1 3],i))];
         else
            Str=[Str sprintf(' %g:%g:%g',E(1:3,i))];
         end
      end
   end
   Str=[Str ' ]'];
end

function Str=Message(id,vs_debug,varargin)
switch id
   case 0
      VarDimCnt=varargin{1};
      Offset=varargin{2};
      NBytesPerCell=varargin{3};
      fidat=varargin{4};
      Loc=ftell(fidat);
      fseek(fidat,0,1);
      Str=sprintf( ...
         ['Trying to read beyond end of file:\n', ...
         'Data of cell %i starts at address %u.\n', ...
         'The cell size is %u.\n', ...
         'The file length is %u.\n'], ...
         VarDimCnt,Offset,NBytesPerCell,ftell(fidat));
      fseek(fidat,Loc,-1);
   case 1
      eName=deblank(varargin{1});
      gName=deblank(varargin{2});
      VarDimCnt=varargin{3};
      Offset=varargin{4};
      NBytesPerCell=varargin{5};
      fidat=varargin{6};
      Loc=ftell(fidat);
      fseek(fidat,0,1);
      Str=sprintf( ...
         ['Out of data, while reading:\n', ...
         'Element %s of group %s:\n', ...
         'Data of cell %i starts at address %u.\n', ...
         'The cell size is %u.\n', ...
         'The file length is %u.\n'], ...
         eName,gName,VarDimCnt,Offset,NBytesPerCell,ftell(fidat));
      fseek(fidat,Loc,-1);
end
if vs_debug
   fprintf(vs_debug,Str);
end

function V=stripsize(W)
wi=max(find(W~=1));
if ~isempty(wi)
   V=W(1:wi);
else
   V=[];
end
if length(V)==0,
   V=[1 1];
elseif length(V)==1,
   V=[V 1];
end

function UpdateFileSize(VS,fid,onefile) % update filesize
fseek(fid,0,1); % move to eof
FSize=ftell(fid);
if onefile
   fseek(fid,128,-1);
else
   fseek(fid,60,-1);
end
fwrite(fid,FSize,VS.AddressType); % update file size, unsigned address space
