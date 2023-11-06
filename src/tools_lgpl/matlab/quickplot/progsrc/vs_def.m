function [VSNEW,ErrMsg]=vs_def(varargin)
%VS_DEF Changes the groups, cells and element definitions.
%   DataInfoOut = VS_DEF(DataInfo,Type,Name, ...) adds a new element, cell,
%   group definition or data group as indicated by the Type:
%   'elm','cell','grp','data' with the specified Name.
%
%   The further arguments depend on the data type:
%    * elm : DataType, NBytesPerValue, Size, QuantityName, UnitsName, Description
%      The last three arguments are optional; if not specified then they
%      default to the empty string. The data types can be specified either
%      by their name (character, complex, integer, logical, real) or by
%      the typenumber (1--5 for above mentioned types). Valid values of
%      NBytesPerValue are:
%                    character : any larger than zero
%                    complex   : 8, 16
%                    integer   : 4, 8
%                    logical   : 4, 8
%                    real      : 4, 8
%    * cell: ElementList
%      The ElementList is a cell array containing the names of the elements
%      to be included in the cell: {'NameOfElement1','NameOfElment2', ...}.
%      The cell definition is only added when all elements have been
%      defined.
%    * grp : CellName, Size, Order
%      The Size is a vector of length in the range 1--5, the Order is
%      optional; when specified it should be a vector of the same length as
%      Size containing the numbers 1:length(Size). The Size vector may
%      contain one element 0 or inf to specify a dimension that may vary in
%      size. The group definition is only added when the cell refered to by
%      CellName exists.
%    * data: GrpDefName, AttribName1, Value1, AttribName2, Value2, ...
%      The AttributeNames and Values are optional. When attributes are
%      specified at most 5 attributes with a scalar real value, 5
%      attributes with scalar integer, and 5 attributes with string value
%      can be specified. Attributes may be changed later using VS_PUT. The
%      data group is only added when the specified group definition exists.
%    * data: {ElementList}, Size, Order, AttribName1, Value1, AttribName2, Value2, ...
%      The Order, AttributeNames and Values are optional. Restrictions as
%      mentioned above. The same string is used for name of the data group,
%      the name of the group definition, and the name of the cell.
%
%   In none of the cases will the function overwrite existing
%   data/definitions. Redefining groups, cells and elements is only
%   possible after removing the old definition, using the following
%   command:
%
%   DataInfoOut=vs_def(DataInfo,Command,Type,Name) removes an element,
%   cell, group definition or data group as indicated by the Type:
%   'elm','cell','grp','data' with the specified Name. The Command
%   determines the behaviour of the function in case of cross linking:
%    * 'remove': removes definitions only if they are not in use. Removing
%                data groups will result in loss of the data in that group.
%                Lower levels of definitions are not removed.
%    * 'purge' : same as remove, but also recursively removes all
%                definitions below if not used by other data groups, or
%                group or cell definitions. Purging a data group will
%                result in loss of the data stored in that group.
%    * '*remove' and '*purge':
%                remove higher levels first if definition is in use. When
%                this requires removing one (or more) data groups, data
%                stored in those groups is lost.
%
%   In both cases the DataInfo argument is optional; if it is not specified
%   the function will use the last opened or created NEFIS file(s).
%
%   See also VS_USE, VS_INI, VS_PUT.

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

% DataInfoOut=vs_def(DataInfo,Type,Name, ...)
% DataInfoOut=vs_def(Type,Name, ...)
% DataInfoOut=vs_def(DataInfo,Command,Type,Name)
% DataInfoOut=vs_def(Command,Type,Name)

% debug?

ErrMsg='';
defaultVS=0;
if nargin<2
   ErrMsg='Not enough input arguments.';
else
   INP=varargin;
   VS=[];
   if isstruct(INP{1})
      VS=INP{1};
      INP=INP(2:end);
   end
   if ~isstruct(VS)
      defaultVS=1;
      VS=vs_use('lastread');
   end
   if ~isfield(VS,'AddressType')
      VS.AddressType = 'uint32';
   end
   
   if ~isstruct(VS) % no defaultVS
      ErrMsg='No NEFIS file specified.';
   else
      % DataInfo has been dealt with, now continue analyzing INP:
      % * Type,Name, ...
      % * Command,Type,Name
      if ~ischar(INP{1}) % something invalid
         ErrMsg='Invalid type; it should be: data, grp, cell, or elm.';
      else
         switch lower(INP{1})
            case {'remove','purge','*remove','*purge'}
               % A remove call: Command,Type,Name
               if length(INP)~=3
                  ErrMsg='Incorrect number of input arguments.';
               else
                  switch lower(INP{2})
                     case {'data','grp','cell','elm'}
                        if ~ischar(INP{3})
                           ErrMsg='Invalid name specified.';
                        else
                           [VS,ErrMsg]=Local_remove(VS,INP{1},INP{2},INP{3});
                        end
                     otherwise
                        ErrMsg='Invalid type; it should be: data, grp, cell, or elm.';
                  end
               end
            case 'data'
               % A create call: 'data', Name, GrpDefName, AttribName1, Value1, AttribName2, Value2, ...
               %            or: 'data', Name, {ElmList}, Size, [Order,] AttribName1, Value1, AttribName2, Value2, ...
               if length(INP)<3
                  ErrMsg='Too few input arguments for creation of data group.';
               elseif iscell(INP{3})
                  % 'data', Name, {ElmList}, Size, [Order,] AttribName1, Value1, AttribName2, Value2, ...
                  if length(INP)<4
                     ErrMsg='Too few input arguments for easy creation of data group.';
                  else
                     [VS,ErrMsg]=Local_create_cell(VS,INP{2},INP{3}{:});
                  end
                  if isempty(ErrMsg)
                     if length(INP)>4 & ~ischar(INP{5})
                        [VS,ErrMsg]=Local_create_grp(VS,INP{[2 2 4 5]});
                        if isempty(ErrMsg)
                           [VS,ErrMsg]=Local_create_data(VS,INP{[2 2 6:end]});
                        end
                     else
                        [VS,ErrMsg]=Local_create_grp(VS,INP{[2 2 4]});
                        if isempty(ErrMsg)
                           [VS,ErrMsg]=Local_create_data(VS,INP{[2 2 5:end]});
                        end
                     end
                  end
               else
                  % 'data', Name, GrpDefName, AttribName1, Value1, AttribName2, Value2, ...
                  [VS,ErrMsg]=Local_create_data(VS,INP{2:end});
               end;
            case 'grp'
               % A create call: 'grp', Name, CellName, Size [,Order]
               if length(INP)<4
                  ErrMsg='Too few input arguments for group definition.';
               elseif  length(INP)>5
                  ErrMsg='Too many input arguments for group definition.';
               else
                  [VS,ErrMsg]=Local_create_grp(VS,INP{2:end});
               end
            case 'cell'
               % A create call: 'cell', Name, Element1, Element2, ...
               if length(INP)<3
                  ErrMsg='Incorrect number of arguments for cell definition.';
               else
                  [VS,ErrMsg]=Local_create_cell(VS,INP{2:end});
               end
            case 'elm'
               % A create call: 'elm', Name, DataType, NBytesPerValue, Size, QuantityName, UnitsName, Description
               if length(INP)<5
                  ErrMsg='Too few input arguments for element definition.';
               elseif  length(INP)>8
                  ErrMsg='Too many input arguments for element definition.';
               else
                  [VS,ErrMsg]=Local_create_elm(VS,INP{2:end});
               end
            otherwise
               ErrMsg=sprintf('Invalid command (remove, purge, *remove, *purge) or\ntype (data, grp, cell, elm).');
         end
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


function [VSNEW,ErrMsg]=Local_create_data(VS,Name,GrpDefName,varargin)
ErrMsg='';
VSNEW=VS;
AddressType = VS.AddressType;
switch AddressType
   case 'uint32'
      AddressBytes = 4;
   case 'uint64'
      AddressBytes = 8;
end
%varargin -> ATTR
ATTR=varargin;
if round(length(ATTR)/2)*2~=length(ATTR)
   ErrMsg='Invalid pair of attribute name and value.';
   return
end
% check group data name
Name=deblank(Name);
if length(Name)>16
   ErrMsg='Group data name is too long.';
   return
end
if ~isempty(VSNEW.GrpDat) & ~isempty(strmatch(Name,{VSNEW.GrpDat(:).Name},'exact'))
   % check if definition is the same
   ig=strmatch(Name,{VSNEW.GrpDat(:).Name},'exact');
   gd=VSNEW.GrpDat(ig);
   if ~isequal(GrpDefName,gd.DefName)
      ErrMsg=sprintf('Another data group with the name ''%s'' already exists.',Name);
   elseif length(ATTR)>0 % if attributes specified
      [VSNEW,ErrMsg]=vs_put(VSNEW,Name,[],ATTR{:});
   end
   return
end
% check group definition name
if isempty(VSNEW.GrpDef)
   ErrMsg=sprintf('No groups defined.');
   return
end
GrpDefName=deblank(GrpDefName);
GrpDefNr=strmatch(GrpDefName,{VSNEW.GrpDef(:).Name},'exact');
if ~isequal(size(GrpDefNr),[1 1])
   ErrMsg=sprintf('Group definition ''%s'' does not exist.',GrpDefName);
   return
end
CelDefNr=VSNEW.GrpDef(GrpDefNr).CelIndex;
TotSize=0;
Elms=VSNEW.CelDef(CelDefNr).Elm;
for i=Elms
   TotSize=TotSize+VSNEW.ElmDef(i).SizeElm;
end
TotSize=TotSize*prod(max(1,VSNEW.GrpDef(GrpDefNr).SizeDim));

%check validity of attributes
if length(ATTR)>0 % if attributes specified
   for i=1:2:length(ATTR)-1
      if ~ischar(ATTR{i}) | size(ATTR{i},1)~=1
         ErrMsg=sprintf('Invalid attribute name %i',(i+1)/2);
         return
      else
         ATTR{i}=deblank(ATTR{i});
         if isempty(ATTR{i})
            ErrMsg=sprintf('Attribute name %i is blank.',(i+1)/2);
            return
         elseif length(ATTR{i})>16
            ErrMsg=sprintf('Attribute name %i (%s)is too long.',(i+1)/2,ATTR{i});
            return
         end
      end
   end
end

fid=fopen([VSNEW.FileName VSNEW.DatExt],'r+',VSNEW.Format);
if fid<0
   ErrMsg='Error opening data file.';
   return
end
OK=fseek(fid,0,1); % go to end of file
Offset=ftell(fid);
VD=any(VSNEW.GrpDef(GrpDefNr).SizeDim==0);
if VD
   VD=find(VSNEW.GrpDef(GrpDefNr).SizeDim==0);
   DatSize=AddressBytes+256*AddressBytes; % celsize + first block of pointer list
else
   DatSize=TotSize;
end
fwrite(fid,-1,AddressType(2:end));
fwrite(fid,392+3*AddressBytes+DatSize,AddressType); % <== Not consistent with other records
RecID=repmat(' ',1,AddressBytes);
RecID(end) = '4'+(VD>0);
fwrite(fid,RecID,'uchar');
fwrite(fid,leftstr(Name,16),'uchar');
fwrite(fid,leftstr(GrpDefName,16),'uchar');
fwrite(fid,char(32*ones(1,5*16)),'uchar');
fwrite(fid,[0 0 0 0 0],'int32');
fwrite(fid,char(32*ones(1,5*16)),'uchar');
fwrite(fid,[0 0 0 0 0],'float32');
fwrite(fid,char(32*ones(1,5*16)),'uchar');
fwrite(fid,char(32*ones(1,5*16)),'uchar');
if VD
   fwrite(fid,TotSize,AddressType);
   Loc=ftell(fid);
   fwrite(fid,Loc+256*AddressBytes,AddressType); % link to second pointer field, unsigned address
   fwrite(fid,-ones(1,255),AddressType(2:end));
   % end of data record -------------------
   fwrite(fid,Loc+2*256*AddressBytes,AddressType); % link to third pointer field, unsigned address
   fwrite(fid,-ones(1,255),AddressType(2:end));
   % end of second pointer block -------------------
   fwrite(fid,Loc+3*256*AddressBytes,AddressType); % link to fourth pointer field, unsigned address
   fwrite(fid,-ones(1,255),AddressType(2:end));
   % end of third pointer block -------------------
   fwrite(fid,-ones(1,256),AddressType(2:end));
   % end of fourth pointer block -------------------
else
   fwrite(fid,zeros(1,TotSize),'int8');
end
onefile=strcmp(VSNEW.DefExt,VSNEW.DatExt);
ErrMsg=AddToHashTable(VSNEW,fid,onefile,'data',Name,Offset);
UpdateFileSize(VSNEW,fid,onefile);
fclose(fid);
i=length(VSNEW.GrpDat)+1;
VSNEW.GrpDat(i).Offset=Offset;
VSNEW.GrpDat(i).VarDim=VD;
VSNEW.GrpDat(i).Name=Name;
VSNEW.GrpDat(i).DefName=GrpDefName;
VSNEW.GrpDat(i).DefIndex=GrpDefNr;
VSNEW.GrpDat(i).IANames=char(repmat(32,5,16));
VSNEW.GrpDat(i).IAValue=[0 0 0 0 0];
VSNEW.GrpDat(i).RANames=char(repmat(32,5,16));
VSNEW.GrpDat(i).RAValue=[0 0 0 0 0];
VSNEW.GrpDat(i).SANames=char(repmat(32,5,16));
VSNEW.GrpDat(i).SAValue=char(repmat(32,5,16));
VSNEW.GrpDat(i).SizeDim=VSNEW.GrpDef(GrpDefNr).SizeDim;
VSNEW.GrpDat(i).OrderDim=VSNEW.GrpDef(GrpDefNr).OrderDim;
if length(ATTR)>0
   [VSNEW,ErrMsg]=vs_put(VSNEW,Name,[],ATTR{:});
end


function [VSNEW,ErrMsg]=Local_create_grp(VS,Name,CellName,Size,Order)
ErrMsg='';
VSNEW=VS;
AddressType = VS.AddressType;
switch AddressType
   case 'uint32'
      AddressBytes = 4;
   case 'uint64'
      AddressBytes = 8;
end
% > order check
if nargin<5
   Order=1:length(Size);
end
% check group def name
Name=deblank(Name);
if length(Name)>16
   ErrMsg='Group definition name is too long.';
   return
end
if ~isempty(VSNEW.GrpDef) & ~isempty(strmatch(Name,{VSNEW.GrpDef(:).Name},'exact'))
   % check if definition is the same
   id=strmatch(Name,{VSNEW.GrpDef(:).Name},'exact');
   gd=VSNEW.GrpDef(id);
   if ~isequal(CellName,gd.CelName) | ~isequal(Size,gd.SizeDim) | ~isequal(Order,gd.OrderDim)
      ErrMsg=sprintf('Another definition of group ''%s'' already exists.',Name);
   end
   return
end
% check cell name
if isempty(VSNEW.CelDef)
   ErrMsg=sprintf('No cells defined.');
   return
end
CellName=deblank(CellName);
CelNr=strmatch(CellName,{VSNEW.CelDef(:).Name},'exact');
if ~isequal(size(CelNr),[1 1])
   ErrMsg=sprintf('Cell ''%s'' does not exist.',CellName);
   return
end
% check size
if (size(Size,1)~=1) | (size(Size,2)>5) | (size(Size,2)==0)
   ErrMsg='Invalid group size specified.';
   return
end
Size(~isfinite(Size))=0;
if sum(Size==0)>1
   ErrMsg='Too many variable size dimensions. At most one allowed.';
   return
end
if ~isnumeric(Size) | ~isequal(Size,round(Size)) | any(round(Size)<0)
   ErrMsg='Size vector should contain positive integers only.';
   return
end
% check order
if ~isequal(size(Order),size(Size))
   ErrMsg='Invalid dimension ordering specified.';
   return
end
fid=fopen([VSNEW.FileName VSNEW.DefExt],'r+',VSNEW.Format);
if fid<0
   ErrMsg='Error opening definition file.';
   return
end
OK=fseek(fid,0,1); % go to end of file
Offset=ftell(fid);
fwrite(fid,-1,AddressType(2:end));
fwrite(fid,76+AddressBytes,AddressType);
RecID=repmat(' ',1,AddressBytes);
RecID(end) = '3';
fwrite(fid,RecID,'uchar');
fwrite(fid,leftstr(Name,16),'uchar');
fwrite(fid,leftstr(CellName,16),'uchar');
fwrite(fid,length(Size),'uint32');
SSize=Size; SSize(6)=0;
fwrite(fid,SSize(1:5),'uint32');
SOrder=Order; SOrder(6)=0;
fwrite(fid,SOrder(1:5),'uint32');
onefile=strcmp(VSNEW.DefExt,VSNEW.DatExt);
ErrMsg=AddToHashTable(VSNEW,fid,onefile,'grp',Name,Offset);
UpdateFileSize(VSNEW,fid,onefile);
fclose(fid);
if isempty(ErrMsg)
   i=length(VSNEW.GrpDef)+1;
   VSNEW.GrpDef(i).Offset=Offset;
   VSNEW.GrpDef(i).Name=Name;
   VSNEW.GrpDef(i).CelName=CellName;
   VSNEW.GrpDef(i).CelIndex=CelNr;
   VSNEW.GrpDef(i).SizeDim=Size;
   VSNEW.GrpDef(i).OrderDim=Order;
end


function [VSNEW,ErrMsg]=Local_create_cell(VS,Name,varargin)
ErrMsg='';
VSNEW=VS;
AddressType = VS.AddressType;
switch AddressType
   case 'uint32'
      AddressBytes = 4;
   case 'uint64'
      AddressBytes = 8;
end
% check cell name
Name=deblank(Name);
if length(Name)>16
   ErrMsg='Cell name is too long.';
   return
end
if ~isempty(VSNEW.CelDef) & ~isempty(strmatch(Name,{VSNEW.CelDef(:).Name},'exact'))
   % check if definition is the same
   ic=strmatch(Name,{VSNEW.CelDef(:).Name},'exact');
   ElmNms={VSNEW.ElmDef(VSNEW.CelDef(ic).Elm).Name};
   if ~isempty(setdiff(ElmNms,varargin)) | ~isempty(setdiff(varargin,ElmNms))
      ErrMsg=sprintf('Another definition of cell ''%s'' already exists.',Name);
   end
   return
end
% check element list
if isempty(VSNEW.ElmDef)
   ErrMsg=sprintf('No elements defined.');
   return
end
AllElms={VSNEW.ElmDef(:).Name};
TotSize=0;
NElm=length(varargin);
uElm = unique(varargin);
if length(uElm)<NElm
    for i=length(uElm):-1:1
        if length(strmatch(uElm{i},varargin,'exact'))==1
            uElm(i) = [];
        end
    end
    elmStr = sprintf('''%s'', ',uElm{:});
    if length(uElm)>1
        elm = 'elements';
        occur = 'occur';
    else
        elm = 'element';
        occur = 'occurs';
    end
    warning('The %s %s %s multiple times in the newly created cell ''%s''; the second copy will be inaccessible!',elm,elmStr(1:end-2),occur,Name)
end
ElmNrs=ones(1,NElm);
for i=1:NElm
   x=strmatch(deblank(varargin{i}),AllElms,'exact');
   if ~isequal(size(x),[1 1])
      ErrMsg=sprintf('Element ''%s'' does not exist.',varargin{i});
      return
   end
   ElmNrs(i)=x;
   TotSize=TotSize+VSNEW.ElmDef(x).SizeElm;
end
fid=fopen([VSNEW.FileName VSNEW.DefExt],'r+',VSNEW.Format);
if fid<0
   ErrMsg='Error opening definition file.';
   return
end
OK=fseek(fid,0,1); % go to end of file
Offset=ftell(fid);
fwrite(fid,-1,AddressType(2:end));
fwrite(fid,20+2*AddressBytes+NElm*16,AddressType);
RecID=repmat(' ',1,AddressBytes);
RecID(end) = '2';
fwrite(fid,RecID,'uchar');
fwrite(fid,leftstr(Name,16),'uchar');
fwrite(fid,TotSize,AddressType);
fwrite(fid,NElm,'uint32');
for i=1:NElm
   fwrite(fid,leftstr(varargin{i},16),'uchar');
end
onefile=strcmp(VSNEW.DefExt,VSNEW.DatExt);
ErrMsg=AddToHashTable(VSNEW,fid,onefile,'cell',Name,Offset);
UpdateFileSize(VSNEW,fid,onefile);
fclose(fid);
if isempty(ErrMsg)
   i=length(VSNEW.CelDef)+1;
   VSNEW.CelDef(i).Offset=Offset;
   VSNEW.CelDef(i).Name=Name;
   VSNEW.CelDef(i).Elm=ElmNrs;
end


function [VSNEW,ErrMsg]=Local_create_elm(VS,Name,DataType,NBytesPerValue,Size,QuantityName,UnitsName,Description)
ErrMsg='';
VSNEW=VS;
if nargin<8
   Description='';
   if nargin<7
      UnitsName='';
      if nargin<6
         QuantityName='';
      end
   end
end
AddressType = VS.AddressType;
switch AddressType
   case 'uint32'
      AddressBytes = 4;
   case 'uint64'
      AddressBytes = 8;
end
% check element name
Name=deblank(Name);
if length(Name)>16
   ErrMsg='Element name is too long.';
   return
end
if ~isempty(VSNEW.ElmDef) & ~isempty(strmatch(Name,{VSNEW.ElmDef(:).Name},'exact'))
   % check if definition is the same
   ie=strmatch(Name,{VSNEW.ElmDef(:).Name},'exact');
   ed=VSNEW.ElmDef(ie);
   if ischar(DataType)
      DataType=ustrcmpi(lower(DataType),{'character','complex','integer','logical','real'});
   end
   if ~isequal(DataType,ed.Type) | ~isequal(NBytesPerValue,ed.SizeVal) ...
         | ~isequal(Size,ed.Size) | ~isequal(QuantityName,ed.Quantity) ...
         | ~isequal(UnitsName,ed.Units) | ~isequal(Description,ed.Description)
      ErrMsg=sprintf('Another definition of element ''%s'' already exists.',Name);
   end
   return
end
% check datatype
if ischar(DataType)
   dt=ustrcmpi(lower(DataType),{'character','complex','integer','logical','real'});
   if dt<0 % unique identification of type
      ErrMsg=sprintf('Invalid datatype string: %s.',DataType);
      return
   end
   DataType=dt;
elseif ~isnumeric(DataType) | ~ismember(DataType,1:5)
   ErrMsg='Invalid datatype value.';
   return
end
% check number of bytes per value
if ~isequal(size(NBytesPerValue),[1 1]) | (NBytesPerValue<=0) | (NBytesPerValue~=round(NBytesPerValue)) | ~isfinite(NBytesPerValue)
   ErrMsg='Invalid number of bytes per value specified.';
   return
end
switch DataType
   case 1 % character
      % all acceptable
      TYPE='CHARACTE';
   case 2 % complex
      if (NBytesPerValue~=8) & (NBytesPerValue~=16)
         ErrMsg='Invalid number of bytes per value specified for complex element.';
         return
      end
      TYPE='COMPLEX ';
   case 3 % integer
      if (NBytesPerValue~=2) & (NBytesPerValue~=4)
         ErrMsg='Invalid number of bytes per value specified for integer element.';
         return
      end
      TYPE='INTEGER ';
   case 4 % logical
      if (NBytesPerValue~=2) & (NBytesPerValue~=4)
         ErrMsg='Invalid number of bytes per value specified for logical element.';
         return
      end
      TYPE='LOGICAL ';
   case 5 % real
      if (NBytesPerValue~=4) & (NBytesPerValue~=8)
         ErrMsg='Invalid number of bytes per value specified for real element.';
         return
      end
      TYPE='REAL    ';
end
% check size
if (size(Size,1)~=1) | (size(Size,2)>5) | (size(Size,2)==0)
   ErrMsg='Invalid element size specified.';
   return
end
if ~isnumeric(Size) | ~isequal(Size,round(Size)) | ...
      any(round(Size)<0) | any(~isfinite(Size))
   ErrMsg='Size vector should contain positive integers only.';
   return
end
% save to file
fid=fopen([VSNEW.FileName VSNEW.DefExt],'r+',VSNEW.Format);
if fid<0
   ErrMsg='Error opening definition file.';
   return
end
OK=fseek(fid,0,1); % go to end of file
Offset=ftell(fid);
fwrite(fid,-1,AddressType(2:end));
fwrite(fid,148+2*AddressBytes,AddressType);
RecID=repmat(' ',1,AddressBytes);
RecID(end) = '1';
fwrite(fid,RecID,'uchar');
fwrite(fid,leftstr(Name,16),'uchar');
fwrite(fid,TYPE,'uchar');
fwrite(fid,NBytesPerValue*prod(Size),AddressType);
fwrite(fid,NBytesPerValue,'uint32');
fwrite(fid,[leftstr(QuantityName,16) leftstr(UnitsName,16) leftstr(Description,64)],'uchar');
ndims=length(Size);
fwrite(fid,ndims,'uint32');
Size(6)=0;
fwrite(fid,Size(1:5),'uint32');
onefile=strcmp(VSNEW.DefExt,VSNEW.DatExt);
ErrMsg=AddToHashTable(VSNEW,fid,onefile,'elm',Name,Offset);
UpdateFileSize(VSNEW,fid,onefile);
fclose(fid);
Size=Size(1:ndims);
if isempty(ErrMsg)
   i=length(VSNEW.ElmDef)+1;
   VSNEW.ElmDef(i).Offset=Offset;
   VSNEW.ElmDef(i).Name=Name;
   VSNEW.ElmDef(i).Type=DataType;
   VSNEW.ElmDef(i).SizeVal=NBytesPerValue;
   VSNEW.ElmDef(i).SizeElm=NBytesPerValue*prod(Size);
   VSNEW.ElmDef(i).Quantity=QuantityName;
   VSNEW.ElmDef(i).Units=UnitsName;
   VSNEW.ElmDef(i).Description=Description;
   VSNEW.ElmDef(i).Size=Size;
end


function [VS,ErrMsg,FatalErr]=Local_remove(VSin,rmCommand,Type,Name) % called recursively
VS=VSin;
ErrMsg='';
onefile=isequal(VS.DatExt,VS.DefExt);
FatalErr=0;
switch Type
   case 'data'
      if isempty(VS.GrpDat)
         i=[];
      else
         i=strmatch(Name,{VS.GrpDat(:).Name},'exact');
      end
      if isempty(i)
         ErrMsg=sprintf('Invalid group name: %s',Name);
         return
      end
      id=VS.GrpDat(i).DefIndex;
      %remove
      fid=fopen([VS.FileName VS.DatExt],'r+',VS.Format);
      ErrMsg=RemoveFromHashTable(VS,fid,onefile,Type,Name,VS.GrpDat(i).Offset);
      fclose(fid);
      if ~isempty(ErrMsg)
         FatalErr=1;
         return
      end
      VS.GrpDat(i)=[];
      %remove
      switch rmCommand
         case {'purge','*purge'}
            if id>0
               [VS,ErrMsg,FatalErr]=Local_remove(VS,'purge','grp',VS.GrpDef(id).Name);
            end
      end
   case 'grp'
      if isempty(VS.GrpDef)
         id=[];
      else
         id=strmatch(Name,{VS.GrpDef(:).Name},'exact');
      end
      if isempty(id)
         ErrMsg=sprintf('Invalid group definition name: %s',Name);
         return
      end
      di=[VS.GrpDat(:).DefIndex];
      InUse=find(di==id);
      switch rmCommand
         case {'remove','purge'}
            if ~isempty(InUse)
               ErrMsg='Cannot remove group definition; it is used.';
               return
            end
         case {'*remove','*purge'}
            for i=-sort(-InUse)
               [VS,ErrMsg]=Local_remove(VS,'*remove','data',VS.GrpDat(i).Name);
            end
      end
      ic=VS.GrpDef(id).CelIndex;
      %remove
      fid=fopen([VS.FileName VS.DefExt],'r+',VS.Format);
      ErrMsg=RemoveFromHashTable(VS,fid,onefile,Type,Name,VS.GrpDef(id).Offset);
      fclose(fid);
      if ~isempty(ErrMsg)
         FatalErr=1;
         return
      end
      for i=1:length(VS.GrpDat)
         if VS.GrpDat(i).DefIndex>id
            VS.GrpDat(i).DefIndex=VS.GrpDat(i).DefIndex-1;
         end
      end
      VS.GrpDef(id)=[];
      %remove
      switch rmCommand
         case {'purge','*purge'}
            if ic>0
               [VS,ErrMsg,FatalErr]=Local_remove(VS,'purge','cell',VS.CelDef(ic).Name);
            end
      end
   case 'cell'
      if isempty(VS.CelDef)
         ic=[];
      else
         ic=strmatch(Name,{VS.CelDef(:).Name},'exact');
      end
      if isempty(ic)
         ErrMsg=sprintf('Invalid cell definition name: %s',Name);
         return
      end
      ci=[VS.GrpDef(:).CelIndex];
      InUse=find(ci==ic);
      switch rmCommand
         case {'remove','purge'}
            if ~isempty(InUse)
               ErrMsg='Cannot remove cell definition; it is used.';
               return
            end
         case {'*remove','*purge'}
            for i=-sort(-InUse)
               [VS,ErrMsg]=Local_remove(VS,'*remove','grp',VS.GrpDef(i).Name);
            end
      end
      el=VS.CelDef(ic).Elm;
      %remove
      fid=fopen([VS.FileName VS.DefExt],'r+',VS.Format);
      ErrMsg=RemoveFromHashTable(VS,fid,onefile,Type,Name,VS.CelDef(ic).Offset);
      fclose(fid);
      if ~isempty(ErrMsg)
         FatalErr=1;
         return
      end
      for i=1:length(VS.GrpDef)
         if VS.GrpDef(i).CelIndex>ic
            VS.GrpDef(i).CelIndex=VS.GrpDef(i).CelIndex-1;
         end
      end
      VS.CelDef(ic)=[];
      %remove
      switch rmCommand
         case {'purge','*purge'}
            for e=-sort(-el)
               [VS,ErrMsg,FatalErr]=Local_remove(VS,'remove','elm',VS.ElmDef(e).Name);
            end
      end
   case 'elm'
      if isempty(VS.ElmDef)
         el=[];
      else
         el=strmatch(Name,{VS.ElmDef(:).Name},'exact');
      end
      if isempty(el)
         ErrMsg=sprintf('Invalid element name: %s',Name);
         return
      end
      InUse=[];
      for ic=1:length(VS.CelDef)
         if ismember(el,VS.CelDef(ic).Elm)
            InUse(end+1)=ic;
         end
      end
      switch rmCommand
         case {'remove','purge'}
            if ~isempty(InUse)
               ErrMsg='Cannot remove element; it is used.';
               return
            end
         case {'*remove','*purge'}
            for i=-sort(-InUse)
               [VS,ErrMsg,FatalErr]=Local_remove(VS,'*remove','cell',VS.CelDef(i).Name);
            end
      end
      %remove
      fid=fopen([VS.FileName VS.DefExt],'r+',VS.Format);
      ErrMsg=RemoveFromHashTable(VS,fid,onefile,Type,Name,VS.ElmDef(el).Offset);
      fclose(fid);
      if ~isempty(ErrMsg)
         FatalErr=1;
         return
      end
      for i=1:length(VS.CelDef)
         k=(VS.CelDef(i).Elm>el);
         VS.CelDef(i).Elm(k)=VS.CelDef(i).Elm(k)-1;
      end
      VS.ElmDef(el)=[];
      %remove
end
if ~FatalErr
   ErrMsg='';
end

function ErrMsg=AddToHashTable(VS,fid,onefile,Type,Label,Offset)
ErrMsg='';
AddressType = VS.AddressType;
switch AddressType
   case 'uint32'
      AddressBytes = 4;
      Nil=2^32-1;
   case 'uint64'
      AddressBytes = 8;
      Nil=2^64-1;
end
if onefile
   switch Type
      case 'data'
         BeginHT=128+AddressBytes+3*AddressBytes*997;
      case 'grp'
         BeginHT=128+AddressBytes+2*AddressBytes*997;
      case 'cell'
         BeginHT=128+AddressBytes+AddressBytes*997;
      case 'elm'
         BeginHT=128+AddressBytes;
   end
else % two files
   switch Type
      case 'data'
         BeginHT=60+AddressBytes; % data file
      case 'grp'
         BeginHT=60+AddressBytes+2*AddressBytes*997; % def file
      case 'cell'
         BeginHT=60+AddressBytes+AddressBytes*997; % def file
      case 'elm'
         BeginHT=60+AddressBytes; % def file
   end
end
Nr=HashNr(Label);
fseek(fid,BeginHT+(Nr-1)*AddressBytes,-1);
Link=fread(fid,[1 1],AddressType); % read hash table element, unsigned address
while Link~=Nil
   if fseek(fid,Link,-1)<0 % follow link
      ErrMsg=sprintf('Invalid link encountered: %i.',Link);
      return
   end
   Link=fread(fid,[1 1],AddressType); % read next link, unsigned address
   if isempty(Link)
      ErrMsg=sprintf('Cannot read next link at: %i.',ftell(fid));
      return
   end
   fread(fid,1,AddressType); % dummy read size
   fread(fid,1,AddressType); % dummy read marker
   TmpLabel=char(fread(fid,[1 16],'uchar'));
   if isequal(deblank(Label),deblank(TmpLabel))
      ErrMsg=sprintf('%s %s already exists.',Type,Label);
      return
   end
   if Link==Nil
      fseek(fid,-16-2*AddressBytes,0); % move back to link: AddressBytes (size)
      %                                + 20 bytes (marker and label)
   end
end
fseek(fid,-AddressBytes,0); % move back to link:  AddressBytes (link)
fwrite(fid,Offset,AddressType); % add link, unsigned address


function ErrMsg=RemoveFromHashTable(VS,fid,onefile,Type,Label,Offset)
ErrMsg='';
AddressType = VS.AddressType;
switch AddressType
   case 'uint32'
      AddressBytes = 4;
      Nil=2^32-1;
   case 'uint64'
      AddressBytes = 8;
      Nil=2^64-1;
end
if onefile
   switch Type
      case 'data'
         BeginHT=128+AddressBytes+3*AddressBytes*997;
      case 'grp'
         BeginHT=128+AddressBytes+2*AddressBytes*997;
      case 'cell'
         BeginHT=128+AddressBytes+AddressBytes*997;
      case 'elm'
         BeginHT=128+AddressBytes;
   end
else % two files
   switch Type
      case 'data'
         BeginHT=60+AddressBytes; % data file
      case 'grp'
         BeginHT=60+AddressBytes+2*AddressBytes*997; % def file
      case 'cell'
         BeginHT=60+AddressBytes+AddressBytes*997; % def file
      case 'elm'
         BeginHT=60+AddressBytes; % def file
   end
end
Nr=HashNr(Label);
LLabel=leftstr(Label,16);
fseek(fid,BeginHT+(Nr-1)*AddressBytes,-1);
Link=fread(fid,[1 1],AddressType); % read hash table element, unsigned address
while (Link~=Offset)
   if Link==Nil
      ErrMsg=sprintf('%s (offset: %u) not found, invalid hash entry (%i) ?',Label,Offset,Nr);
      return
   end
   if fseek(fid,Link,-1)<0 % follow link
      ErrMsg=sprintf('Invalid link encountered: %u.',Link);
      return
   end
   Link=fread(fid,[1 1],AddressType); % read next link, unsigned address
   if isempty(Link)
      ErrMsg=sprintf('Cannot read next link at: %u.',ftell(fid));
      return
   end
end
Address=ftell(fid)-AddressBytes; % store where the offset is found
fseek(fid,Offset,-1); % move to offset
Link=fread(fid,[1 1],AddressType); % read possible next link, unsigned address
fread(fid,1,AddressType); % read dummy size
fread(fid,1,AddressType); % read dummy marker
TmpLabel=char(fread(fid,[1 16],'uchar'));
if ~isequal(LLabel,TmpLabel)
   ErrMsg=sprintf('%s %s not stored at specified offset: %u.',Type,Label,Offset);
   return
end
fseek(fid,Address,-1); % move to location where offset was found
fwrite(fid,Link,AddressType); % replace offset by possible next link, unsigned address


function Nr=HashNr(Label) % determine nr of hash entry
LLabel=leftstr(Label,16);
Nr=mod(sum(prod(reshape(char(LLabel)+1,[4 4]))),997)+1;


function Str=leftstr(StrIn,Len)
Str=char(32*ones(1,Len));
J=min(length(StrIn),Len);
Str(1:J)=StrIn(1:J);


function UpdateFileSize(VS,fid,onefile) % update filesize
fseek(fid,0,1); % move to eof
FSize=ftell(fid);
if onefile
   fseek(fid,128,-1);
else
   fseek(fid,60,-1);
end
fwrite(fid,FSize,VS.AddressType); % update file size, unsigned address space
