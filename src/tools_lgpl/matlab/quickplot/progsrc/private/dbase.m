function varargout=dbase(cmd,varargin)
%DBASE Read data from a dBase file.
%
%   FI=DBASE('open','filename')
%   Open a dBase file.
%
%   Data=DBASE('read',FI,Records,Fields)
%   Read specified records from the opened dBase file.
%   Support 0 for reading all records / fields.

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

if nargin==0
    if nargout>0
        varargout=cell(1,nargout);
    end
    return
end

switch lower(cmd)
    case {'open'}
        Info=Local_open_dbase(varargin{:});
        varargout={Info};
    case {'read'}
        Data=Local_read_dbase(varargin{:});
        varargout={Data};
    otherwise
        error('Unknown command')
end


function S=Local_open_dbase(filename)
S.Check='NotOK';
S.FileType='dBase';

if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.dbf','Select dBase file');
    if ~ischar(fname)
        return
    end
    filename=fullfile(fpath,fname);
end

S.FileName=filename;
encoding = Default_Encoding;
fid=fopen(filename,'r','l',encoding);
HTerminator = 13;
%--------------------------------------------------------------------------
% Version number
% bit pattern: MSSSFVVV
%                   VVV = version number 0-7
%                  F    = memo flag
%               SSS     = SQL table flag
%              M        = dBase III memo file (dbt) flag
S.SubTypeNr=fread(fid,1,'uint8');
switch S.SubTypeNr
    case 2
        S.SubType='dBase II'; % FoxBase
    case 3
        S.SubType='dBase III+'; % File without DBT = dBASE Level 5
    case 4
        S.SubType='dBase IV'; % dBASE IV w/o memo file = dBASE Level 7
    case 5
        S.SubType='dBase V'; % dBASE V w/o memo file
    case 7
        % VISUAL OBJECTS (first 1.0 versions) for the dBase III files w/o memo file
        S.SubType='Visual Objects';
    case 48
        S.SubType='Visual FoxPro';
    case 49
        S.SubType='Visual FoxPro with AutoIncrement field';
    case 67
        S.SubType='with .dbv memo var size'; % .dbv memo var size (Flagship)
    case 123
        S.SubType='dBASE IV with memo';
    case 131
        S.SubType='dBase III+ with memo'; % .dbt
    case 135
        % VISUAL OBJECTS (first 1.0 versions) for the Dbase III files (NTX clipper driver) with memo file
        S.SubType='Visual Objects with memo';
    case 139
        S.SubType='dBase IV with memo'; % .dbt
    case 142
        S.SubType='dBase IV with SQL table';
    case 179
        S.SubType='with .dbv and .dbt memo';
    case 229
        S.SubType='Clipper SIX driver w. SMT memo file';
        % Note! Clipper SIX driver sets lowest 3 bytes to 110 in descriptor of crypted databases. So, 3->6, 83h->86h, F5->F6, E5->E6 etc.
    case 245
        S.SubType='FoxPro with memo'; % .fmp
    otherwise
        S.SubType='unknown';
end
%--------------------------------------------------------------------------
% Date of last change
Date=fread(fid,[1 3],'uint8');
S.LastUpdate = ymd2date(Date);
%--------------------------------------------------------------------------
if S.SubTypeNr == 2
    % Following information from:
    % http://www.clicketyclick.dk/databases/xbase/format/db2_dbf.html
    S.NBytesRec = fread(fid,1,'uint16');
    if S.NBytesRec>1000
        fclose(fid);
        error('Specified record length too large for dBase II file.')
    end
    S.NFld = 32; % At most 32 records allowed
    for i = 1:32
        Record = fread(fid,[1 16],'uint8');
        if Record(1) == 0 % Empty record
            HTerminator = 0;
            S.NFld = i-1;
            break
        end
    end
    if S.NFld == 32
        if fread(fid,1,'uint8') ~= 13
            % Terminator: 0Dh if all 32 fields present, otherwise 00h.
            % http://www.clicketyclick.dk/databases/xbase/format/db2_dbf.html
            fclose(fid);
            error('Invalid header terminator in dBase II file.')
        end
    end
    StartData = ftell(fid);
    fseek(fid,0,1);
    EndData = ftell(fid);
    S.NRec = (EndData-StartData)/S.NBytesRec;
    fseek(fid,8,-1);
else
    S.NRec        = fread(fid,1,'uint32');
    S.HeaderBytes = fread(fid,1,'uint16');
%     S.NFld        = (S.HeaderBytes-33)/32;
%     if S.NFld~=round(S.NFld)
%         fclose(fid);
%         error('Invalid header size in dBase file.')
%     end
    S.NBytesRec   = fread(fid,1,'uint16'); % includes deleted flag
    fread(fid,2,'uint8'); % reserved
    S.Transaction = fread(fid,1,'uint8'); % Transaction status: Completed (or rolled back) = 0, Ongoing = 1
    S.Encrypted   = fread(fid,1,'uint8');
    fread(fid,4,'uint8'); % Free record thread (for LAN only)
    fread(fid,8,'uint8'); % dBase III+ multi-user environment
    S.ProdIndex = fread(fid,1,'uint8'); % Production Index Exists (Fp,dB4,dB5)
    S.LangID    = fread(fid,1,'uint8');
    %   1: DOS USA       Code Page  437
    %   2: DOS MultiLing            850
    %   3: Win ANSI                1252
    %   4: Mac
    % 100: DOS EE                   852
    %      Nordic (101; CP 865), Russian (102, CP 866), Iceland (103), Czech (104), Polish (105), Greek (106), Turkish (107)
    % 200: Win EE                  1250
    %      Russian (201), Turkish (202), Greek (203)
    %   0: ignored
    % --> might use this to determine the encoding
    fread(fid,2,'uint8'); % reserved
end
%--------------------------------------------------------------------------
% Field descriptor array
i = 0;
while 1
    i = i+1;
    loc = ftell(fid);
    Name = fread(fid,[1 11],'*char');
    if Name(1)==HTerminator
        S.NFld = i-1;
        fseek(fid,loc,-1);
        break
    end
    S.Fld(i).Name = deblank(Name);
    S.Fld(i).Type = fread(fid,1,'*char');
    % C : ASCII text <254 char in dBase (up to 32kB in FoxPro/Clipper by
    %     using Width+Decimal fields)
    % N : number (right justified string; max 18 char in dBase, up to 20 char in FoxPro/Clipper)
    % L : Logical ("?", "Y","y","T","t", "N","n","F","f")
    % D : Date YYYYMMDD (8 char)
    % M : Pointer to ASCII text field in memo file [10 digit DBT block number stored as right justified string; 0 = header of DBT file]
    % F : Floating points (20 char; dBase IV and later)
    % A : Character name variable (<254 char, up to 64kB in FoxPro/Clipper)
    % B : Binary (like M for dBase V, FoxPro/FoxBase double integer)
    % G : General (like M for dBase V) OLE Objects
    % P : Picture (like M for FoxPro)
    % Y : Currency (FoxPro)
    % T : DateTime (FoxPro 8 bytes: 4bytes [Julian Date Oct15,1582 = 2299161]
    %                               4bytes [milliseconds since midnight])
    % I : Integer (FoxPro 4 bytes little endian)
    % V : VariField Width=2: binary signed 2byte integer [FlagShip]
    %                     3: YYMMDD date
    %                     4: binary signed 4byte integer [FlagShip]
    %                     8: binary signed double IEEE [FlagShip]
    %                    10: Variable [FlagShip]
    %                        4bytes [start pos in memo]
    %                        4bytes [block size]
    %                        1bytes [subtype]
    %                        1bytes [reserved 0x1a]
    % X : Variant (X) for compatibility with SQL (Clipper)
    % @ : Time stamp (8 bytes: 4bytes [Days since Jan 1, 4713BC]
    %                          4bytes [milliseconds since midnight])
    % O : Double (8 bytes)
    % + : Autoincrement (4 bytes; long)
    if S.SubTypeNr == 2
        S.Fld(i).Width = fread(fid,1,'uint8');
        fread(fid,1,'uint16'); % memory address
        S.Fld(i).NDec = fread(fid,1,'uint8');
    else
        fread(fid,1,'uint32'); % memory address, record offset, ignored in latest versions
        S.Fld(i).Width     = fread(fid,1,'uint8');
        if ismember(S.Fld(i).Type,'MBGP') && S.Fld(i).Width==4 % binary version M
            S.Fld(i).Type  = 'MB';
        end
        S.Fld(i).NDec      = fread(fid,1,'uint8'); % Type='C' also Width
        fread(fid,2,'uint8'); % dBase III multi-user+ field
        fread(fid,1,'uint8'); % Work area ID
        fread(fid,2,'uint8'); % dBase III multi-user+ field
        fread(fid,1,'uint8'); % set fields
        fread(fid,7,'uint8'); % reserved
        S.Fld(i).ProdIndex = fread(fid,1,'uint8'); % field is part of production index
        % or ...
        %fread(fid,1,'uint8'); % field flags
        %                        0x01: system column
        %                        0x02: column can store null values
        %                        0x04: binary column (for char/memo only)
        %                        0x06: null and binary (integer, currency, char/memo)
        %                        0x0C: autoincrementing
        %fread(fid,4,'uint8'); % value of autoincrement next
        %fread(fid,1,'uint8'); % autoincrement step value
        %fread(fid,8,'uint8'); % reserved
        % or ...
        % dBase 7
        %fread(fid,2,'uint8'); % reserved
        %S.Fld(i).ProdIndex = fread(fid,1,'uint8'); % field has an index tag in the production .MDX file
        %fread(fid,2,'uint8'); % reserved
        %fread(fid,4,'uint8'); % next autoincrement value
        %fread(fid,4,'uint8'); % reserved
    end
end
%--------------------------------------------------------------------------
% Header terminator
Flag = fread(fid,1,'uint8');
%--------------------------------------------------------------------------
% Check record length
if S.NBytesRec ~= sum([S.Fld.Width]) + 1
    fclose(fid);
    error('Record length does not match accumulated field sizes.')
end
%--------------------------------------------------------------------------
% here for Visual Foxpro only: A 263-byte range that contains the
% backlink, which is the relative path of an associated database (.dbc)
% file, information. If the first byte is 0x00, the file is not associated
% with a database. Therefore, database files always contain 0x00.
%--------------------------------------------------------------------------
% here for dBase7: field properties structure
%fread(fid,1,'uint16'); % number of standard properties
%fread(fid,1,'uint16'); % start of standard property descriptor array
%fread(fid,1,'uint16'); % number of custom properties
%fread(fid,1,'uint16'); % start of custom property descriptor array
%fread(fid,1,'uint16'); % number of referential integrity properties
%fread(fid,1,'uint16'); % start of referential integrity property descriptor array
%fread(fid,1,'uint16'); % start of data
%fread(fid,1,'uint16'); % actual size of structure includeing data (file may be padded with zeros)
%[standard property array]
%fread(fid,1,'uint16'); % generational number - field with highest value is current
%fread(fid,1,'uint16'); % table field offset (1 = first table, 0 = in case of constraint)
%fread(fid,1,'uint8');  % property: 1=required, 2=min, 3=max, 4=default, 6=constraint
%fread(fid,1,'uint8');  % type: 0=no type - constraint, 1=char, 2=numeric, 3=memo, 4=logical, 5=date, 6=float, 8=OLE, 9=binary, 11=long, 12=timestamp, 13=double, 14=autoincrement
%fread(fid,1,'uint8');  % 0=constraint, 2=other
%fread(fid,4,'uint8');  % reserved
%fread(fid,1,'uint16'); % offset from the start of this structure to the data for the property.
%fread(fid,1,'uint16'); % width of database field associated with the property (including null terminator for constraint).
%[custom property array]
%fread(fid,1,'uint16'); % generational number - field with highest value is current
%fread(fid,1,'uint16'); % table field offset (1 = first table)
%fread(fid,1,'uint8');  % type: 1=char, 2=numeric, 3=memo, 4=logical, 5=date, 6=float, 8=OLE, 9=binary, 11=long, 12=timestamp, 13=double, 14=autoincrement
%fread(fid,1,'uint16'); % offset from the start of this structure to the property name.
%fread(fid,1,'uint16'); % length of property name.
%fread(fid,1,'uint16'); % offset from the start of this structure to the property data.
%fread(fid,1,'uint16'); % length of property data (excluding null terminator).
%[referential integrity array]
%fread(fid,1,'uint8');  % 7=master/parent, 8=dependent/child
%fread(fid,1,'uint16'); % sequential number (1 based; 0 if rule has been dropped)
%fread(fid,1,'uint16'); % offset to RI rule name (null terminated)
%fread(fid,1,'uint16'); % size of previous value
%fread(fid,1,'uint16'); % offset to foreign table name (null terminated)
%fread(fid,1,'uint16'); % size of previous value
%fread(fid,1,'uint8');  % 0=update cascade/1=delete cascade
%fread(fid,1,'uint16'); % number of fields in the linking key
%fread(fid,1,'uint16'); % offset to local table tag name (null terminated)
%fread(fid,1,'uint16'); % size of previous value
%fread(fid,1,'uint16'); % offset to foreign table tag name (null terminated)
%fread(fid,1,'uint16'); % size of previous value
%
%--------------------------------------------------------------------------
fseek(fid,S.HeaderBytes,-1);
%--------------------------------------------------------------------------
% Read all deleted flags
Flag=fread(fid,S.NRec,'*char',S.NBytesRec-1); % first record
if any(Flag~=' ' & Flag~='*') % *=deleted
    fclose(fid);
    Wrong = Flag(Flag~=' ' & Flag~='*');
    error('Invalid deleted flag in dBase file: ''%c'' (char %i).',Wrong(1),abs(Wrong(1)));
end
S.Deleted = Flag=='*';
%--------------------------------------------------------------------------
fclose(fid);
%
% Should the following depend on ProdIndex?
%
MemoFileName=S.FileName;
MemoFileName(end-2:end)=MemoFileName(end-2:end)-'DBF'+'DBT';
MemoFormat = 'DBT';
fid = fopen(MemoFileName,'r','l','US-ASCII');
if fid<0
    MemoFileName(end-2:end)=MemoFileName(end-2:end)-'DBT'+'FPT';
    MemoFormat = 'FPT';
    fid = fopen(MemoFileName,'r','l','US-ASCII');
end
if fid>0
    MemoFPT = strcmp(MemoFormat,'FPT');
    Memo.FileName = MemoFileName;
    Memo.Format   = MemoFormat;
    FFBlockL = fread(fid,1,'uint32','l');
    fseek(fid,0,-1);
    FFBlockB = fread(fid,1,'uint32','b');
    if FFBlockB<FFBlockL
        BOrd = 'b';
    else
        BOrd = 'l';
    end
    Memo.ByteOrder = BOrd;
    Memo.FirstFreeBlock = min(FFBlockL,FFBlockB);
    if MemoFPT
        fread(fid,1,'uint16'); % reserved
        Memo.BlockSize = fread(fid,1,'uint16',BOrd); % Size of block
    else
        Memo.BlockSize = 512;
        fread(fid,1,'uint32'); % reserved
    end
    fread(fid,2,'uint32'); % reserved
    Memo.Version = fread(fid,1,'uint8');
    %
    Memo.Block(Memo.FirstFreeBlock).Type   = 1;
    Memo.Block(Memo.FirstFreeBlock).Length = 1;
    Memo.Block(Memo.FirstFreeBlock)        = [];
    for i = 1:Memo.FirstFreeBlock-1 % doesn't take into account that a memo can be longer than one block
        fseek(fid,512+Memo.BlockSize*(i-1),-1);
        if MemoFPT
           Memo.Block(i).Type = fread(fid,1,'uint32',BOrd); % 0=picture, 1=memo, 2=object
           Memo.Block(i).Length = fread(fid,1,'uint32',BOrd);
           % fread(fid,Memo.Block(i).Length,'*char'); % memo text
        else
           Memo.Block(i).Type = 1;
           Text = fread(fid,512,'*char');
           EOT  = find(Text==26); % should be two occurrences, but FoxPro may use only one
           Memo.Block(i).Length = EOT(1);
        end
    end
    fclose(fid);
    %
    S.Memo = Memo;
end
%
% Should the following depend on ProdIndex?
%
MdxFileName=S.FileName;
MdxFileName(end-2:end)=MdxFileName(end-2:end)-'DBF'+'MDX';
fid = fopen(MdxFileName,'r','l','US-ASCII');
if fid>0
    fseek(fid,0,1);
    FileSize = ftell(fid);
    fseek(fid,0,-1);
    %
    MDX.FileName = MdxFileName;
    % https://www.cs.cmu.edu/~varun/cs315p/xbase.txt
    %----------------------------------------------------------------------
    % File Header
    MDX.SubTypeNr=fread(fid,1,'uint8');
    Date=fread(fid,[1 3],'uint8');
    MDX.Date_Creation = ymd2date(Date,'MDX','creation');
    MDX.Name = deblank(fread(fid,[1 16],'*char'));
    MDX.BlockSizePages = fread(fid,1,'uint16'); % 2 % Page = 512 Bytes
    MDX.BlockSizeBytes = fread(fid,1,'uint16'); % 1024
    MDX.ProdIndex = fread(fid,1,'uint8'); % Production Index Exists (Fp,dB4,dB5)
    MDX.NEntries = fread(fid,1,'uint8'); % max 48
    MDX.TagLen = fread(fid,1,'uint8'); % max 32
    fread(fid,1,'uint8');
    %MDX.NTags = fread(fid,1,'uint16'); % No.of tags in use
    %fread(fid,1,'uint16');
    MDX.NPages = fread(fid,1,'int32'); % No.of pages in tagfile
    MDX.FileSizePages = fread(fid,1,'int32'); % First Free Page
    %MDX.FileSizePages = fread(fid,1,'int32'); % No.of block available
    X = fread(fid,[1 2],'int32'); % 0 0
    Date=fread(fid,[1 3],'uint8');
    MDX.Date_LastUpdate=ymd2date(Date,'MDX','update');
    %
    %----------------------------------------------------------------------
    % Tag table entries
    fseek(fid,528,-1); % 544
    fread(fid,[1 4],'int32'); % 256 0 0 0
    for i=1:MDX.NPages
        MDX.Index(i).PageNumber = fread(fid,[1 1],'int32'); % 2*(1+i)
        MDX.Index(i).Offset = 512*MDX.Index(i).PageNumber; % 2*(1+i)
        MDX.Index(i).Name = deblank(fread(fid,[1 11],'*char'));
        MDX.Index(i).KeyFormat = fread(fid,1,'uint8'); % 00h Calculated, 10h Data field
        switch MDX.Index(i).KeyFormat
            case 0
                MDX.Index(i).KeyFormatStr = 'calculated';
            case 16
                MDX.Index(i).KeyFormatStr = 'data field';
            otherwise
                MDX.Index(i).KeyFormatStr = 'unknown';
        end
        MDX.Index(i).ForwardTagThread1 = fread(fid,1,'uint8'); % 0/3 (<)
        MDX.Index(i).ForwardTagThread2 = fread(fid,1,'uint8'); % 0/2/3/4/5 (>)
        MDX.Index(i).BackwardTagThread = fread(fid,1,'uint8'); % 0/1/2/4 (Pervious tag)
        MDX.Index(i).X1a = fread(fid,1,'uint8'); % reserved
        MDX.Index(i).KeyType = fread(fid,1,'*char'); % C (char), N (numerical), D (date)
        MDX.Index(i).X1b = fread(fid,[1 11],'uint8'); % reserved
        % 15 6 7 8 9 20
        % 16 0 2 0 2 67 0 0 0 0 0 0 0 0 0 0 0: LOCID, D_LOCID+D_STORMID
        % 16 3 2 0 2 67 0 0 0 0 0 0 0 0 0 0 0: STORMID
        % 16 0 4 1 2 67 0 0 0 0 0 0 0 0 0 0 0: MPEIL+WINDR+WINDS
        % 16 0 0 1 2 67 0 0 0 0 0 0 0 0 0 0 0: MPEIL, X+Y
        % 16 0 5 2 2 67 0 0 0 0 0 0 0 0 0 0 0: WINDR
        % 16 0 0 4 2 67 0 0 0 0 0 0 0 0 0 0 0: WINDS
        % 16 0 3 1 2 67 0 0 0 0 0 0 0 0 0 0 0: D_LOCID
        % 16 0 0 2 2 67 0 0 0 0 0 0 0 0 0 0 0: D_STORMID
    end
    %
    %----------------------------------------------------------------------
    % Tag header
    for i=1:MDX.NPages % same i ?
        fseek(fid,MDX.Index(i).Offset,-1);
        %
        % Offset of last part of table?
        %
        MDX.Index(i).RootPage = fread(fid,[1 1],'int32');
        MDX.Index(i).RootOffset = 512*MDX.Index(i).RootPage;
        %
        MDX.Index(i).X2a = fread(fid,1,'int32'); % File size in pages
        MDX.Index(i).KeyFormat1 = fread(fid,1,'uint8'); % 00h Right left dtoc, 08h Decending order, 10h Fields string, 40h Unique keys
        KT = fread(fid,1,'*char'); % C (char), N (numerical), D (date)
        if ~isequal(KT,MDX.Index(i).KeyType)
            error('Second key type (%s) does not match first key type (%s)',KT,MDX.Index(i).KeyType)
        end
        MDX.Index(i).X2b = fread(fid,1,'int16'); % reserved
        MDX.Index(i).NBytes = fread(fid,1,'int16'); % Numeric 12, Date 8, Character <= 100
        MDX.Index(i).NBytesExtended = ceil(MDX.Index(i).NBytes/4)*4;
        MDX.Index(i).MaxNKeysPerPage = fread(fid,1,'int16');
        MDX.Index(i).SecKeyType = fread(fid,1,'int16'); % 0 char/num (db4), char (db3); 1 date (db4), num/date (db3)
        MDX.Index(i).NBytesItem = fread(fid,1,'int16'); % 4+MDX.Index(i).NBytesExtended
        MDX.Index(i).X3 = fread(fid,[1 4],'uint8'); % 3 bytes reserved
        %MDX.Index(i).UniqueFlag = fread(fid,1,'uint8');
        % XX = mod(S.NRec,256)+2
        % XX 0 0 64: STORMID, LOCID
        % XX 0 0 64: MPEIL+WINDR+WINDS, D_LOCID+D_STORMID
        % XX 0 0  0: MPEIL, D_LOCID, D_STORMID
        % XX 0 0  0: WINDR, WINDS
        % XX 0 0 64: X+Y
        %      Key^
        MDX.Index(i).Name2 = deblank(fread(fid,[1 1000],'*char'));
        fseek(fid,MDX.Index(i).RootOffset,-1);
        MDX.Index(i).X4 = fread(fid,[1 2],'int32');
        for j=1:MDX.Index(i).X4(1)
            {MDX.Index(i).Name2 fread(fid,[1 1],'int32') fread(fid,[1 MDX.Index(i).NBytesExtended],'*char')};
        end
        MDX.Index(i)
    end
    %
    %fread(fid,[1 1],'int32')
    %fread(fid,[1 1],'int32')
    %for i=1:89
    %   fread(fid,[1 1],'int32')
    %   fread(fid,[1 4],'*char')
    %end

    N = zeros(1,MDX.NPages);
    %
    % Storm
    ii = [1 2 3 4 5 2 2 1 1 3 3 4 4 5 5 2 1 4 5];
    %
    %  1   2   3   4   5   2   2   1   1   3   3   4   4   5   5   2   1   4   5
    % 90  66  93  90  73  84   2  63   2 123   1  63   2  78   2  66  63  63  65
    %                          *       *       *       *       *
    %
    % Location
    %ii = [1 2 2 2];
    %
    %      1     2     2     2
    %     89    33    56     1
    %      *                 *
    %
    % Data
    %    ii = [1 2 3 1 1 2 2 3 3 1 1 2 2 2 2 2 1 1 1 2 1 2 2 2 2 1 2 1 2 1 2 ...
    %       2 1 1 2 1 1 2 2 2 1 2 2 2 2 1 2 2 2 1 2 1 2 1 1 1 1 2 2 2 2 2 2 2 ...
    %       2 1 1 1 1 2 1 1 1 2 1 2 2 2 2 2 1 2 2 2 2 2 2 2 1 1 2 1 2 2 2 2 2 ...
    %       1 2 2 1 1 2 1 1 1 1 1 2 1 1 1 2 1 1 1 1 1 1 1 2 2 1 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 1 2 1 1 1 1 1 1 2 2 1 2 2 1 ...
    %       2 2 1 2 2 2 2 2 2 2 1 2 1 1 2 2 1 2 2 2 2 1 2 1 1 2 1 1 1 1 1 2 1 ...
    %       1 1 2 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 ...
    %       2 2 2 2 2 2 1 2 2 2 2 2 1 1 1 1 2 1 1 1 2 1 1 1 2 1 1 1 2 1 2 1 2 ...
    %       1 1 2 2 1 2 2 2 2 2 2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 ...
    %       2 1 1 2 1 1 2 2 2 1 1 2 1 1 1 1 2 2 1 1 2 2 2 2 1 1 2 1 1 1 1 2 1 ...
    %       1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 ...
    %       1 1 2 1 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 ...
    %       2 2 2 2 1 2 2 2 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 2 1 2 2 2 ...
    %       2 2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1 2 1 1 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 ...
    %       2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 1 2 1 1 2 1 1 2 1 1 2 1 1 ...
    %       1 1 1 1 1 1 2 2 1 1 1 1 2 2 1 1 2 1 1 1 1 1 1 1 2 2 1 1 2 1 1 1 2 ...
    %       1 1 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 ...
    %       2 2 2 1 2 2 2 1 1 1 1 2 1 1 1 1 1 1 2 1 1 1 1 1 2 1 1 2 1 2 2 2 2 ...
    %       2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1 1 2 2 1 1 2 2 1 1 2 ...
    %       2 2 2 2 2 2 1 1 2 1 1 1 1 2 2 1 1 2 2 2 2 1 1 2 1 1 1 1 2 1 1 1 1 ...
    %       1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 ...
    %       2 1 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ...
    %       2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 ...
    %       2 2 2 1 1 1 1 2 2 1 1 1 1 2 1 2 1 1 2 1 1 1 2 2 1 2 1 2 1 1 2 2 2 ...
    %       2 1 2 2 2 2 2 2 2 2 2 1 2 2 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 ...
    %       1 1 2 1 1 2 2 2 1 1 2 1 1 1 1 2 2 1 1 2 2 2 2 1 1];
    for b = 1:MDX.FileSizePages/2-2-MDX.NPages
        fseek(fid,(b+1+MDX.NPages)*1024,-1);
        i=ii(1,b);
        A=fread(fid,[1 2],'int32');
        ii(2,b)=A(1);
        for j=1:A(1)-1
            {MDX.Index(i).Name2 fread(fid,[1 1],'int32') fread(fid,[1 MDX.Index(i).NBytesExtended],'*char')};
        end
        {MDX.Index(i).Name2 fread(fid,[1 1],'int32') fread(fid,[1 MDX.Index(i).NBytesExtended],'*char')};
        N(i)=N(i)+A(1);
    end
    ii;
    fclose(fid);
    S.MDX = MDX;
end
%
S.Check='OK';

function D = ymd2date(Date,FileType,DateType)
if nargin<2
    FileType = '';
else
    FileType = sprintf(' %s',FileType);
end
if nargin<3
    DateType = '';
else
    DateType = sprintf('%s ',DateType);
end
if (Date(2)>12) || (Date(2)==0) || (Date(3)==0) || (Date(3)>31)
    fclose(fid);
    error('Invalid %sdate in dBase%2 file.',DateType,FileType)
end
if Date(1)<70
    D = datenum(Date(1)+2000,Date(2),Date(3));
else
    D = datenum(Date(1)+1900,Date(2),Date(3));
end

function Encoding = Default_Encoding
Encoding = 'windows-1252';

function Dbs=Local_read_dbase(S,Records,Fields)
if ~isequal(Records,0)
    Records=Records(:);
    if any((Records>S.NRec) | (Records<1) | (Records~=round(Records)))
        error('Invalid record number.');
    end
end
if isequal(Fields,0)
    Fields=1:S.NFld;
else
    Fields=Fields(:)';
    if any((Fields>S.NFld) | (Fields<1) | (Fields~=round(Fields)))
        error('Invalid field number.');
    end
end
encoding = Default_Encoding;
if isfield(S,'Encoding')
    encoding = S.Encoding;
end
fid=fopen(S.FileName,'r','l',encoding);
Dbs=cell(1,length(Fields));
for j=1:length(Fields)
    i=Fields(j);
    fseek(fid,S.HeaderBytes+1+sum([S.Fld(1:(i-1)).Width]),-1);
    switch S.Fld(i).Type
        case '2' % binary (int16)
            ReadFld=fread(fid,[S.NRec 1],'int16',S.NBytesRec-2);
        case {'I','4','MB'} % binary (int32)
            ReadFld=fread(fid,[S.NRec 1],'int32',S.NBytesRec-4);
        case {'O','8'} % binary (float64)
            ReadFld=fread(fid,[S.NRec 1],'float64',S.NBytesRec-8);
        case {'B','G','M','P','C','D','F','N','L'}
            type = S.Fld(i).Type;
            StFormat=sprintf('%%%ic',S.NBytesRec);
            switch type
                case {'C','L'}
                    ReadFld=repmat(' ',S.NRec,S.Fld(i).Width);
                case {'M','B','G','P'}
                    Format=sprintf('%%%id',S.Fld(i).Width);
                    ReadFld=zeros(S.NRec,1);
                case 'D' % date: YYYYMMDD, Width=8
                    Format='%4i%2i%2i';
                    ReadFld=zeros(S.NRec,1);
                case {'F','N'} % F floating point, or N numeric
                    Format=sprintf('%%%if',S.Fld(i).Width);
                    ReadFld=zeros(S.NRec,1);
            end
            %
            NPerRead=max(1,floor(10000/S.NBytesRec));
            ix=0;
            while ix<S.NRec
                if S.NRec-ix<NPerRead
                    NPerRead = S.NRec-ix;
                end
                St=fscanf(fid,StFormat,[1 NPerRead]);
                if length(St)<NPerRead*S.NBytesRec
                    St(NPerRead*S.NBytesRec)=' ';
                end
                St=reshape(St,[S.NBytesRec NPerRead]);
                St=St(1:S.Fld(i).Width,:);
                switch type
                    case 'C'
                        % C character
                        Tmp=St';
                    case 'D' % date: YYYYMMDD, Width=8
                        ValidValues = all((St>=48 & St<=57) | St=='/' | St==' ') & ~all(St==' ');
                        if ~all(ValidValues)
                            St = St(:,ValidValues);
                        end
                        if sum(St(:)=='/')==2*size(St,2)
                            Format='%d/%d/%d'; % assumed MM/DD/YYYY
                            T=sscanf(St,Format,[3 NPerRead]);
                            Tmp=datenum(T(3,:),T(1,:),T(2,:))';
                        else
                            T=sscanf(St,Format,[3 NPerRead]);
                            Tmp=datenum(T(1,:),T(2,:),T(3,:))';
                        end
                        if ~all(ValidValues)
                            Tmp2=NaN(length(ValidValues),1);
                            Tmp2(ValidValues)=Tmp;
                            Tmp=Tmp2;
                        end
                    case {'M','B','G','P'}
                        % M dbt memo index
                        % B binary: binary data in .dbt
                        % G general (FoxPro)
                        % P picture (FoxPro): binary data in .ftp
                        EmptyValues = all(St==' ');
                        if any(EmptyValues)
                            St = St(:,~EmptyValues);
                        end
                        Tmp=sscanf(St,Format,[NPerRead 1]);
                        if any(EmptyValues)
                            Tmp2=NaN(length(EmptyValues),1);
                            Tmp2(~EmptyValues)=Tmp;
                            Tmp=Tmp2;
                        end
                    case {'F','N'} % F floating point, or N numeric
                        ValidValues = ~all(St==' ');
                        if ~all(ValidValues)
                            St = St(:,ValidValues);
                        end
                        %
                        % You would expect that this would be as simple as
                        %
                        %Tmp=sscanf(St,Format,[NPerRead 1]);
                        %
                        % However, the simple testcase
                        %
                        %   sscanf(' 1.3  457.9 ','%4g',3)
                        %
                        % shows that behaviour is different than expected.
                        % You would expect to receive [1.3 45 7.9] but you
                        % receive [1.3 457. 9]. Hence spaces are not
                        % counted and the number (4) in the format string
                        % indicates the maximum number of characters to
                        % read for one value.
                        %
                        % The following is a workaround that works always.
                        %
                        St(end+1,:)=' ';
                        Tmp=sscanf(St,'%f',[NPerRead 1]);
                        %
                        % Tmp=str2num(St')
                        %
                        % works as well, but is much slower for large data
                        % sets.
                        %
                        if ~all(ValidValues)
                            Tmp2=NaN(length(ValidValues),1);
                            Tmp2(ValidValues)=Tmp;
                            Tmp=Tmp2;
                        end
                    case 'L' % logical (T:t,F:f,Y:y,N:n,? or space)
                        Tmp=upper(St');
                        Tmp(Tmp=='Y')='T';
                        Tmp(Tmp=='N')='F';
                end
                ReadFld(ix+(1:NPerRead),:)=Tmp;
                ix = ix+NPerRead;
            end
            %
            switch type
                case 'C'
                    ReadFld=cellstr(ReadFld);
            end
        otherwise
            error('Invalid dBase field type.');
    end
    if isequal(Records,0)
        Dbs{j}=ReadFld;
    else
        Dbs{j}=ReadFld(Records);
    end
end
fclose(fid);
