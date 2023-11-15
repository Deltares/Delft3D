function Out=waqua(cmd,varargin)
%WAQUA Read SIMONA SDS files (low level).
%
%   FileData = WAQUA('openpro-r',FileName)
%   Open and read data from a WAQPRO result file.
%
%   FileData = waqua('open',FileName)
%   Open a SIMONA SDS file.
%
%   Bool = WAQUA('exists',FileData,ExperimentName,Characteristic);
%   Check whether the specified characteristic is located on the
%   SDS file.
%
%   Data = WAQUA('read',FileData,ExperimentName,Characteristic,Timesteps,Index);
%   Read data from an SDS file given the experiment name and the name of
%   of the characteristic. For time dependent data a timestep is required.
%   By default all timesteps are loaded, specify [] to read the timesteps but
%   not the data itself. Using the Index command a subset of the array can be
%   obtained.
%
%   WAQUA('sidsview',FileData)
%   Give standard listing of file contents.
%
%   See also WAQUAIO

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
        Out=[];
    end
    return
end

switch cmd
    case 'opensimetf'
        Structure = read_simetf_messages(varargin{:});
        if nargout>0
            Out = Structure;
        end
    case 'openpro-r'
        Structure=Local_open_pror(varargin{:});
        if nargout>0
            if ~isstruct(Structure)
                Out=[];
            elseif strcmp(Structure.Check,'NotOK')
                Out=[];
            else
                Out=Structure;
            end
        end
    case {'opensds','open'}
        Structure=Local_open_sds(varargin{:});
        if nargout>0
            if ~isstruct(Structure)
                Out=[];
            elseif strcmp(Structure.Check,'NotOK')
                Out=[];
            else
                Out=Structure;
            end
        end
    case {'readsds','read'}
        Out=Local_read_sds(varargin{:});
    case {'exists'}
        Out=Local_char_exists(varargin{:});
    case {'size'}
        Out=Local_char_size(varargin{:});
    case 'sidsview'
        sidsview(varargin{:});
    otherwise
        uiwait(msgbox('unknown command','modal'));
end


function Structure=Local_open_pror(filename)
Structure.Check='NotOK';
if nargin==0
    [fn,fp]=uigetfile('waqpro-r.*');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'rt','n','US-ASCII');

Line=fgetl(fid);
i=1;
Structure.Field.Name=[];
Structure.Field.Step=[];
Structure.Field.Time=[];
Structure.Field.Offset=[];
Structure.Field.NLines=[];
while ischar(Line)
    if ~isempty(Line)
        if Line(1)=='0'
            At=min(strfind(Line,' AT '));
            Name=Line(2:(At-1));
            Step=eval(Line(min(strfind(Line,'NST='))+4:min(strfind(Line,' ( ='))-1));
            Time=eval(Line(min(strfind(Line,' ( ='))+4:min(strfind(Line,' ELAPSED'))-1));
            if (i>1) & isequal(Name,Structure.Field(i-1).Name) & Step==Structure.Field(i-1).Step & Time==Structure.Field(i-1).Time
                i=i-1;
            else
                Structure.Field(i).Name=Name;
                Structure.Field(i).Step=Step;
                Structure.Field(i).Time=Time;
            end
            Line=fgetl(fid); % skip
            Line=fgetl(fid); % skip
            Structure.Field(i).Offset(end+1)=ftell(fid);
            N=0;
            Line=fgetl(fid);
            while ischar(Line) & ~isempty(Line) & Line(1)==' '
                Line=fgetl(fid);
                N=N+1;
            end
            Structure.Field(i).NLines(end+1)=N;
            i=i+1;
        else % non-empty line before 0-line (comment)
            Line=fgetl(fid);
        end
    else % empty line
        Line=fgetl(fid);
    end
end

fclose(fid);
Structure.Check='OK';


function Structure=Local_open_sds(filename,wqdebug)
% SDS - SIMONA Data Storage file
Structure.Check='NotOK';
if nargin<2
    wqdebug=0;
end

if (nargin==0) | strcmp(filename,'')
    [fn,fp]=uigetfile('SDS-*');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'r','b');

lasterr('');
try
    if wqdebug
        wqdebug=fopen([tempdir 'waqua.dbg'],'w');
        if wqdebug<=0
            wqdebug=0;
            warning(sprintf('Cannot open debug file: %swaqua.dbg.',tempdir));
        else
            fprintf(1,'Writing to debug file: %swaqua.dbg ...\n',tempdir);
        end
        YesNoStr={'No' 'Yes'};
    end

    Structure.Format='b';
    Structure.FileName=filename;
    if wqdebug
        fprintf(wqdebug,'Opening %s as big-endian ...\n',filename);
    end
    X=fread(fid,[1 20],'int32'); % <-------- info
    Structure.FileCreationDate=X2Date(X(9:10));
    Structure.MaxNWriteProg=X(3);
    Structure.NWriteProg=X(12);
    if Structure.MaxNWriteProg<1 | Structure.MaxNWriteProg>100000,
        if wqdebug
            fprintf(wqdebug,'Maximum number of writing programs: %i.\n',Structure.MaxNWriteProg);
            fprintf(wqdebug,'Invalid maximum number of writing programs.\n');
            fprintf(wqdebug,'Reopening %s as little-endian ...\n',filename);
        end
        fclose(fid);
        fid=fopen(filename,'r','l'); % might also be vaxd, vaxg, cray, ieee-le.l64, ieee-be.l64

        Structure.Format='l';

        X=fread(fid,[1 20],'int32'); % <-------- info
        Structure.FileCreationDate=X2Date(X(9:10));
        Structure.MaxNWriteProg=X(3);
        Structure.NWriteProg=X(12);
        if wqdebug
            fprintf(wqdebug,'File creation date %s\n',datestr(Structure.FileCreationDate));
            fprintf(wqdebug,'Maximum number of writing programs: %i.\n',Structure.MaxNWriteProg);
            fprintf(wqdebug,'Number of writing programs: %i.\n',Structure.NWriteProg);
        end
        if Structure.MaxNWriteProg<1 | Structure.MaxNWriteProg>100000,
            if wqdebug
                fprintf(wqdebug,'Maximum number of writing programs still invalid.\n');
            end
            error('Invalid number of writing programs.');
        end
    elseif wqdebug
        fprintf(wqdebug,'File creation date %s\n',datestr(Structure.FileCreationDate));
        fprintf(wqdebug,'Maximum number of writing programs: %i.\n',Structure.MaxNWriteProg);
        fprintf(wqdebug,'Number of writing programs: %i.\n',Structure.NWriteProg);
    end

    RecSize=2048;

    Structure.OffsetAdmin1=X(5);
    Structure.OffsetAdmin2=X(6);
    Structure.OffsetChar1=X(16);
    Structure.Data1=X([1 2 4 7:8 11 13:15 17:20]);
    if wqdebug
        fprintf(wqdebug,'\nSkipping unknown data:');
        fprintf(wqdebug,' %i',Structure.Data1);
        fprintf(wqdebug,'\n\n');
    end

    X=fread(fid,1,'int32'); % filetype: SIMONA SDS FILE
    if X>76
        error('Invalid length of file type identification string.');
    end
    Str=char(fread(fid,[1 76],'uchar'));
    if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
    Structure.FileType=Str(1:X);
    if wqdebug
        fprintf(wqdebug,'\nFile type: %s\n',Str);
    end
    if ~strcmp(lower(Structure.FileType),'simona sds file')
        error('Not Simona SDS file? FileType=''%s''',Structure.FileType)
    end
    if wqdebug
        fprintf(wqdebug,'Accepted as Simona SDS file.\n\n');
    end

    X=fread(fid,1,'int32'); % SDS-...
    Str=char(fread(fid,[1 76],'uchar'));
    if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
    Structure.FileName1=Str(1:X);
    if any(Structure.FileName1<32 | Structure.FileName1>122)
        error('Invalid file: %s',Structure.FileName1)
    end

    X=fread(fid,1,'int32'); % HP 9000
    Str=char(fread(fid,[1 76],'uchar'));
    if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
    Structure.Platform=deblank(Str(1:X));

    X=fread(fid,1,'int32'); % SDS-...
    Str=char(fread(fid,[1 76],'uchar'));
    if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
    Structure.FileName2=Str(1:X);
    if wqdebug
        fprintf(wqdebug,'First filename stored in file: %s.\n',Structure.FileName1);
        fprintf(wqdebug,'Second filename stored in file: %s.\n',Structure.FileName2);
        fprintf(wqdebug,'Created on: %s.\n\n',Structure.Platform);
    end


    X=fread(fid,1,'int32'); % blank space
    Str=char(fread(fid,[1 76],'uchar'));
    if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
    Structure.WritingProg=Str(1:X);
    if wqdebug & ~isempty(deblank(Structure.WritingProg))
        fprintf(wqdebug,'\n**********************************\n');
        fprintf(wqdebug,'WARNING: File is currently accessed by: %s.\nOr this program has not properly shut down.',Structure.WritingProg);
        fprintf(wqdebug,'\n**********************************\n\n');
    end

    Structure.WriteProg(1).Name='';
    for i=1:Structure.MaxNWriteProg
        X=fread(fid,1,'int32');
        if (X==0)
            X=fread(fid,[1 19],'int32');
        else
            StrL=ceil(X/4);
            Str=char(fread(fid,[1 StrL*4],'uchar'));
            if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
            Structure.WriteProg(end).Name=Str(1:X);
            X=fread(fid,[1 19-StrL],'int32');
            Structure.WriteProg(end).Date=X2Date(X(1:2));
            Structure.WriteProg(end+1).Name='';
        end
    end
    Structure.WriteProg(end)=[];
    if wqdebug
        fprintf(wqdebug,'The following programs have used this file:\n');
        for i=1:length(Structure.WriteProg)
            fprintf(wqdebug,'  %s on %s.\n',Structure.WriteProg(i).Name,datestr(Structure.WriteProg(i).Date));
        end
        fprintf(wqdebug,'\n');
        fprintf(wqdebug,'Reading first administration ...\n' );
    end

    fseek(fid,(Structure.OffsetAdmin1-1)*RecSize,-1);

    X=fread(fid,[1 25],'int32');
    if strcmp(Structure.Format,'l') & isequal(X(1),0)
        %Well, with some PC versions (at least on Windows, I don't
        %know about Linux) the SDS file is written using data blocks
        %of 8192 bytes. However, only the first 2048 bytes are used.
        %Offsets are based on the non-extended file size.
        RecSize=8192;
        if wqdebug
            fprintf(wqdebug,'Encountered sequence number zero for administration.\nSwitching to a record size of 8192.\n');
        end
        fseek(fid,(Structure.OffsetAdmin1-1)*RecSize,-1);
        X=fread(fid,[1 25],'int32');
    end
    Icim=isequal(RecSize,8192);
    Structure.Icim=Icim;
    Structure.SequenceNrAdmin=X(1);
    Structure.MaxNumEntries=X(3);
    Structure.NumEntries=X(4);
    Structure.MaxNumArrays=X(7);
    Structure.NumArrays=X(8);
    Structure.LastChanged=X2Date(X(10:11));
    Structure.MaxNumExperiments=X(16);
    Structure.NumExperiments=X(17);
    Structure.MaxNumWriteDates=X(18);
    Structure.Data2=X([2 5 6 9 12:15 19:25]);
    if wqdebug
        fprintf(wqdebug,'  Administration number: %i\n',Structure.SequenceNrAdmin);
        fprintf(wqdebug,'  Number of experiments: %i (max. %i)\n',Structure.NumExperiments,Structure.MaxNumExperiments);
        fprintf(wqdebug,'  Number of entries: %i (max. %i)\n',Structure.NumEntries,Structure.MaxNumEntries);
        fprintf(wqdebug,'  Number of arrays: %i (max. %i)\n',Structure.NumArrays,Structure.MaxNumArrays);
        fprintf(wqdebug,'  Last changed on %s\n',datestr(Structure.LastChanged));
        fprintf(wqdebug,'  Skipping unknown data:');
        fprintf(wqdebug,' %i',Structure.Data2);
        fprintf(wqdebug,'\n\n');
        fprintf(wqdebug,'  Analyzing experiments ...\n' );
    end

    for expment=1:Structure.MaxNumExperiments
        X=fread(fid,[1 1],'int32'); % length of string
        Str=char(fread(fid,[1 76],'uchar'));
        if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
        Name=Str(1:X);
        X=fread(fid,[1 7],'int32');
        if expment<=Structure.NumExperiments
            Structure.Experiment(expment).Name=Name;
            Structure.Experiment(expment).MaxNumChar=X(1);
            Structure.Experiment(expment).NumChar=X(2);
            Structure.Experiment(expment).NWriteProg=X(7);
            Structure.Experiment(expment).Data=X(3:6);
            if wqdebug
                fprintf(wqdebug,'    Experiment name: %s\n',Structure.Experiment(expment).Name);
                fprintf(wqdebug,'      Number of characteristics: %i (max. %i)\n',Structure.Experiment(expment).NumChar,Structure.Experiment(expment).MaxNumChar);
                fprintf(wqdebug,'      Number of writing programs: %i (central: %i)\n',Structure.Experiment(expment).NWriteProg,Structure.NWriteProg);
                fprintf(wqdebug,'      Unknown data:');
                fprintf(wqdebug,' %i',Structure.Experiment(expment).Data);
                fprintf(wqdebug,'\n\n');
            end
        end
    end

    if wqdebug
        fprintf(wqdebug,'  Analyzing arrays ...\n' );
    end

    for arr=1:Structure.NumArrays
        X=fread(fid,1,'int32');
        Str=char(fread(fid,[1 76],'uchar'));
        if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
        Structure.Array(arr).Name=Str(1:X);
        X=fread(fid,1,'int32');
        Structure.Array(arr).Index=X;
        if wqdebug
            fprintf(wqdebug,'    Array name: %s\n',Structure.Array(arr).Name);
            fprintf(wqdebug,'      corresponding entry index: %i\n\n',Structure.Array(arr).Index);
        end
    end

    if Structure.MaxNumEntries>Structure.NumArrays % MaxNumArrays is invalid!!
        if Icim
            IcimJumpStep(fid,84*(Structure.MaxNumEntries-Structure.NumArrays))
        else
            X=fread(fid,84*(Structure.MaxNumEntries-Structure.NumArrays),'uchar');
        end
    end

    if wqdebug
        fprintf(wqdebug,'  Analyzing entries ...\n' );
        dtstr={'integer','single precision float','?','integer coded text'};
        itstr{100}='';
        itstr(1:100)={' '};
        itstr(1:7)={'--','mesh','problem','solution','input','coefficient','boundary condition'};
        itstr{97+1}='flag';
        atstr{100}=' ';
        atstr(1:100)={'subarray'};
        atstr(1)={'single array'};
        atstr{100}='compound (main array)';
    end

    for entry=1:Structure.NumEntries
        if Icim
            X=ReadIcimInt32(fid,32);
        else
            X=fread(fid,[1 32],'int32');
        end

        Y=X2Y(X(1));
        Structure.Entry(entry).DataType=Y(1);  %  0 integer
        %  1 single precision
        %  2 ?
        %  3 integer coded text
        Structure.Entry(entry).InfoType=Y(2);  %  0 --
        %  1 mesh
        %  2 problem
        %  3 solution
        %  4 input
        %  5 coefficient
        %  6 boundary condition
        % 97 flag
        Structure.Entry(entry).ArrayType=Y(3); %  0 single array
        % ** subarray
        % 99 compound (main array)
        Structure.Entry(entry).BlockSize=X(2);
        Structure.Entry(entry).Data1=X(3:5);
        Structure.Entry(entry).BlockSizeInclHdr=X(6);
        Structure.Entry(entry).CreationDate=X2Date(X(7:8));
        Structure.Entry(entry).LastChanged=X2Date(X(9:10));
        Structure.Entry(entry).MaxNumIter=X(11);
        Structure.Entry(entry).NumIter=X(12);
        Structure.Entry(entry).Offset=((X(13)-1)*512+X(14)-1)*4;
        if Structure.Entry(entry).NumIter
            Structure.Entry(entry).DataOffset=((X(15)-1)*512+X(16)-1)*4;
        else
            Structure.Entry(entry).DataOffset=0;
        end
        Structure.Entry(entry).Data2=X(17:32);
        if wqdebug
            fprintf(wqdebug,'    Entry number: %i\n',entry);
            fprintf(wqdebug,'      Entry data type: %i (%s)\n',Structure.Entry(entry).DataType,dtstr{Structure.Entry(entry).DataType+1});
            fprintf(wqdebug,'      Entry info type: %i (%s)\n',Structure.Entry(entry).InfoType,itstr{Structure.Entry(entry).InfoType+1});
            fprintf(wqdebug,'      Entry array type: %i (%s)\n',Structure.Entry(entry).ArrayType,atstr{Structure.Entry(entry).ArrayType+1});
            fprintf(wqdebug,'      Block size: %i (%i incl. header)\n',Structure.Entry(entry).BlockSize,Structure.Entry(entry).BlockSizeInclHdr);
            fprintf(wqdebug,'      Skipping unknown data:');
            fprintf(wqdebug,' %i',Structure.Entry(entry).Data1);
            fprintf(wqdebug,'\n');
            fprintf(wqdebug,'      Created on %s\n',datestr(Structure.Entry(entry).CreationDate));
            fprintf(wqdebug,'      Last changed on %s\n',datestr(Structure.Entry(entry).LastChanged));
            fprintf(wqdebug,'      Number of iter: %i (max. %i)\n',Structure.Entry(entry).NumIter,Structure.Entry(entry).MaxNumIter);
            fprintf(wqdebug,'      Offset: %i\n',Structure.Entry(entry).Offset);
            fprintf(wqdebug,'      Data offset: %i\n',Structure.Entry(entry).DataOffset);
            fprintf(wqdebug,'      Skipping unknown data:');
            fprintf(wqdebug,' %i',Structure.Entry(entry).Data2);
            fprintf(wqdebug,'\n\n');
        end
    end
    %X=fread(fid,[32 Structure.MaxNumEntries-Structure.NumEntries],'int32');

    if wqdebug
        fprintf(wqdebug,'Reading second administration ...\n' );
        fprintf(wqdebug,'Reporting ...\n\n' );
    end

    %[ftell(fid) 49*2048 49*2048-ftell(fid)]
    fseek(fid,(Structure.OffsetAdmin2-1)*RecSize,-1);

    X=fread(fid,[1 25],'int32');
    Structure2.SequenceNrAdmin=X(1);
    Structure2.MaxNumEntries=X(3);
    Structure2.NumEntries=X(4);
    Structure2.MaxNumArrays=X(7);
    Structure2.NumArrays=X(8);
    Structure2.LastChanged=X2Date(X(10:11));
    Structure2.MaxNumExperiments=X(16);
    Structure2.NumExperiments=X(17);
    Structure2.Data2=X([2 5 6 9 12:15 18:25]);
    if wqdebug
        fprintf(wqdebug,'  Administration number: %i [%i]\n',Structure.SequenceNrAdmin,Structure2.SequenceNrAdmin);
        fprintf(wqdebug,'  Number of experiments: %i (max. %i) [%i (max. %i)]\n',Structure.NumExperiments,Structure.MaxNumExperiments,Structure2.NumExperiments,Structure2.MaxNumExperiments);
        fprintf(wqdebug,'  Number of entries: %i (max. %i) [%i (max. %i)]\n',Structure.NumEntries,Structure.MaxNumEntries,Structure2.NumEntries,Structure2.MaxNumEntries);
        fprintf(wqdebug,'  Number of arrays: %i (max. %i) [%i (max. %i)]\n',Structure.NumArrays,Structure.MaxNumArrays,Structure2.NumArrays,Structure2.MaxNumArrays);
        fprintf(wqdebug,'  Last changed on %s [%s]\n',datestr(Structure.LastChanged),datestr(Structure2.LastChanged));
        fprintf(wqdebug,'  Skipping unknown data:');
        fprintf(wqdebug,' %i',Structure.Data2);
        fprintf(wqdebug,'\n                       [');
        fprintf(wqdebug,' %i',Structure2.Data2);
        fprintf(wqdebug,']\n\n');
        fprintf(wqdebug,'  Analyzing experiments ...\n' );
    end

    for expment=1:Structure2.MaxNumExperiments
        X=fread(fid,[1 1],'int32'); % length of string
        Str=char(fread(fid,[1 76],'uchar'));
        if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
        Name=Str(1:X);
        X=fread(fid,[1 7],'int32');
        if expment<=Structure2.NumExperiments
            Structure2.Experiment(expment).Name=Name;
            Structure2.Experiment(expment).MaxNumChar=X(1);
            Structure2.Experiment(expment).NumChar=X(2);
            Structure2.Experiment(expment).NWriteProg=X(7);
            Structure2.Experiment(expment).Data=X(3:6);
            if wqdebug
                if expment<=Structure.NumExperiments & ~isequal(Structure.Experiment(expment),Structure2.Experiment(expment))
                    fprintf(wqdebug,'    ********* changed!\n');
                    fprintf(wqdebug,'    Experiment name: %s [%s]\n',Structure.Experiment(expment).Name,Structure2.Experiment(expment).Name);
                    fprintf(wqdebug,'      Number of characteristics: %i (max. %i) [%i (max. %i)]\n',Structure.Experiment(expment).NumChar,Structure.Experiment(expment).MaxNumChar,Structure2.Experiment(expment).NumChar,Structure2.Experiment(expment).MaxNumChar);
                    fprintf(wqdebug,'      Number of writing programs: %i [%i]\n',Structure.Experiment(expment).NWriteProg,Structure2.Experiment(expment).NWriteProg);
                    fprintf(wqdebug,'      Unknown data:');
                    fprintf(wqdebug,' %i',Structure.Experiment(expment).Data);
                    fprintf(wqdebug,'\n                  [');
                    fprintf(wqdebug,' %i',Structure2.Experiment(expment).Data);
                    fprintf(wqdebug,']\n\n');
                else
                    if expment>Structure.NumExperiments
                        fprintf(wqdebug,'    ********* new!\n');
                    end
                    fprintf(wqdebug,'    Experiment name: %s\n',Structure2.Experiment(expment).Name);
                    fprintf(wqdebug,'      Number of characteristics: %i (max. %i)\n',Structure2.Experiment(expment).NumChar,Structure2.Experiment(expment).MaxNumChar);
                    fprintf(wqdebug,'      Number of writing programs: %i (central: %i)\n',Structure2.Experiment(expment).NWriteProg,Structure.NWriteProg);
                    fprintf(wqdebug,'      Unknown data:');
                    fprintf(wqdebug,' %i',Structure2.Experiment(expment).Data);
                    fprintf(wqdebug,'\n\n');
                end
            end
        end
    end

    if wqdebug
        fprintf(wqdebug,'  Analyzing arrays ...\n' );
    end

    for arr=1:Structure2.NumArrays
        X=fread(fid,1,'int32');
        Str=char(fread(fid,[1 76],'uchar'));
        if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
        Structure2.Array(arr).Name=Str(1:X);
        X=fread(fid,1,'int32');
        Structure2.Array(arr).Index=X;
        if wqdebug
            if arr<=Structure.NumArrays & ~isequal(Structure.Array(arr),Structure2.Array(arr))
                fprintf(wqdebug,'    ********* changed!\n');
                fprintf(wqdebug,'    Array name: %s [%s]\n',Structure.Array(arr).Name,Structure2.Array(arr).Name);
                fprintf(wqdebug,'      corresponding entry index: %i [%i]\n\n',Structure.Array(arr).Index,Structure2.Array(arr).Index);
            else
                if arr>Structure.NumArrays
                    fprintf(wqdebug,'    ********* new!\n');
                end
                fprintf(wqdebug,'    Array name: %s\n',Structure2.Array(arr).Name);
                fprintf(wqdebug,'      corresponding entry index: %i\n\n',Structure2.Array(arr).Index);
            end
        end
    end
    if Structure2.MaxNumEntries>Structure2.NumArrays % MaxNumArrays is invalid!!
        if Icim
            IcimJumpStep(fid,84*(Structure2.MaxNumEntries-Structure2.NumArrays))
        else
            X=fread(fid,84*(Structure2.MaxNumEntries-Structure2.NumArrays),'uchar');
        end
    end

    if wqdebug
        fprintf(wqdebug,'  Analyzing entries ...\n' );
    end

    for entry=1:Structure2.NumEntries
        if Icim
            X=ReadIcimInt32(fid,32);
        else
            X=fread(fid,[1 32],'int32');
        end

        Y=X2Y(X(1));
        Structure2.Entry(entry).DataType=Y(1);  %  0 integer
        %  1 single precision
        %  2 ?
        %  3 integer coded text
        Structure2.Entry(entry).InfoType=Y(2);  %  0 --
        %  1 mesh
        %  2 problem
        %  3 solution
        %  4 input
        %  5 coefficient
        %  6 boundary condition
        % 97 flag
        Structure2.Entry(entry).ArrayType=Y(3); %  0 single array
        % ** subarray
        % 99 compound (main array)
        Structure2.Entry(entry).BlockSize=X(2);
        Structure2.Entry(entry).Data1=X(3:5);
        Structure2.Entry(entry).BlockSizeInclHdr=X(6);
        Structure2.Entry(entry).CreationDate=X2Date(X(7:8));
        Structure2.Entry(entry).LastChanged=X2Date(X(9:10));
        Structure2.Entry(entry).MaxNumIter=X(11);
        Structure2.Entry(entry).NumIter=X(12);
        Structure2.Entry(entry).Offset=((X(13)-1)*512+X(14)-1)*4;
        if Structure2.Entry(entry).NumIter
            Structure2.Entry(entry).DataOffset=((X(15)-1)*512+X(16)-1)*4;
        else
            Structure2.Entry(entry).DataOffset=0;
        end
        Structure2.Entry(entry).Data2=X(17:32);
        if wqdebug
            fprintf(wqdebug,'    Entry number: %i\n',entry);
            if entry<=Structure.NumEntries & ~isequal(Structure.Entry(entry),Structure2.Entry(entry))
                fprintf(wqdebug,'      ********* changed!\n');
                fprintf(wqdebug,'      Entry data type: %i (%s) [%i (%s)]\n',Structure.Entry(entry).DataType,dtstr{Structure.Entry(entry).DataType+1},Structure2.Entry(entry).DataType,dtstr{Structure2.Entry(entry).DataType+1});
                fprintf(wqdebug,'      Entry info type: %i (%s) [%i (%s)]\n',Structure.Entry(entry).InfoType,itstr{Structure.Entry(entry).InfoType+1},Structure2.Entry(entry).InfoType,itstr{Structure2.Entry(entry).InfoType+1});
                fprintf(wqdebug,'      Entry array type: %i (%s) [%i (%s)]\n',Structure.Entry(entry).ArrayType,atstr{Structure.Entry(entry).ArrayType+1},Structure2.Entry(entry).ArrayType,atstr{Structure2.Entry(entry).ArrayType+1});
                fprintf(wqdebug,'      Block size: %i (%i incl. header) [%i (%i incl. header)]\n',Structure.Entry(entry).BlockSize,Structure.Entry(entry).BlockSizeInclHdr,Structure2.Entry(entry).BlockSize,Structure2.Entry(entry).BlockSizeInclHdr);
                fprintf(wqdebug,'      Skipping unknown data:');
                fprintf(wqdebug,' %i',Structure.Entry(entry).Data1);
                fprintf(wqdebug,'\n                           [');
                fprintf(wqdebug,' %i',Structure2.Entry(entry).Data1);
                fprintf(wqdebug,']\n');
                fprintf(wqdebug,'      Created on %s [%s]\n',datestr(Structure.Entry(entry).CreationDate),datestr(Structure2.Entry(entry).CreationDate));
                fprintf(wqdebug,'      Last changed on %s [%s]\n',datestr(Structure.Entry(entry).LastChanged),datestr(Structure2.Entry(entry).LastChanged));
                fprintf(wqdebug,'      Number of timesteps/iterations: %i (max. %i) [%i (max. %i)]\n',Structure.Entry(entry).NumIter,Structure.Entry(entry).MaxNumIter,Structure2.Entry(entry).NumIter,Structure2.Entry(entry).MaxNumIter);
                fprintf(wqdebug,'      Offset: %i [%i]\n',Structure.Entry(entry).Offset,Structure2.Entry(entry).Offset);
                fprintf(wqdebug,'      Data offset: %i [%i]\n',Structure.Entry(entry).DataOffset,Structure2.Entry(entry).DataOffset);
                fprintf(wqdebug,'      Skipping unknown data:');
                fprintf(wqdebug,' %i',Structure.Entry(entry).Data2);
                fprintf(wqdebug,'\n                           [');
                fprintf(wqdebug,' %i',Structure.Entry(entry).Data2);
                fprintf(wqdebug,']\n\n');
            else
                if entry>Structure.NumEntries
                    fprintf(wqdebug,'      ********* new!\n');
                end
                fprintf(wqdebug,'      Entry data type: %i (%s)\n',Structure2.Entry(entry).DataType,dtstr{Structure2.Entry(entry).DataType+1});
                fprintf(wqdebug,'      Entry info type: %i (%s)\n',Structure2.Entry(entry).InfoType,itstr{Structure2.Entry(entry).InfoType+1});
                fprintf(wqdebug,'      Entry array type: %i (%s)\n',Structure2.Entry(entry).ArrayType,atstr{Structure2.Entry(entry).ArrayType+1});
                fprintf(wqdebug,'      Block size: %i (%i incl. header)\n',Structure2.Entry(entry).BlockSize,Structure2.Entry(entry).BlockSizeInclHdr);
                fprintf(wqdebug,'      Skipping unknown data:');
                fprintf(wqdebug,' %i',Structure2.Entry(entry).Data1);
                fprintf(wqdebug,'\n');
                fprintf(wqdebug,'      Created on %s\n',datestr(Structure2.Entry(entry).CreationDate));
                fprintf(wqdebug,'      Last changed on %s\n',datestr(Structure2.Entry(entry).LastChanged));
                fprintf(wqdebug,'      Number of iter: %i (max. %i)\n',Structure2.Entry(entry).NumIter,Structure2.Entry(entry).MaxNumIter);
                fprintf(wqdebug,'      Offset: %i\n',Structure2.Entry(entry).Offset);
                fprintf(wqdebug,'      Data offset: %i\n',Structure2.Entry(entry).DataOffset);
                fprintf(wqdebug,'      Skipping unknown data:');
                fprintf(wqdebug,' %i',Structure2.Entry(entry).Data2);
                fprintf(wqdebug,'\n\n');
            end
        end
    end
    %X=fread(fid,[32 Structure2.MaxNumEntries-Structure2.NumEntries],'int32');
    Structure.SecondAdministration=Structure2;

    for expment=1:Structure.NumExperiments
        if wqdebug
            fprintf(wqdebug,'Experiment: %i (%s)\n',expment,Structure.Experiment(expment).Name);
        end
        
        %[ftell(fid) 88*2048 88*2048-ftell(fid)]
        fseek(fid,(Structure.Experiment(expment).Data(1)-3)*RecSize,-1);
        X=fread(fid,1,'int32'); % 0
        
        X=fread(fid,1,'int32');
        Str=char(fread(fid,[1 20],'uchar')); % waqua
        if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
        if wqdebug
            fprintf(wqdebug,'WAQUA label: %s\n',Str(1:X));
        end
        
        fseek(fid,ceil(ftell(fid)/RecSize)*RecSize,-1);
        X=fread(fid,1,'int32'); % 0
        X=fread(fid,1,'int32');
        Str=char(fread(fid,[1 20],'uchar')); % waqua
        if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
        if wqdebug
            fprintf(wqdebug,'WAQUA label: %s\n',Str(1:X));
        end
        
        fseek(fid,ceil(ftell(fid)/RecSize)*RecSize,-1);
        X=fread(fid,[1 2*Structure.MaxNumWriteDates+1],'int32'); % 9902, creation date, last change, zeros
        if wqdebug
            fprintf(wqdebug,'9902 flag: %i\n',X(1));
            fprintf(wqdebug,'Creation date: %s [%s]\n',datestr(X2Date(X(2:3))),datestr(Structure.FileCreationDate));
            fprintf(wqdebug,'Last changed: %s\n',datestr(X2Date(X(4:5))));
            fprintf(wqdebug,'Last changed: %s [%s]\n',datestr(X2Date(X(6:7))),datestr(Structure.LastChanged));
            fprintf(wqdebug,'\nReading first administration of characteristics ...\n');
        end
        
        for entry=1:Structure.Experiment(expment).NumChar
            if Icim
                X=ReadIcimInt32(fid,1);
                Str=ReadIcimUchar(fid,76);
            else
                X=fread(fid,1,'int32');
                Str=char(fread(fid,[1 76],'uchar'));
            end
            if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
            Structure.Experiment(expment).Char(entry).Name=Str(1:X);
            
            if Icim
                X=ReadIcimInt32(fid,13);
            else
                X=fread(fid,[1 13],'int32');
            end
            Structure.Experiment(expment).Char(entry).CreationDate=X2Date(X([3 2]));
            Structure.Experiment(expment).Char(entry).Index=X(1);
            Structure.Experiment(expment).Char(entry).Level=X(4:13);
            
            if wqdebug
                fprintf(wqdebug,'  Characteristic: %s\n',Structure.Experiment(expment).Char(entry).Name);
                fprintf(wqdebug,'  Creation date: %s\n',datestr(Structure.Experiment(expment).Char(entry).CreationDate));
                fprintf(wqdebug,'  Index: %i\n',Structure.Experiment(expment).Char(entry).Index);
                fprintf(wqdebug,'  Level vector:');
                fprintf(wqdebug,' %i',Structure.Experiment(expment).Char(entry).Level);
                fprintf(wqdebug,'\n\n');
            end
        end
        
        if Structure.Experiment(expment).MaxNumChar>Structure.Experiment(expment).NumChar
            if Icim
                IcimJumpStep(fid,132*(Structure.Experiment(expment).MaxNumChar-Structure.Experiment(expment).NumChar))
            else
                X=fread(fid,33*(Structure.Experiment(expment).MaxNumChar-Structure.Experiment(expment).NumChar),'int32');
            end
        end
    end

    if Icim
        X=ReadIcimInt32(fid,24);
    else
        X=fread(fid,[1 3],'int32'); % 9902, last change
        X=fread(fid,[1 21],'int32'); % 9902, creation date, last change, zeros
    end

    if wqdebug
        fprintf(wqdebug,'Reading second administration of characteristics ...\nReporting ...\n\n');
    end

    for expment=1:Structure.NumExperiments
        if wqdebug
            fprintf(wqdebug,'Experiment: %i (%s)\n',expment,Structure.Experiment(expment).Name);
        end
        for entry=1:Structure.Experiment(expment).NumChar
            if Icim
                X=ReadIcimInt32(fid,1);
                Str=ReadIcimUchar(fid,76);
            else
                X=fread(fid,1,'int32');
                Str=char(fread(fid,[1 76],'uchar'));
            end
            if strcmp(Structure.Format,'l'), Str=Swap4byte(Str); end
            Structure2.Experiment(expment).Char(entry).Name=Str(1:X);

            if Icim
                X=ReadIcimInt32(fid,13);
            else
                X=fread(fid,[1 13],'int32');
            end
            Structure2.Experiment(expment).Char(entry).CreationDate=X2Date(X([3 2]));
            Structure2.Experiment(expment).Char(entry).Index=X(1);
            Structure2.Experiment(expment).Char(entry).Level=X(4:13);
            if wqdebug
                if ~isequal(Structure2.Experiment(expment).Char(entry),Structure.Experiment(expment).Char(entry))
                    fprintf(wqdebug,'    ********* changed!\n');
                    fprintf(wqdebug,'  Characteristic: %s [%s]\n',Structure.Experiment(expment).Char(entry).Name,Structure2.Experiment(expment).Char(entry).Name);
                    fprintf(wqdebug,'  Creation date: %s [%s]\n',datestr(Structure.Experiment(expment).Char(entry).CreationDate),datestr(Structure2.Experiment(expment).Char(entry).CreationDate));
                    fprintf(wqdebug,'  Index: %i [%i]\n',Structure.Experiment(expment).Char(entry).Index,Structure2.Experiment(expment).Char(entry).Index);
                    fprintf(wqdebug,'  Level vector:');
                    fprintf(wqdebug,' %i',Structure.Experiment(expment).Char(entry).Level);
                    fprintf(wqdebug,'\n              [');
                    fprintf(wqdebug,' %i',Structure2.Experiment(expment).Char(entry).Level);
                    fprintf(wqdebug,']\n\n');
                else
                    fprintf(wqdebug,'  Characteristic: %s\n',Structure.Experiment(expment).Char(entry).Name);
                    fprintf(wqdebug,'  Creation date: %s\n',datestr(Structure.Experiment(expment).Char(entry).CreationDate));
                    fprintf(wqdebug,'  Index: %i\n',Structure.Experiment(expment).Char(entry).Index);
                    fprintf(wqdebug,'  Level vector:');
                    fprintf(wqdebug,' %i',Structure.Experiment(expment).Char(entry).Level);
                    fprintf(wqdebug,'\n\n');
                end
            end
        end
        if Structure.Experiment(expment).MaxNumChar>Structure.Experiment(expment).NumChar
            if Icim
                IcimJumpStep(fid,132*(Structure.Experiment(expment).MaxNumChar-Structure.Experiment(expment).NumChar))
            else
                X=fread(fid,33*(Structure.Experiment(expment).MaxNumChar-Structure.Experiment(expment).NumChar),'int32');
            end
        end
    end

    if Icim
        X=ReadIcimInt32(fid,4);
    else
        X=fread(fid,[1 4],'int32'); % 9902, last change
    end

    if wqdebug
        fprintf(wqdebug,'Reading additional administration of entries ...\n\n');
    end

    for entry=1:Structure.NumEntries
        if Icim
            IcimJumpTo(fid,Structure.Entry(entry).Offset);
            X=ReadIcimInt32(fid,1);
            if Structure.Entry(entry).ArrayType==99
                Structure.Entry(entry).Tree=ReadIcimInt32(fid,Structure.Entry(entry).BlockSize);
            else
                Structure.Entry(entry).Tree=[];
            end
        else
            fseek(fid,Structure.Entry(entry).Offset,-1);
            X=fread(fid,[1 1],'int32');
            if Structure.Entry(entry).ArrayType==99,
                Structure.Entry(entry).Tree=fread(fid,[1 Structure.Entry(entry).BlockSize],'int32');
            else
                Structure.Entry(entry).Tree=[];
            end
        end
        Y=X2Y(X);
        %Structure.Entry(entry).I2=X;
        Structure.Entry(entry).DataType2=Y(1);
        Structure.Entry(entry).InfoType2=Y(2);
        Structure.Entry(entry).ArrayType2=Y(3);
        if wqdebug
            fprintf(wqdebug,'  Entry number: %i\n',entry);
            %fprintf(wqdebug,'    Info2: %i\n',Structure.Entry(entry).I2);
            fprintf(wqdebug,'    Data type: %i [%i:%s]\n',Structure.Entry(entry).DataType2,Structure.Entry(entry).DataType,dtstr{Structure.Entry(entry).DataType+1});
            fprintf(wqdebug,'    Info type: %i [%i:%s]\n',Structure.Entry(entry).InfoType2,Structure.Entry(entry).InfoType,itstr{Structure.Entry(entry).InfoType+1});
            fprintf(wqdebug,'    Array type: %i [%i:%s]\n',Structure.Entry(entry).ArrayType2,Structure.Entry(entry).ArrayType,atstr{Structure.Entry(entry).ArrayType+1});
            if ~isempty(Structure.Entry(entry).Tree)
                fprintf(wqdebug,'    Tree:');
                plottree(Structure.Entry(entry).Tree,1,'         ',wqdebug)
            end
            fprintf(wqdebug,'\n');
        end
    end

    for entry=1:Structure.NumEntries
        if (Structure.Entry(entry).DataOffset~=0)
            if Icim
                IcimJumpTo(fid,Structure.Entry(entry).Offset);
                X=ReadIcimInt32(fid,1);
            else
                fseek(fid,Structure.Entry(entry).DataOffset,-1);
                X=fread(fid,[1 1],'int32');
            end
            Structure.Entry(entry).ID3=X;
        else
            Structure.Entry(entry).ID3=0;
        end
        if wqdebug
            fprintf(wqdebug,'  Entry number: %i\n',entry);
            fprintf(wqdebug,'    ID3: %i\n',Structure.Entry(entry).ID3);
        end
    end


    for expment=1:Structure.NumExperiments
        if wqdebug
            fprintf(wqdebug,'Experiment: %s\n',Structure.Experiment(expment).Name);
        end
        for entry=1:Structure.Experiment(expment).NumChar,
            [idx,Info]=Local_getinfo(Structure.Entry, ...
                Structure.Experiment(expment).Char(entry).Index, ...
                Structure.Experiment(expment).Char(entry).Level);
            Structure.Experiment(expment).Char(entry).DataIndex=idx;
            if wqdebug
                fprintf(wqdebug,'  Characteristic: %s\n',Structure.Experiment(expment).Char(entry).Name);
                fprintf(wqdebug,'    DataIndex: %i\n',idx);
            end
        end
    end

    Structure.Check='OK';
    if wqdebug
        fprintf(wqdebug,'\n\nThe SDS file was opened successfully.\n\n');
        fprintf(wqdebug,'SIDSview information:\n\n');
        sidsview(Structure,wqdebug);
    end
    %try
catch
end
if ~isempty(fopen(fid))
    fclose(fid);
end
if wqdebug
    fclose(wqdebug);
end
if ~isempty(lasterr)
    error(lasterr)
end


function Found=Local_char_exists(S,Expment,Entry)
Found=0;
if nargin<3
    return
end

%if strmatch(Expment,'-')
%  idx=Entry;
%else
if isempty(Expment) & length(S.Experiment)==1
    expment=1;
else
    expment=strmatch(Expment,{S.Experiment.Name},'exact');
    if ~isequal(size(expment),[1 1])
        return
    end
end
entry=strmatch(Entry,{S.Experiment(expment).Char.Name},'exact');
if ~isequal(size(entry),[1 1])
    return
end
Found=1;
%end


function Size=Local_char_size(S,Expment,Entry)
if nargin<3
    error('Not enough input arguments')
end

if isempty(Expment) & length(S.Experiment)==1
    expment = 1;
else
    expment = strmatch(Expment,{S.Experiment.Name},'exact');
    if ~isequal(size(expment),[1 1])
       error('Cannot find experiment %s',Expment)
    end
end
nr = strmatch(Entry,{S.Experiment(expment).Char.Name},'exact');
if ~isequal(size(nr),[1 1])
    error('Cannot find characteristic %s',Entry)
end
entry = S.Experiment(expment).Char(nr).DataIndex;
Size = S.Entry(entry).BlockSize;


function Data=Local_read_sds(S,Expment,Entry,tsteps,indices)
Data=[];
if nargin<3
    return
elseif nargin==3
    tsteps=0;
    indices=0;
elseif nargin==4
    indices=0;
end
min_index = min(indices);
max_index = max(indices);

if isequal(Expment,'-')
    idx=Entry;
else
    if isempty(Expment) & length(S.Experiment)==1
        expment=1;
    else
        expment=strmatch(Expment,{S.Experiment.Name},'exact');
        if ~isequal(size(expment),[1 1])
            error(['Experiment ',Expment,' not found.']);
        end
    end
    entry=strmatch(Entry,{S.Experiment(expment).Char.Name},'exact');
    if ~isequal(size(entry),[1 1])
        error(['Characteristic ',Entry,' not found.']);
    end
    idx=S.Experiment(expment).Char(entry).DataIndex;
end

fid=fopen(S.FileName,'r',S.Format);
if S.Icim
    IcimJumpTo(fid,S.Entry(idx).Offset);
    ID=ReadIcimInt32(fid,1);
else
    fseek(fid,S.Entry(idx).Offset,-1);
    ID=fread(fid,1,'int32');
end
switch S.Entry(idx).DataType2
    case 0
        if S.Icim
            Data=ReadIcimInt32(fid,S.Entry(idx).BlockSize);
        else
            Data=fread(fid,S.Entry(idx).BlockSize,'int32');
        end
    case 1
        if S.Icim
            Data=ReadIcimFloat32(fid,S.Entry(idx).BlockSize);
            if ~isequal(indices,0)
                Data=Data(indices);
            end
        else
            if isequal(indices,0)
                Data=fread(fid,S.Entry(idx).BlockSize,'float32');
            else
                indices = indices-min_index+1;
                fseek(fid,4*(min_index-1),0);
                Data=fread(fid,max_index-min_index+1,'float32');
                Data=Data(indices);
            end
        end
    case 3
        Data={};
        if S.Icim
            L=ReadIcimInt32(fid,1);
            while L~=ID
                X=ReadIcimUchar(fid,80);
                X=Swap4byte(X);
                Data{end+1}=X(1:L);
                L=ReadIcimInt32(fid,1);
            end
        else
            L=fread(fid,1,'int32');
            while L~=ID
                X=char(fread(fid,[1 80],'uchar'));
                if strcmp(S.Format,'l')
                    X=Swap4byte(X);
                end
                Data{end+1}=X(1:L);
                L=fread(fid,1,'int32');
            end
        end
    case 10,
        if S.Icim
            Data.SimTime=ReadIcimFloat64(fid,S.Entry(idx).MaxNumIter)';
            X=reshape(ReadIcimInt32(fid,5*S.Entry(idx).MaxNumIter),[5 S.Entry(idx).MaxNumIter]);
        else
            Data.SimTime=fread(fid,[1 S.Entry(idx).MaxNumIter],'float64')';
            X=fread(fid,[5 S.Entry(idx).MaxNumIter],'int32');
        end
        X=X(:,1:S.Entry(idx).NumIter);
        Data.Date=X2Date(X([1 2],:));
        Data.SimTime=Data.SimTime(1:S.Entry(idx).NumIter);
        Data.Date=Data.Date;
        I=[];
        Offset=((X(3,:)-1)*512+X(4,:)-1)*4;
        Size=X(5,:);
        allequalsize=all(Size==Size(1));
        Data.Data=[];
        if ~isempty(tsteps)
            if isequal(tsteps,0)
                tsteps=1:S.Entry(idx).NumIter;
            end
            Data.SimTime=Data.SimTime(tsteps);
            %Data.Date=Data.Date(tsteps);
            Offset=Offset(tsteps);
            Size=Size(tsteps);
            if allequalsize
                if isequal(indices,0)
                    Data.Data=zeros(length(tsteps),Size(1));
                else
                    Data.Data=zeros(length(tsteps),length(indices));
                end
            else
                Data.Data=cell(1,length(tsteps));
            end
            for i=1:length(tsteps)
                if Offset(i)<0
                    if allequalsize
                        Data.Data(i,:)=NaN;
                    else
                        Data.Data{i}=NaN;
                    end
                else
                    if S.Icim
                        IcimJumpTo(fid,Offset(i));
                        ID=ReadIcimInt32(fid,1);
                    else
                        fseek(fid,Offset(i),-1);
                        ID=fread(fid,1,'int32');
                    end
                    if S.Icim
                        switch S.Entry(idx).DataType
                            case 0
                                X=ReadIcimInt32(fid,Size(1));
                            case 1
                                X=ReadIcimFloat32(fid,Size(1));
                            otherwise
                                error(fprintf('S.Entry(idx).DataType = %i not supported.',S.Entry(idx).DataType));
                        end
                    else
                        switch S.Entry(idx).DataType
                            case 0
                                X=fread(fid,[1 Size(1)],'int32');
                            case 1
                                X=fread(fid,[1 Size(1)],'float32');
                            otherwise
                                error(fprintf('S.Entry(idx).DataType = %i not supported.',S.Entry(idx).DataType));
                        end
                    end
                    if ~isequal(indices,0), X=X(indices); end
                    if allequalsize
                        Data.Data(i,:)=X;
                    else
                        Data.Data{i}=X;
                    end
                end
            end
        end
        %  fseek(fid,S.Entry(idx).DataOffset,-1);
        %  ID=fread(fid,1,'int32');
        %  Data2.Field1=fread(fid,[2 S.Entry(idx).NumIter],'float32');
        %  X=fread(fid,[5 S.Entry(idx).NumIter],'int32');
        %  Data2.Date=X2Date(X([1 2],:));
        %  Data2.Offset=((X(3,:)-1)*512+X(4,:)-1)*4;
        %  Data2.Size=X(5,:);
    otherwise
        error(fprintf('S.Entry(idx).DataType2 = %i not supported,',S.Entry(idx).DataType2));
end
fclose(fid);



function sidsview(Structure,fid)
if nargin<2
    fid=1;
end
DataTypes={'integer','single precision','double precision','integer coded text','single complex','double complex','mix of integers and reals'};
InfoTypes=cell(1,100);
InfoTypes(1+(1:6)) = {'mesh','problem','solution','input','coefficient','boundary conditions'};
InfoTypes(1+(7:95)) = {'??'};
InfoTypes(1+(96:99)) = {'time','flag','semi permanent','SDS-administration'};
SAI=[Structure.Array.Index];
expment=1;
for entry=1:Structure.Experiment(1).NumChar
    k=Structure.Experiment(expment).Char(entry).DataIndex;
    fprintf(fid,'The name of the %i-th characteristic is %s.\n',entry,deblank(Structure.Experiment(expment).Char(entry).Name));
    fprintf(fid,'This characteristic was created on %i-%2.2i-%2.2i at %i:%2.2i:%2.2i h.\n',datevec(Structure.Experiment(expment).Char(entry).CreationDate));
    if (Structure.Experiment(expment).Char(entry).Level(1)==0)
        fprintf(fid,'The corresponding array is a main array.\n');
        if (Structure.Entry(k).ArrayType==0)
            AT='single array, ';
            TD='This array is not timedependent.\n';
        elseif (Structure.Entry(k).ArrayType==99)
            AT='compound array, ';
            TD='';
        end
    else
        fprintf(fid,'The path to the corresponding array:\n');
        L=max(find(Structure.Experiment(expment).Char(entry).Level>0));
        fprintf(fid,'ilevel = %i\n',L);
        if L==1
            fprintf(fid,'levels(1) = %3i\n',Structure.Experiment(expment).Char(entry).Level(1));
        else
            fprintf(fid,'levels = ');
            fprintf(fid,'%3i',Structure.Experiment(expment).Char(entry).Level(1:L));
            fprintf(fid,'\n');
        end
        AT=sprintf('%i-th subarray, ',Structure.Entry(k).ArrayType);
        TD='This array is not timedependent.\n';
    end
    if Structure.Entry(k).InfoType==97
        IT='flag, ';
    else
        if Structure.Entry(k).InfoType<=length(InfoTypes)
            IT=InfoTypes{Structure.Entry(k).InfoType+1};
            if ~isempty(IT)
                IT = [IT ', '];
            end
        else
            IT='??';
        end
    end
    DT=DataTypes{Structure.Entry(k).DataType+1};
    array=find(SAI==Structure.Experiment(expment).Char(entry).Index);
    fprintf(fid,'The corresponding array (%s) is a %s%s%s.\n',Structure.Array(array).Name,AT,IT,DT);
    fprintf(fid,'The array was created on %i-%2.2i-%2.2i at %i:%2.2i:%2.2i h.\n',datevec(Structure.Entry(k).CreationDate));
    fprintf(fid,'The last change to this array was on %i-%2.2i-%2.2i at %i:%2.2i:%2.2i h.\n',datevec(Structure.Entry(k).LastChanged));
    if (Structure.Entry(k).ArrayType==99)
        % print nothing
        plottree(Structure.Entry(k).Tree,1,'',1,Structure.Experiment(expment).Char)
    else
        if Structure.Entry(k).MaxNumIter>0
            fprintf(fid,'%i timesteps (iterations) are stored in the SDS-file, ( max = %i ).\n',Structure.Entry(k).NumIter,Structure.Entry(k).MaxNumIter);
            fprintf(fid,'  nr   | creation date | creation time | simulation time\n');
            Data=Local_read_sds(Structure,Structure.Experiment(expment).Name, ...
                Structure.Experiment(expment).Char(entry).Name,[]);
            X=[1:Structure.Entry(k).NumIter; round(transpose(datevec(Data.Date))); transpose(Data.SimTime)];
            fprintf(fid,'%5i      %4i-%2.2i-%2.2i      %2i:%2.2i:%2.2i      %12i\n',X(:,1:Structure.Entry(k).NumIter));
            %      fprintf(fid,'%5i      %4i-%2.2i-%2.2i      %2i:%2.2i:%2.2i      %12.4e\n',X(:,1:Structure.Entry(k).NumIter));
            if nargin<2
                pause
            end
        else
            fprintf(fid,'This array is not timedependent.\n');
        end
    end
    fprintf(fid,'\n');
end


function [idx,Info]=Local_getinfo(Entry,k,Level)
Info=[];
if k>length(Entry)
    idx=[];
elseif (Entry(k).ArrayType==99) & Level(1)~=0
    idx=1;
    t=1;
    while 1
        Ncell=Entry(k).Tree(idx);
        if (Level(t)<1) | (Level(t)>Ncell)
            idx=[];
            return
        else
            idx=Entry(k).Tree(idx+Level(t));
            t=t+1;
            if (idx<0) % entry number found
                if (t>length(Level)) | (Level(t)==0) % final level
                    Info=Entry(-idx);
                    idx=-idx;
                else % not final level, but entry found - invalid
                    idx=[];
                end
                return
            elseif (idx==0) % empty link followed
                idx=[];
                return;
            else % link followed
                if (t>length(Level)) | (Level(t)==0), % no more index
                    idx=[];
                    return
                else % continue ...
                end
            end
        end
    end
else
    Info=Entry(k);
    idx=k;
end


function Y=X2Date(X)
transp=0;
if size(X,1)~=2
    X=transpose(X);
    transp=1;
end
Y(6,1:size(X,2))=X(2,:)-floor(X(2,:)/100)*100;
X(2,:)=(X(2,:)-Y(6,:))/100;
Y(5,:)=X(2,:)-floor(X(2,:)/100)*100;
Y(4,:)=(X(2,:)-Y(5,:))/100;
Y(3,:)=X(1,:)-floor(X(1,:)/100)*100;
X(1,:)=(X(1,:)-Y(3,:))/100;
Y(2,:)=X(1,:)-floor(X(1,:)/100)*100;
Y(1,:)=(X(1,:)-Y(2,:))/100;
Y=transpose(Y);
Idx=Y(:,1)>3900;
Y(Idx,1)=Y(Idx,1)-1900;
Y=datenum(Y(:,1),Y(:,2),Y(:,3),Y(:,4),Y(:,5),Y(:,6));

function Y=X2Y(X)
Y(3)=X-floor(X/100)*100;
X(1)=(X-Y(3))/100;
Y(2)=X-floor(X/100)*100;
Y(1)=(X-Y(2))/100;

function plottree(Tree,i,BaseStr,fid,Characteristics)
if nargin<5
   Characteristics = [];
end
n=Tree(i);
V=Tree(i+(1:n));
Cont='| ';
for j=1:n
    if j==n
        Cont='  ';
    end
    if j==1
        fprintf(fid,'+-');
    else
        fprintf(fid,'%s|\n%s+-',BaseStr,BaseStr);
    end
    if V(j)<=0
       if ~isempty(Characteristics)
          k=find([Characteristics.DataIndex]==-V(j));
          if isempty(k)
             fprintf(fid,'[]\n');
          else
             if length(k)>1
                1
             end
             fprintf(fid,'%s\n',Characteristics(k).Name);
          end
       else
          fprintf(fid,'%i\n',-V(j));
       end
    else
        plottree(Tree,V(j),[BaseStr Cont],fid,Characteristics);
    end
end

function Str=Swap4byte(Str1)
N=length(Str1);
if N~=round(N/4)*4
    error('String length not multiple of 4 bytes. Cannot swap bytes.')
end
I=1:N;
I=flipud(reshape(I,[4 N/4]));
Str=Str1(I(:)');

function IcimJumpStep(fid,Jump)
Offset=ftell(fid);
NBlock8192=floor(Offset/8192);
RelOffset=Offset-NBlock8192*8192;
NewRelOffset=RelOffset+Jump;
NBlock2048=floor(NewRelOffset/2048);
NewOffset=NBlock8192*8192+NewRelOffset+NBlock2048*6144;
fseek(fid,NewOffset,-1);

function IcimJumpTo(fid,Offset)
NBlock2048=floor(Offset/2048);
NewOffset=Offset+NBlock2048*6144;
fseek(fid,NewOffset,-1);

function Near=NearEndOfBlock(fid,Jump)
Offset=ftell(fid);
NBlock8192=floor(Offset/8192);
RelOffset=Offset-NBlock8192*8192;
NewRelOffset=RelOffset+Jump;
Near=max(0,NewRelOffset-2048);

function X=ReadIcimUchar(fid,N)
Beyond=NearEndOfBlock(fid,N);
if Beyond==0
    X=char(fread(fid,[1 N],'uchar'));
%elseif Beyond==N
%    IcimJumpStep(fid,0);
%    X=char(fread(fid,[1 N],'uchar'));
else % Beyond>0
    X=repmat(char(32),1,N);
    n=N-Beyond;
    X(1:n)=fread(fid,[1 n],'uchar');
    n0=n;
    while Beyond>0
        IcimJumpStep(fid,0);
        n=min(512,Beyond);
        X(n0+(1:n))=fread(fid,[1 n],'uchar');
        n0=n0+n;
        Beyond=Beyond-n;
    end
    %X1=fread(fid,[1 N-Beyond],'uchar');
    %IcimJumpStep(fid,0);
    %X2=fread(fid,[1 Beyond],'uchar');
    %X=char(cat(2,X1,X2));
end

function X=ReadIcimInt32(fid,N)
Beyond=NearEndOfBlock(fid,4*N);
if Beyond==0
    X=fread(fid,[1 N],'int32');
%elseif Beyond==4*N
%    IcimJumpStep(fid,0);
%    X=fread(fid,[1 N],'int32');
else % Beyond>0
    X=zeros(1,N);
    n=N-(Beyond/4);
    X(1:n)=fread(fid,[1 n],'int32');
    n0=n;
    while Beyond>0
        IcimJumpStep(fid,0);
        n=min(512,Beyond/4);
        X(n0+(1:n))=fread(fid,[1 n],'int32');
        n0=n0+n;
        Beyond=Beyond-4*n;
    end
end

function X=ReadIcimFloat32(fid,N)
Beyond=NearEndOfBlock(fid,4*N);
if Beyond==0
    X=fread(fid,[1 N],'float32');
%elseif Beyond==4*N
%    IcimJumpStep(fid,0);
%    X=fread(fid,[1 N],'float32');
else % Beyond>0
    X=zeros(1,N);
    n=N-(Beyond/4);
    X(1:n)=fread(fid,[1 n],'float32');
    n0=n;
    while Beyond>0
        IcimJumpStep(fid,0);
        n=min(512,Beyond/4);
        X(n0+(1:n))=fread(fid,[1 n],'float32');
        n0=n0+n;
        Beyond=Beyond-4*n;
    end
end

function X=ReadIcimFloat64(fid,N)
Beyond=NearEndOfBlock(fid,8*N);
if Beyond==0
    X=fread(fid,[1 N],'float64');
%elseif Beyond==8*N
%    IcimJumpStep(fid,0);
%    X=fread(fid,[1 N],'float64');
else % Beyond>0
    X=zeros(1,N);
    n=N-(Beyond/8);
    if n~=round(n), error('Cannot read float64 splitted by Icim dummy blocks'); end
    % if the above mentioned error occurs, the only work around seems to be
    % writing all 8 bytes to a temporary file and reading the combined bytes
    % from that file into X.
    X(1:n)=fread(fid,[1 n],'float64');
    n0=n;
    while Beyond>0
        IcimJumpStep(fid,0);
        n=min(256,Beyond/8);
        X(n0+(1:n))=fread(fid,[1 n],'float64');
        n0=n0+n;
        Beyond=Beyond-8*n;
    end
end


% Read the SIMETF file containing the error messages
function Message = read_simetf_messages(FileName)
if nargin<1
    FileName='simetf';
end
fid = fopen(FileName,'r');
X = fread(fid,[1 20],'int32');
nr = X(1);
Index = fread(fid,[1 nr+1],'int32');
fseek(fid,(Index(1)-1)*80,-1);
NrLines = diff(Index);
for i=1:nr
    if NrLines(i)==0
        Message(i).Txt = '';
    else
        Message(i).Txt = char(fread(fid,[80 NrLines(i)],'uchar'))';
    end
end
fclose(fid);
