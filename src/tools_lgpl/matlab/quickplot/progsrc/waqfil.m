function S = waqfil(cmd,file,varargin)
%WAQFIL Read various Delwaq binary files.
%
%   FI = WAQFIL('open',FILENAME,...extra arguments...) opens the specified
%   file and reads a part or all of the data. The file type is detected
%   based on the file extension. If the file extension does not correspond
%   to the file contents, then the 'open' string should be changed to the
%   string ['open' EXT] where EXT is the file extension that matches the
%   file contents, for instance 'open.vol'. Important (meta)data is
%   returned as the structure FI. This routine does not support .his, .bal
%   and .map files; use the DELWAQ function for those files.
%
%   Data = WAQFIL('read',FI,...extra arguments...) reads additional data
%   from the file previously opened using a WAQFIL('open',...) call.
%
%   This function call supports the following file types (the extra
%   arguments for the open call are indicated after the list of file name
%   extensions).
%
%   Volume, salinity, temperature, and shear stress files
%     * .vol, .sal, .tem, .vdf, .tau files: NSeg
%     * new .srf files fall into this category as well
%
%   Segment function and parameter files
%     * .segfun files                     : NSeg, NPar
%
%   Flow area and flux files
%     * .are, .flo files                  : NExch
%
%   Pointer table files
%     * .poi files                        : NExch
%
%   Distance table files
%     * .len files                        : NExch
%
%   Chezy files
%     * .chz files                        : -
%
%   Segment surface area and depth files
%     * .srf (old format), *.dps files    : -
%
%   table files
%     * .lgt files                        : -
%
%   See also: DELWAQ, QPFOPEN. 

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

if length(cmd)<3
    error('Unknown command: %s',cmd)
end
switch lower(cmd(1:4))
    case 'open'
        if length(cmd)>4
            e = cmd(5:end);
        else
            [p,f,e]=fileparts(file);
        end
        switch lower(e)
            case {'.vol','.sal','.tem','.vdf','.tau'}
                S = openvol(file,varargin{:});
            case {'.are','.flo'}
                S = openare(file,varargin{:});
            case {'.segfun','.par'}
                S = opensegfun(file,varargin{:});
            case '.poi'
                S = openpoi(file,varargin{:});
            case '.len'
                S = openlen(file,varargin{:});
            case '.chz'
                S = openchz(file,varargin{:});
            case {'.srf','.srfold','.dps'}
                if nargin==3
                    S = openvol(file,varargin{:});
                else
                    S = opensrf(file,varargin{:});
                end
            case '.lgt'
                S = openlgt(file,varargin{:});
            otherwise
                error('Unrecognized file type: %s',e)
        end
        S.FileType = lower(e);
    case 'read'
        switch file.FileType
            case {'.vol','.sal','.tem','.vdf','.tau'}
                S = readvol(file,varargin{:});
            case {'.are','.flo'}
                S = readvol(file,varargin{:});
            case '.segfun'
                S = readvol(file,varargin{:});
            case '.par'
                S = readpar(file,varargin{:});
            case '.poi'
                S = readpoi(file,varargin{:});
            case '.len'
                S = readlen(file,varargin{:});
            case '.chz'
                S = file.Chezy(varargin{end},:)';
            case {'.srf','.srfold','.dps'}
                if isfield(file,'Srf')
                    S = file.Srf;
                else
                    S = readvol(file,varargin{:});
                end
            case '.lgt'
                S = readlgt(file,varargin{:});
        end
end

%% Vol file
function S = openvol(file,nseg)
S = openfile(file,nseg,1);

function D = readvol(file,itime,ipar)
if nargin<3
    ipar = ':';
end
fid = fopen(file.FileName,'r');
if isfield(file,'Grouped') && strcmp(file.Grouped,'parameters')
    sz = [file.NPar file.NVals];
    sub = {ipar,':'};
else
    sz = [file.NVals file.NPar];
    sub = {':',ipar};
end
fseek(fid,((itime-1)*(file.NVals*file.NPar+1)+1)*4,-1);
D = fread(fid,sz,'float32');
fclose(fid);
D = D(sub{:});

%% Segfun or Par file
function S = opensegfun(file,nseg,npar)
S = openfile(file,nseg,npar);
%S.Grouped = 'parameters'; % seems to be necessary for par, but for segfun?

function D = readpar(file,iseg,ipar)
if nargin<3
    ipar = 1:file.NPar;
end
fid = fopen(file.FileName,'r');
fread(fid,1,'int32');
D = fread(fid,[file.NPar file.NVals],'float32');
fclose(fid);
D = D(ipar,iseg);

%% Are or Flo file
function S = openare(file,nexch)
sum_noq = sum(nexch);
S = openfile(file,sum_noq);
S.Grouped = 'parameters';

%% Vol, Are or Flo File
function S = openfile(file,nval,npar)
if nargin<3
    npar = -1;
end
fid = fopen(file,'r');
S.FileName = file;
S.NVals = nval;
S.Grouped = 'segments';
%
%time=fread(fid,1,'int32');
%vals=fread(fid,nval,'float32');
%
fseek(fid,0,1);
fz = ftell(fid);
if npar<0 % auto detect number of parameters
    npartime = floor(fz/nval/4); % this number is too big if ntime>nval
    while 1
       ntime = (fz-npartime*nval*4)/4;
       npar = npartime/ntime;
       if npar==round(npar) % perfect match
           break
       elseif npar<1 % give up
           npar = 1;
           break
       else % try reducing npartime (necessary if ntime>nval)
           npartime = npartime-1;
       end
    end
end
nbytes_pertime = (nval*npar+1)*4;
S.NPar = npar;
S.NTimes = fz/nbytes_pertime;
%
fseek(fid,0,-1);
S.Times = fread(fid,S.NTimes,'int32',nbytes_pertime-4);
%
%fseek(fid,0,-1);
%S.Vals = zeros(nval,npar,S.NTimes);
%for t = 1:S.NTimes
%    fread(fid,1,'int32');
%    S.Vals(:,:,t) = fread(fid,[nval npar],'float32');
%end
%
fclose(fid);

%% Poi File
function S = openpoi(file)
fid = fopen(file,'r');
S.FileName = file;
%
S.Pointer=fread(fid,[4 inf],'int32')';
S.PointerLabels={'From','To','Upstream_of_From','Downstream_of_To'};
%
fclose(fid);

%% Len File
function S = openlen(file)
fid = fopen(file,'r');
S.FileName = file;
%
S.Sum_NOQ = fread(fid,1,'int32');
S.Dist    = fread(fid,[2 S.Sum_NOQ],'float32')';
S.DistLabels={'Distance_from_From_to_Interface','Distance_from_Interface_to_To'};
%
fclose(fid);

%% Chz File
function S = openchz(file)
fid = fopen(file,'r');
S.FileName = file;
%
S.NLayers = fread(fid,1,'int32');
S.Dims    = fread(fid,[1 2],'int32');
S.NAct    = fread(fid,1,'int32');
S.XXX     = fread(fid,[1 3],'int32');
S.Chezy   = fread(fid,[prod(S.Dims) 2],'float32')';
S.ChezyLabels = {'Chezy_Direction1','Chezy_Direction2'};
%
fclose(fid);

%% Srf File
function S = opensrf(file)
S.FileName = file;

fid = fopen(file,'r','b');
uf1 = fread(fid,1,'int32');
X = fread(fid,[1 6],'int32');
uf2 = fread(fid,1,'int32');
if isequal(uf1,24) && isequal(uf2,24)
    S.ByteOrder = 'b';
    S.FormatType = 'formatted4';
else
    S.ByteOrder = 'l';
    S.FormatType = 'binary';
    fclose(fid);
    fid = fopen(file,'r','l');
    X = fread(fid,[1 6],'int32');
end
%
if X(1)==0
    error('Could this be a new format srf file?')
end
S.Dims    = X(1:2);
S.NAct    = X(3);
S.XXX     = X(4:6);
if strcmp(S.FormatType,'formatted4')
    uf1 = fread(fid,1,'int32');
    if ~isequal(uf1,4*S.NAct)
        fclose(fid);
        error('Unexpected format size %i while expecting %i',uf1,4*S.NAct)
    end
end
S.Srf     = fread(fid,[S.NAct 1],'float32');
%
fclose(fid);

%% Lgt File
function S = openlgt(file)
fid = fopen(file,'r');
S.FileName = file;
%
S.Dims    = fread(fid,[1 2],'int32');
S.NSeg1   = fread(fid,1,'int32');
S.NLayers = fread(fid,1,'int32');
S.Index   = fread(fid,S.Dims,'int32');
%
fclose(fid);
