function rtc_communicator(varargin)
%RTC_COMMUNICATOR  Routine handling communication with RTC module.
%   Features of this RTC_COMMUNICATOR function include:
%    * Performs all communication with RTC module via DelftIO.
%    * Calls user script in time loop.
%    * Allows for almost all possible user script statements, including
%      'clear all', and persistent variables between evaluations.
%
%   See also DIOPLT_DEFINE.

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

% Move input arguments to fields in structure.
RTC_COMM_.errorfile = RTC_COMM_errorfile;
for i = 1:2:nargin
    switch lower(varargin{i})
        case {'debug','channel','userscript'}
            RTC_COMM_.(lower(varargin{i})) = varargin{i+1};
        case 'errorfile'
            RTC_COMM_errorfile(varargin{i+1});
            return
        otherwise
            error('Unknown parameter: %s',varargin{i})
    end
end
clear i

if ~isfield(RTC_COMM_,'channel')
    error('Missing RTC Communication channel.')
end
if ~isfield(RTC_COMM_,'debug')
    RTC_COMM_.debug = 0;
end
% Define communication channel names.
RTC_COMM_.M2R_sig = [RTC_COMM_.channel,'.MATLAB2RTC.signal.shm'];
RTC_COMM_.R2M_sig = [RTC_COMM_.channel,'.RTC2MATLAB.signal.shm'];
RTC_COMM_.R2M_in  = [RTC_COMM_.channel,'.RTC2MATLAB.shm'];
RTC_COMM_.R2M_out = [RTC_COMM_.channel,'.RTC2MATLAB.requested.shm'];
RTC_COMM_.M2R_out = [RTC_COMM_.channel,'.MATLAB2RTC.shm'];

RTC_COMM_debug(RTC_COMM_,'Open signal stream from MATLAB to RTC module: %s.',RTC_COMM_.M2R_sig)
RTC_COMM_.M2R_sig = dioplt_define(RTC_COMM_.M2R_sig,{'Value'},{'Signal'},'int32');
RTC_COMM_debug(RTC_COMM_,'Open signal stream from RTC module to MATLAB: %s.',RTC_COMM_.R2M_sig)
RTC_COMM_.R2M_sig = dioplt_define(RTC_COMM_.R2M_sig,'double');

RTC_COMM_debug(RTC_COMM_,'Obtain communication test code from RTC module.')
RTC_COMM_.ComTest = dioplt_read(RTC_COMM_.R2M_sig);
RTC_COMM_debug(RTC_COMM_,'Received: %i\nSend confirmation.',RTC_COMM_.ComTest)
dioplt_write(RTC_COMM_.M2R_sig,RTC_COMM_.ComTest);

RTC_COMM_debug(RTC_COMM_,'Get names of variables to be provided by RTC module: %s.',RTC_COMM_.R2M_in)
RTC_COMM_.R2M_in = dioplt_define(RTC_COMM_.R2M_in,'double');
RTC_COMM_.InVars = RTC_COMM_.R2M_in.Loc;
RTC_COMM_debug(RTC_COMM_,'%s',RTC_COMM_.InVars{:})
RTC_COMM_debug(RTC_COMM_,'Send confirmation.')
dioplt_write(RTC_COMM_.M2R_sig,length(RTC_COMM_.InVars));

RTC_COMM_debug(RTC_COMM_,'Get names of variables to be provided by MATLAB: %s.',RTC_COMM_.R2M_out)
RTC_COMM_.R2M_out = dioplt_define(RTC_COMM_.R2M_out,'double');
if ~isfield(RTC_COMM_,'userscript')
    RTC_COMM_.userscript = RTC_COMM_.R2M_out.Par{1};
end
RTC_COMM_.OutVars = RTC_COMM_.R2M_out.Loc;
RTC_COMM_debug(RTC_COMM_,'%s',RTC_COMM_.OutVars{:})
RTC_COMM_debug(RTC_COMM_,'Send confirmation.')
dioplt_write(RTC_COMM_.M2R_sig,length(RTC_COMM_.OutVars));
RTC_COMM_debug(RTC_COMM_,'Delete R2M_out stream.')
dioplt_delete(RTC_COMM_.R2M_out)

RTC_COMM_debug(RTC_COMM_,'Open stream for sending values back to RTC module: %s.',RTC_COMM_.M2R_out)
RTC_COMM_.M2R_out = dioplt_define(RTC_COMM_.M2R_out,{'Value'},RTC_COMM_.OutVars,'double');

RTC_COMM_debug(RTC_COMM_,'Set status and determine constant derived variables.')
RTC_COMM_.MatlabStatus = 0;
RTC_COMM_.catOutVars = ['[' sprintf('%s ',RTC_COMM_.OutVars{:}) ']'];

RTC_COMM_debug(RTC_COMM_,'Start time loop.')
RTC_COMM_.t = 0;
while 1
    RTC_COMM_debug(RTC_COMM_,'---- Increment time step. ----')
    RTC_COMM_.t = RTC_COMM_.t + 1;
    
    RTC_COMM_debug(RTC_COMM_,'Obtain signal from RTC module.')
    RTC_COMM_.tRtc = dioplt_read(RTC_COMM_.R2M_sig);
    if RTC_COMM_.tRtc < 0
        RTC_COMM_debug(RTC_COMM_,'Close signal received.')
        break
    elseif RTC_COMM_.tRtc ~= RTC_COMM_.t
        RTC_COMM_debug(RTC_COMM_,'Invalid signal received from RTC module.')
        RTC_COMM_debug(RTC_COMM_,'Sending stop signal back.')
        dioplt_write(RTC_COMM_.M2R_sig,-1);
        break
    end
    
    RTC_COMM_debug(RTC_COMM_,'Get data values from RTC module.')
    RTC_COMM_.InVals = dioplt_read(RTC_COMM_.R2M_in);
    
    RTC_COMM_debug(RTC_COMM_,'Expand InVars and InVals into separate variables.')
    RTC_COMM_setvariables(RTC_COMM_.InVars,RTC_COMM_.InVals)
    
    RTC_COMM_debug(RTC_COMM_,'Temporarily remove all control data from this workspace.')
    RTC_COMM_safe(RTC_COMM_);
    clear RTC_COMM_
    
    try
        RTC_COMM_debug(RTC_COMM_safe,'Call user script: %s.',RTC_COMM_userscript)
        eval(RTC_COMM_userscript)
        
        RTC_COMM_debug(RTC_COMM_safe,'Restore control data to this workspace.')
        RTC_COMM_ = RTC_COMM_safe;
        
        RTC_COMM_debug(RTC_COMM_,'Collect values of OutVars into one array.')
        RTC_COMM_.OutVals = eval(RTC_COMM_.catOutVars);
    catch
        RTC_COMM_errorhandler(RTC_COMM_safe,lasterror) %#ok<LERR> % using lasterror for backward compatibility
        
        RTC_COMM_debug(RTC_COMM_safe,'Restore control data to this workspace.')
        RTC_COMM_ = RTC_COMM_safe;
        
        RTC_COMM_debug(RTC_COMM_,'Indicate failure status.')
        RTC_COMM_.MatlabStatus = -1;
    end
    
    RTC_COMM_debug(RTC_COMM_,'Send status.')
    dioplt_write(RTC_COMM_.M2R_sig,RTC_COMM_.MatlabStatus);
    if RTC_COMM_.MatlabStatus<0
        break
    end
    
    RTC_COMM_debug(RTC_COMM_,'Send data values back to RTC module.')
    dioplt_write(RTC_COMM_.M2R_out,RTC_COMM_.OutVals);
end

if RTC_COMM_.MatlabStatus<0 && RTC_COMM_.debug>0
    RTC_COMM_debug(RTC_COMM_,'Copy all variables to base workspace.')
    RTC_COMM_.Vars = who;
    RTC_COMM_.nVar = length(RTC_COMM_.Vars);
    RTC_COMM_.iVar = 0;
    while RTC_COMM_.iVar<RTC_COMM_.nVar
        RTC_COMM_.iVar = RTC_COMM_.iVar+1;
        if ~strcmp(RTC_COMM_.Vars{RTC_COMM_.iVar},'RTC_COMM_')
            assignin('base',RTC_COMM_.Vars{RTC_COMM_.iVar},eval(RTC_COMM_.Vars{RTC_COMM_.iVar}))
        end
    end
    RTC_COMM_debug(RTC_COMM_,'Return to MATLAB prompt ...')
else
    RTC_COMM_debug(RTC_COMM_,'Close MATLAB.')
    exit
end


function Out = RTC_COMM_safe(In)
%RTC_COMM_SAFE  Store and retreive control data.
%   This function with persistent State acts as a temporary store for the
%   control data while the user script evaluates.

persistent State
if nargin==1
    State = In;
else
    Out = State;
end


function userscript = RTC_COMM_userscript
%RTC_COMM_USERSCRIPT  Retreive name of user script.
%   This function returns the name of the user script while the control
%   data is not locally available.

RTC_COMM_  = RTC_COMM_safe;
userscript = RTC_COMM_.userscript;


function RTC_COMM_setvariables(Variables,Values)
%SETVARIABLES  Expand InVars and InVals into separate variables.
%   SETVARIABLES(VARS,VALUES) expands the values stored in VALUES into
%   separate variables of which the names are given by cell array VARS. The
%   VALUES and VARS arrays should have the same number of elements. The
%   variables are defined in the calling routine.

for i = 1:numel(Variables)
    switch Variables{i}
        case {'SobekDate','SobekTime','SobekCompTimestepSize'}
            assignin('caller',Variables{i},sprintf('%08i',Values(i)))
        otherwise
            assignin('caller',Variables{i},Values(i))
    end
end


function RTC_COMM_debug(RTC_COMM_,varargin)
args = varargin;
args{1} = [args{1} '\n'];
if RTC_COMM_.debug>0
    fprintf(RTC_COMM_.debug,args{:});
end


function filename_out = RTC_COMM_errorfile(filename_in)
persistent filename
if nargin>0
   filename = fullfile(pwd,filename_in);
end
if nargout>0
   filename_out = filename;
end


function RTC_COMM_errorhandler(RTC_COMM_,Exception)
% first write message to the screen (for diary or online debugging)
fprintf('ERROR: %s\n',Exception.message);
if isfield(Exception,'stack') % stack wasn't included in first releases
    Exception.stack = stack2str(Exception.stack);
    fprintf('%s\n',Exception.stack{1:end-2});
end
% now write error message to the errorfile to be picked up by RTC.EXE
try
    fid = fopen(RTC_COMM_.errorfile,'wt');
    fprintf(fid,'ERROR: %s\n',Exception.message);
    if isfield(Exception,'stack')
        fprintf(fid,'%s\n',Exception.stack{1:end-2});
    end
    fclose(fid);
end


function str = stack2str(stack)
%STACK2STR Convert exception stack into cell string.
%
%   CELLSTR = STACK2STR(STACK)
%   where STACK is a stack as obtained from MException.stack.
%
%   See also MException.

stacklen = length(stack);
str = repmat({''},stacklen,1);
mpath = multiline(matlabpath,pathsep,'cell');
for i = 1:stacklen
    [p,f] = fileparts(stack(i).file);
    if ~strcmp(f,stack(i).name)
        fcn = sprintf('>%s',stack(i).name);
    else
        fcn = '';
    end
    z = zeros(size(mpath));
    for j = 1:length(mpath)
        if strncmp(p,mpath{j},length(mpath{j}))
            z(j) = length(mpath{j});
        end
    end
    [len,j] = max(z);
    p = p(len+1:end);
    if ~isempty(p) && isequal(p(1),filesep)
        p = p(2:end);
    end
    if ~isempty(p)
        p = [p filesep];
    end
    str{i} = sprintf('In %s%s%s at line %i',p,f,fcn,stack(i).line);
end 

function [Sout,Ssep]=multiline(Sin,varargin)
%MULTILINE Converts a string containing LineFeeds to a char matrix.
%   STRMAT=MULTILINE(S,C) splits the input string S at the characters
%   listed in the char vector C. Each section forms a line of the char
%   matrix STRMAT (left aligned). Enter \n for char(10):linefeed and \t for
%   char(9):tab. If C is not specified, the string S is split at linefeeds
%   (i.e. the default value of C is char(10).
%
%   CELSTR=MULTILINE(...,'cell') converts the input string into a cell
%   string instead of a char matrix.
%
%   Example:
%      Str=multiline(sprintf('%i\n',1:10:31))
%      % gives the 4 x 2 char matrix
%
%      Str = 1
%            11
%            21
%            31
%
%   See also CELLSTR, STR2MAT.

cArray=char(10);
cellOutput=0;
if nargin>1
    Inp=varargin;
    if isequal('cellrow',lower(Inp{end}))
        cellOutput=2;
        Inp(end)=[];
    elseif isequal('cell',lower(Inp{end}))
        cellOutput=1;
        Inp(end)=[];
    end
    if length(Inp)>1
        error('Too many input arguments.')
    elseif length(Inp)==1
        cArray=Inp{1};
        cArray=strrep(cArray,'\t',char(9));
        cArray=strrep(cArray,'\n',char(10));
    end
end
if nargin<1 || ~ischar(Sin)
    error('Invalid input argument.')
elseif isempty(Sin)
    if cellOutput
        Sout={};
    else
        Sout='';
    end
    if nargout>1
        Ssep='';
    end
elseif ndims(Sin)~=2 || min(size(Sin))~=1
    error('Invalid input argument.')
else
    LineFeed=find(ismember(Sin,cArray));
    if nargout>1
        Ssep=Sin(LineFeed);
    end
    Start=[1 LineFeed+1];
    End=[LineFeed-1 length(Sin)];
    if cellOutput
        if cellOutput==2
            Sout=cell(1,length(Start));
        else
            Sout=cell(length(Start),1);
        end
        for k=1:length(Start)
            Sout{k}=deblank(Sin(Start(k):End(k)));
        end
    else
        maxStrL=max(End-Start)+1;
        Sout=repmat(' ',length(Start),maxStrL);
        for k=1:length(Start)
            Sout(k,1:(End(k)-Start(k)+1))=Sin(Start(k):End(k));
        end
    end
end 