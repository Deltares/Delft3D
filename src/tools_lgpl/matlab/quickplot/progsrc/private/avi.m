function varargout=avi(cmd,varargin)
%AVI MATLAB AVI interface.

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

%#function writeavi

persistent AVIOptions
if isempty(AVIOptions)
    AVIOptions.C8 = [];
    AVIOptions.C24 = [];
end

if nargout>0
    varargout=cell(1,nargout);
end

if nargin==0
    return
end

switch cmd
    case 'initialize'
        AVIHandle.CPointer=calldll('writeavi','initialize');
        varargout={AVIHandle};
    case 'open'
        if nargin<3
            error('Not enough input arguments')
        elseif nargout>1
            error('Too many output arguments')
        else
            AVIHandle = varargin{1};
            FileName = varargin{2};
            if ~isstruct(AVIHandle) || ~isfield(AVIHandle,'CPointer') ...
                    || AVIHandle.CPointer==0
                error('First argument is not a valid AVI handle.')
            elseif ~ischar(FileName) || ~isequal(size(FileName),[1 length(FileName)])
                error('Second argument is not a valid file name.')
            end
            fid=fopen(FileName,'w');
            if fid<0
                error('Cannot open output file: %s',FileName)
            end
            fclose(fid);
            AVIHandle.CPointer = calldll('writeavi','open',AVIHandle.CPointer,FileName);
            AVIHandle.FileName = FileName;
            varargout={AVIHandle};
        end
    case 'options'
        if nargin>1
            error('Too many input arguments')
        elseif nargout>1
            error('Too many output arguments')
        end
        if isempty(AVIOptions.C24)
            AVIOptions.C24 = calldll('writeavi','getoptions',24);
        else
            AVIOptions.C24 = calldll('writeavi','getoptions',24,AVIOptions.C24);
        end
        if AVIOptions.C24.fccHandler==0
            AVIOptions.C24=[];
        end
        if nargout>0
            varargout={AVIOptions.C24};
        end
    case 'addvideo'
        if nargin<4
            error('Not enough input arguments')
        elseif nargout>2
            error('Too many output arguments')
        else
            AVIHandle = varargin{1};
            FrameRate = varargin{2};
            Fig = varargin{3};
            if ~isstruct(AVIHandle) | ~isfield(AVIHandle,'CPointer') ...
                    | AVIHandle.CPointer==0
                error('First argument is not a valid AVI handle.')
            end
            if ~isfield(AVIHandle,'FileName')
                error('No output file opened.')
            end
            if isempty(AVIOptions.C24)
                avi('options');
                if isempty(AVIOptions.C24)
                    varargout={AVIHandle 0};
                end
            end
            szFig=size(Fig);
            szAVI4=ceil(szFig(1:2)/4);
            szAVI=ceil(szAVI4)*4; % most compressors require that
            % the number of pixels is a
            % multiple of 4.
            AVIHandle.CPointer = calldll('writeavi','addvideo', AVIHandle.CPointer, ...
                FrameRate, szAVI(1), szAVI(2), 24, AVIOptions.C24);
            AVIHandle.VideoSize=szFig;
            AVIHandle.FrameNr=0;
            varargout={AVIHandle 1};
        end
    case 'addframe'
        if nargin<3
            error('Not enough input arguments')
        elseif nargout>1
            error('Too many output arguments')
        else
            AVIHandle = varargin{1};
            Fig = varargin{2};
            if ~isstruct(AVIHandle) | ~isfield(AVIHandle,'CPointer') ...
                    | AVIHandle.CPointer==0
                error('First argument is not a valid AVI handle.')
            end
            if ~isfield(AVIHandle,'FileName')
                error('No output file opened.')
            end
            if ~isfield(AVIHandle,'VideoSize')
                error('No video stream opened for file %s.',AVIHandle.FileName)
            end
            szFig=size(Fig);
            szAVI4=ceil(szFig(1:2)/4);
            szAVI=ceil(szAVI4)*4; % most compressors require that
            % the number of pixels is a
            % multiple of 4.
            Fig=Fig(min(1:szAVI(1),szFig(1)),min(1:szAVI(2),szFig(2)),:);
            if ~isequal(AVIHandle.VideoSize,szFig)
                error('Image size should not change during video.');
            end
            AVIHandle.FrameNr=AVIHandle.FrameNr+1;
            AVIHandle.CPointer = calldll('writeavi','addframe', AVIHandle.CPointer, ...
                Fig, AVIHandle.FrameNr);
            varargout={AVIHandle};
        end
    case 'close'
        if nargin<2
            error('Not enough input arguments')
        elseif nargout>1
            error('Too many output arguments')
        else
            AVIHandle = varargin{1};
            if ~isstruct(AVIHandle) | ~isfield(AVIHandle,'CPointer') ...
                    | AVIHandle.CPointer==0
                error('First argument is not a valid AVI handle.')
            end
            AVIHandle.CPointer = calldll('writeavi','close', AVIHandle.CPointer);
            if isfield(AVIHandle,'VideoSize')
                AVIHandle = rmfield(AVIHandle,'FrameNr');
                AVIHandle = rmfield(AVIHandle,'VideoSize');
            end
            if isfield(AVIHandle,'FileName')
                AVIHandle = rmfield(AVIHandle,'FileName');
            end
            varargout={AVIHandle};
        end
    case 'finalize'
        if nargin<2
            error('Not enough input arguments')
        elseif nargout>0
            error('Too many output arguments')
        else
            AVIHandle = varargin{1};
            if ~isstruct(AVIHandle) | ~isfield(AVIHandle,'CPointer') ...
                    | AVIHandle.CPointer==0
                error('First argument is not a valid AVI handle.')
            end
            if isfield(AVIHandle,'FileName')
                AVIHandle = avi('close',AVIHandle);
            end
            ok=calldll('writeavi','finalize', AVIHandle.CPointer);
        end
    otherwise
        error('Unknown command')
end

