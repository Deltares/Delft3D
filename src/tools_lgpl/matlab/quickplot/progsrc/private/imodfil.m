function varargout = imodfil(FI,domain,field,cmd,varargin)
%IMODFIL QP support for iMOD data files.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [TZshift   ,TZstr  ]    = XXXFIL(FI,Domain,DataFld,'timezone')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%   [hNew      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
%
%   The DataFld can only be either an element of the DataProps structure.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2024 Stichting Deltares.                                     
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

T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments')
elseif nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'getparams'
            varargout={[]};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'subfields'
        varargout={{}};
        return
    case 'plotoptions'
        varargout = {[]};
        return
    case 'plot'
        varargout={[] FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

iarg = 0;
t = 1;
rows = 1:FI.n_rows;
cols = 1:FI.n_columns;
for i = 1:5
    if Props.DimFlag(i)
        if nargin-4 > iarg % dimension specified
            iarg = iarg+1;
            arg = varargin{iarg};
        else % dimension not specified
            % use default
            continue
        end
        if arg == 0
            % returning all values is the default
            continue
        end
        switch i
            case T_
                t = arg;
            case M_ % rows
                rows = arg;
            case N_ % columns
                cols = arg;
        end
    end
end

if DataRead
    switch Props.Name
        case 'mesh'
        otherwise
            data.Val = imod('read',FI,'val');
            data.Val = data.Val(rows,cols);
    end
end
if XYRead
    [data.X, data.Y] = imod('read',FI,'x','y');
    data.Y = fliplr(data.Y);
    if DataInCell
        data.X = data.X(unique([cols,cols+1]));
        data.Y = data.Y(unique([rows,rows+1]));
        data.X = repmat(data.X,length(rows)+1,1);
        data.Y = repmat(data.Y.',1,length(cols)+1);
    else
        data.X = (data.X(1:end-1) + data.X(2:end))/2;
        data.Y = (data.Y(1:end-1) + data.Y(2:end))/2;
        data.X = data.X(cols);
        data.Y = data.Y(rows);
        data.X = repmat(data.X,length(rows),1);
        data.Y = repmat(data.Y.',1,length(cols));
    end
end

if Props.DimFlag(T_)
    data.Time = readtim(FI,Props,t);
end

varargout={data, FI};


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
PropNames={'Name'                      'DimFlag' 'DataInCell' 'NVal'};
DataProps={'mesh'                      [0 0 1 1 0]  0         0
           'value'                     [0 0 1 1 0]  1         1     };
DataProps{2,1} = FI.name;
if ~isempty(FI.date)
    DataProps{2,2}(1) = 1;
end
Out=cell2struct(DataProps,PropNames,2);


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
sz=[0 0 FI.n_rows FI.n_columns 0];
if Props.DimFlag(1)
    sz(1) = 1;
end


% -------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T = FI.date;