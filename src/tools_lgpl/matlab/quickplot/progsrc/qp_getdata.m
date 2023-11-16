function [varargout]=qp_getdata(varargin)
%QP_GETDATA General interface for various data files
%   Core implementation of QPREAD functionality. See QPREAD for syntax details.
%   The only difference between QPREAD and this function is the treatment of
%   the flag "Success".
%
%   [Success,Dimensions]            = QP_GETDATA(FI,'dimensions')
%   [Success,Locations ]            = QP_GETDATA(FI,'locations')
%   [Success,Quantities]            = QP_GETDATA(FI,'quantities')
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,'data',Quantity,DimSelection)
%
%   [Success,Domains   ]            = QP_GETDATA(FI,'domains')
%   [Success,DataProps ]            = QP_GETDATA(FI,Domain)
%   [Success,DataProps ]            = QP_GETDATA(FI,Domain,DimMask)
%   [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain)
%   [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain,DimMask)
%   [Success,Parameters]            = QP_GETDATA(FI,Domain,'getparams')
%   [Success,Size      ]            = QP_GETDATA(FI,Domain,DataFld,'size')
%   [Success,DimLabels ]            = QP_GETDATA(FI,Domain,DataFld,'dimlabels')
%   [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times')
%   [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times',T)
%   [Success,StNames   ]            = QP_GETDATA(FI,Domain,DataFld,'stations',S)
%   [Success,SubFields ]            = QP_GETDATA(FI,Domain,DataFld,'subfields',F)
%   [Success,TZshift   ,TZstr]      = QP_GETDATA(FI,Domain,DataFld,'timezone')
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%
%   where FI = qpfopen(filename)
%
%   The DataFld can be either a unique datafield name or an element of
%   the DataProps structure. The Domain parameter is optional and will
%   only be used if the function call to QP_GETDATA(FI,'domains')
%   returns a non-empty cell array. The subf(ield) parameter should only
%   be specified if and only if the call 'subfields' returns a non-empty
%   result.
%
%   See also QPREAD, QPFOPEN.
 
%
%   Extra function calls for QuickPlot.
%
%      [Success]                       = QP_GETDATA(FI,'options',OptionsFigure,'initialize')
%      [Success,NewFI     ,cmdargs]    = QP_GETDATA(FI,'options',OptionsFigure,OptionsCommand, ...)
%      [Success,hNew      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
%      [Success,Options   ]            = QP_GETDATA(FI,Domain,DataFld,'plotoptions',subf,t,station,m,n,k)

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

%
% Initialize output array. Set the Success flag to 0.
%
varargout=cell(1,nargout);
varargout{1}=0;
%
% Determine file type for calling the appropriate function
%
X=varargin;
if ~isempty(X) && isempty(X{1})
    ui_message('warning','Empty file information supplied, cancelling operation.')
    return
else
    Info=[];
    if ~isempty(X) && isstruct(X{1})
        tp = qp_gettype(X{1});
        if strcmp(tp,'unknown file type')
            % the second argument should then be of type DataFld
            if  ~isfield(X,'Name') || ~isfield(X,'Units') || ~isfield(X,'DimFlag') || ~isfield(X,'NVal')
                % This isn't a DataFld structure, so there is something
                % wrong. Most likely the filetype cannot be identified
                % because the structure didn't come from qpfopen.
                error('Unable to identify filetype associated with the first argument.')
            end
        else
            Info=X{1};
            X=X(2:end);
        end
    end
    %
    if isempty(Info)
        %
        % Assume that the user intents to use the last-opened NEFIS file.
        % This is consistent with the Delft3D-MATLAB functions.
        %
        Info=vs_use('lastread');
        if isempty(Info)
            error('No data file specified or data file not recognized.')
        end
    end
end
%
% Determine function to call ...
% If no function associated return to the caller (Success flag = 0).
%
Fcn = qp_file2function(Info);
if isempty(Fcn)
    return
end
%
% Try calling the function and catch errors.
%
lasterr('');
calltype='';
try
    %
    % Remove QuickPlot wrapper ...
    %
    [FI,Info]=qp_unwrapfi(Info);
    %
    % Call function and pass results if requested.
    %
    OK=1;
    switch nargout
        case {0,1}
            %
            % [Success]                       = QP_GETDATA(FI,'options',OptionsFigure,'initialize')
            %
            % Add empty array for dummy domain argument.
            %
            calltype='options1';
            feval(Fcn,FI,[],X{:});
        case 2
            %
            % [Success,Dimensions]            = QP_GETDATA(FI,'dimensions')
            % [Success,Locations ]            = QP_GETDATA(FI,'locations')
            % [Success,Domains   ]            = QP_GETDATA(FI,'domains')
            % [Success,DomainName]            = QP_GETDATA(FI,'domainname')
            % [Success,Quantities]            = QP_GETDATA(FI,'quantities')
            % [Success,DataProps ]            = QP_GETDATA(FI,Domain)
            % [Success,DataProps ]            = QP_GETDATA(FI,Domain,DimMask)
            % [Success,Parameters]            = QP_GETDATA(FI,Domain,'getparams')
            % [Success,Size      ]            = QP_GETDATA(FI,Domain,DataFld,'size')
            % [Success,DimLabels ]            = QP_GETDATA(FI,Domain,DataFld,'dimlabels')
            % [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times')
            % [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times',T)
            % [Success,StNames   ]            = QP_GETDATA(FI,Domain,DataFld,'stations',S)
            % [Success,SubFields ]            = QP_GETDATA(FI,Domain,DataFld,'subfields',F)
            % [Success,hNew      ]            = QP_GETDATA(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
            % [Success,Options   ]            = QP_GETDATA(FI,Domain,DataFld,'plotoptions',subf,t,station,m,n,k)
            % [Success,Data      ]            = QP_GETDATA(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
            %
            % Check for domain index ... add if necessary
            %
            if ~isempty(X)
                if isequal(X{1},'domainname')
                    %
                    % [Success,DomainName]            = QP_GETDATA(FI,'domainname')
                    %
                    % Add empty array for dummy domain argument.
                    %
                    varargout{2} = 'Domain';
                    if isfield(FI,'DomainName')
                        varargout{2} = FI(1).DomainName;
                    end
                    varargout{1} = OK;
                    return
                elseif isequal(X{1},'domains') || ...
                        isequal(X{1},'domainname') || ...
                        isequal(X{1},'dimensions') || ...
                        isequal(X{1},'locations') || ...
                        isequal(X{1},'quantities')
                    %
                    % [Success,Dimensions]            = QP_GETDATA(FI,'dimensions')
                    % [Success,Locations ]            = QP_GETDATA(FI,'locations')
                    % [Success,Domains   ]            = QP_GETDATA(FI,'domains')
                    % [Success,DomainName]            = QP_GETDATA(FI,'domainname')
                    % [Success,Quantities]            = QP_GETDATA(FI,'quantities')
                    %
                    % Add empty array for dummy domain argument.
                    %
                    calltype=X{1};
                    X={[] X{1}};
                elseif ischar(X{1}) && length(X)==1
                    %
                    % Cannot understand input.
                    %
                    error('Invalid input or incorrect number of arguments.')
                elseif isnumeric(X{1}) && isequal(size(X{1}),[1 1])
                    %
                    % [Success,DataProps ]            = QP_GETDATA(FI,Domain)
                    % [Success,DataProps ]            = QP_GETDATA(FI,Domain,DimMask)
                    % [Success,Parameters]            = QP_GETDATA(FI,Domain,'getparams')
                    % [Success,Size      ]            = QP_GETDATA(FI,Domain,DataFld,'size')
                    % [Success,DimLabels ]            = QP_GETDATA(FI,Domain,DataFld,'dimlabels')
                    % [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times')
                    % [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times',T)
                    % [Success,StNames   ]            = QP_GETDATA(FI,Domain,DataFld,'stations',S)
                    % [Success,SubFields ]            = QP_GETDATA(FI,Domain,DataFld,'subfields',F)
                    % [Success,hNew      ]            = QP_GETDATA(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
                    % [Success,Data      ]            = QP_GETDATA(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
                    %
                    % Nothing to do, we already have a domain number.
                    %
                    if length(X)>2
                        calltype=X{3};
                    elseif length(X)==2 && ischar(X{2})
                        calltype=X{2};
                    else
                        calltype='getprops2';
                    end
                else
                    %
                    % [Success,DataProps ]            = QP_GETDATA(FI,DimMask)
                    % [Success,Parameters]            = QP_GETDATA(FI,'getparams')
                    % [Success,Size      ]            = QP_GETDATA(FI,DataFld,'size')
                    % [Success,DimLabels ]            = QP_GETDATA(FI,DataFld,'dimlabels')
                    % [Success,Times     ]            = QP_GETDATA(FI,DataFld,'times')
                    % [Success,Times     ]            = QP_GETDATA(FI,DataFld,'times',T)
                    % [Success,StNames   ]            = QP_GETDATA(FI,DataFld,'stations',S)
                    % [Success,SubFields ]            = QP_GETDATA(FI,DataFld,'subfields',F)
                    % [Success,hNew      ]            = QP_GETDATA(FI,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
                    % [Success,Data      ]            = QP_GETDATA(FI,DataFld,'data',subf,t,station,m,n,k)
                    %
                    % No domain number, so use default domain number 0.
                    %
                    if length(X)>1
                        if ~ischar(X{2})
                            error('Encountered %s instead of command string.',class(X{2}))
                        else
                            calltype=X{2};
                        end
                    elseif length(X)==1 && ischar(X{1})
                        calltype=X{1};
                    else
                        calltype='getprops2';
                    end
                    X=cat(2,{0},X);
                end
            else
                %
                % [Success,DataProps ]            = QP_GETDATA(FI)
                %
                % No parameters at all, so no domain number. Use default domain
                % number 0.
                %
                calltype='getprops2';
                X={0};
            end
            if ~isempty(X) && isnumeric(X{end}) && isequal(size(X{end}),[1 5]) && isequal(calltype,'getprops2')
                %
                % [Success,DataProps ]            = QP_GETDATA(FI,Domain,DimMask)
                %
                % Do not pass the DimMask filter to the function to be called ...
                %
                DataProps=feval(Fcn,FI,X{1:end-1});
                %
                % ... and apply DimMask filter afterwards.
                %
                dims=cat(1,DataProps.DimFlag);
                flag=ismember(dims,X{end},'rows');
                DataProps=DataProps(flag);
                DataProps=separators(DataProps);
                varargout{2}=DataProps;
            else
                %
                % If DataFld is indicated by a character string (name) replace
                % it by an element of the DataProps structure ...
                %
                if length(X)>2 && ischar(X{2}) && ischar(X{3})
                    X{2}=qp_datafield_name2prop(Info,X{1},X{2});
                end
                %
                % Add T array if not specified ...
                %
                if length(X)>=1 && isequal(X{end},'times')
                    %
                    % [Success,Times     ]            = QP_GETDATA(FI,Domain,DataFld,'times')
                    %
                    X{end+1}=0;
                end
                %
                % ... and call the appropriate function for the file type.
                %
                if ~any(strcmp(calltype,gridcelldata('types')))
                    %
                    % for calls like: domains, dimensions, times, stations, plot, ...
                    %
                    varargout{2}=feval(Fcn,FI,X{:});
                else
                    %
                    % for calls like: data, gridcelldata, ...
                    %
                    [varargout{2},dummy] = hvslice(Fcn,FI,X);
                end
                %
                % ... in case of DataProps remove excessive separators
                %
                if length(X)==1
                    varargout{2}=separators(varargout{2});
                end
            end
        case 3
            %
            % [Success,NewFI     ,cmdargs]    = QP_GETDATA(FI,'options',OptionsFigure,OptionsCommand, ...)
            % [Success,Data      ,NewFI]      = QP_GETDATA(FI,'data',Quantity,DimSelection)
            % [Success,TZshift   ,TZstr]      = QP_GETDATA(FI,Domain,DataFld,'timezone')
            % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
            % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
            % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
            % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
            % [Success,hNew      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
            %
            % If a NewFI argument is returned, it may have to be wrapped in a
            % QuickPlot wrapper. However, it is sometimes returned as the
            % second argument, sometimes as third. Select appropriate index
            % using the variable fi. The default index is 3 ...
            %
            if isequal(X{1},'options')
                %
                % [Success,NewFI     ,cmdargs]    = QP_GETDATA(FI,'options',OptionsFigure,OptionsCommand, ...)
                % NewFI returned as second argument ...
                %
                fi=2;
                %
                % Add empty array for dummy domain argument.
                %
                calltype=sprintf('options3/%s',X{3});
                X=cat(2,{[]},X);
                [varargout{2:3}]=feval(Fcn,FI,X{:});
            elseif isequal(X{1},'data')
                %
                % [Success,Data      ,NewFI]      = QP_GETDATA(FI,'data',Quantity,DimSelection)
                % NewFI returned as third argument ...
                %
                fi=3;
                %
                calltype='objdata';
                if ischar(X{2})
                    X{2}=qp_datafield_name2prop(Info,'NEW_RESOURCE_CALL',X{2});
                end
                X=cat(2,{[]},X);
                [varargout{2:3}]=feval(Fcn,FI,X{:});
            else
                %
                % [Success,TZshift   ,TZstr]      = QP_GETDATA(FI,...,'timezone')
                % [Success,Data      ,NewFI]      = QP_GETDATA(FI,...)
                %
                % Check for domain number ... and add if necessary.
                %
                if isnumeric(X{1})
                    %
                    % Case with domain number ...
                    %
                    % [Success,TZshift   ,TZstr]      = QP_GETDATA(FI,Domain,DataFld,'timezone')
                    % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,...)
                    % Nothing to do, we already have a domain number.
                    %
                else
                    %
                    % Case without domain number ...
                    %
                    % [Success,TZshift   ,TZstr]      = QP_GETDATA(FI,DataFld,'timezone')
                    % [Success,Data      ,NewFI]      = QP_GETDATA(FI,DataFld,...)
                    % No domain number, so use default domain number 0.
                    %
                    X=cat(2,{0},X);
                end
                %
                % [Success,TZshift   ,TZstr]      = QP_GETDATA(FI,Domain,DataFld,'timezone')
                % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,...)
                %
                % If DataFld is indicated by a character string (name) replace
                % it by an element of the DataProps structure ...
                %
                if ischar(X{2})
                    X{2}=qp_datafield_name2prop(Info,X{1},X{2});
                end
                %
                calltype=X{3};
                if strcmp(calltype,'timezone')
                    % [Success,TZshift   ,TZstr]      = QP_GETDATA(FI,Domain,DataFld,'timezone')
                    %
                    % NewFI not returned ...
                    fi=[];
                    [varargout{2:3}]=feval(Fcn,FI,X{:});
                elseif strcmp(calltype,'plot')
                    %
                    % [Success,hNew      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'plot',Parent,Ops,hOld,subf,t,station,m,n,k)
                    %
                    %
                    % NewFI returned as third argument ...
                    fi=3;
                    [varargout{2:3}]=feval(Fcn,FI,X{:});
                else
                    %
                    % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
                    % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
                    % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
                    % [Success,Data      ,NewFI]      = QP_GETDATA(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
                    %
                    % NewFI returned as third argument ...
                    fi=3;
                    %
                    % Catch v_slice options for further processing a bit further down.
                    %
                    [arg2,arg3] = hvslice(Fcn,FI,X);
                    %
                    % Set output arguments.
                    %
                    varargout{2}=arg2;
                    varargout{3}=arg3;
                    %
                end
            end
            %
            % If NewFI was returned, add QuickPlot wrapper if necessary ...
            %
            if ~isempty(fi)
                varargout{fi}=qp_wrapfi(varargout{fi},Info);
            end
        case 4
            %
            % [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain)
            % [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain,DimMask)
            %
            % Check for domain number ... and add if necessary.
            %
            calltype='getprops4';
            if isempty(X)
                %
                % [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI)
                %
                % No parameters at all, so no domain number. Use default domain
                % number 0.
                %
                X={0};
            elseif isequal(size(X{1}),[1 1])
                %
                % [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain)
                % [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,Domain,DimMask)
                %
                % Nothing to do, we already have a domain number.
                %
            else
                %
                % [Success,DataFields,Dims ,NVal] = QP_GETDATA(FI,DimMask)
                %
                % No domain number, so use default domain number 0.
                %
                X=cat(2,{0},X);
            end
            %
            % Replace call by a call requesting a DataProps structure.
            %
            [OK,DataProps]=qp_getdata(FI,X{:});
            %
            % Split DataProps into Name, DimFlag and NVal arrays ...
            %
            if OK
                DataProps=separators(DataProps);
                varargout(2:4)={transpose({DataProps.Name}) cat(1,DataProps.DimFlag) [DataProps.NVal]};
            end
        otherwise
            error('Unexpected number of output arguments');
    end
    %
    % Set the success flag to 1.
    %
    varargout{1}=OK;
catch Ex
    %
    % Catch errors and show the error message.
    %
    if ~isempty(calltype)
        calltype = ['/' calltype];
    end
    qp_error(sprintf('Caught in qp_getdata%s:',calltype),Ex)
end


function [arg2,arg3] = hvslice(Fcn,FI,X)
T_=1; ST_=2; M_=3; N_=4; K_=5;
Props   = X{2};
DimFlag = Props.DimFlag;
%
v_slice=[];
h_slice=[];
if ~isempty(feval(Fcn,FI,X{1:2},'subfields'))
    sf = 1;
else
    sf = 0;
end
if DimFlag(K_)
    idxK = 3+sf+sum(DimFlag(1:K_)~=0);
    if length(X)>=idxK && iscell(X{idxK})
        h_slice = X{idxK};
        X{idxK}=0;
    end
end
if DimFlag(M_)
    idxM = 3+sf+sum(DimFlag(1:M_)~=0);
    if length(X)>=idxM && iscell(X{idxM})
        v_slice = X{idxM};
        X{idxM}=0;
    end
end
%
% If function does not support delivery of DataInCell but
% cell data requested, then convert call into a griddata
% call and take average later.
%
dic=0;
if ~X{2}.DataInCell
    switch X{3}
        case {'gridcelldata','celldata'}
            dic=1;
            X{3}(end-7:end-4)=[];
    end
end
%
% Get all data needed.
%
[arg2,arg3]=feval(Fcn,FI,X{:});
%
% Take average if cell data was required, but not provided.
% The following code does not yet work for 3D slices!
%
if dic
    triangles = 0;
    if isfield(X{2},'Tri') && isequal(X{2}.Tri,1)
        triangles = 1;
    elseif isfield(X{2},'Geom') && strcmp(X{2}.Geom,'TRI')
        triangles = 1;
    end
    if triangles
        for flc = {'Val','XComp','YComp','ZComp'}
            fld = flc{1};
            if isfield(arg2,fld)
                arg2.(fld) = mean(arg2.(fld)(arg2.TRI),2);
            end
        end
    end
end
%
% Handle h_slice options as needed.
%
if ~isempty(h_slice)
    arg2 = hslice(arg2,h_slice{:});
end
%
% Handle v_slice options as needed.
%
if ~isempty(v_slice)
    arg2 = vslice(arg2,v_slice{1},v_slice{2});
end
%
if isstruct(arg2)
    %
    % This was a QP_GETDATA call for grid or data: arg2 == Data.
    %
    Props=X{2};
    [arg2.Name]=deal(Props.Name);
    if ~isfield(arg2,'Units')
        if isfield(X{2},'Units')
            Units=X{2}.Units;
        else
            Units='';
        end
        [arg2.Units]=deal(Units);
        try
            [conversion,SIunit,dimensions]=qp_unitconversion(Units,'relative');
            if dimensions.temperature~=0
                if isfield(Props,'TemperatureType')
                    TempType = Props.TemperatureType;
                else
                    TempType = 'unspecified';
                end
                [arg2.TemperatureType] = deal(TempType);
            end
        end
    end
end
