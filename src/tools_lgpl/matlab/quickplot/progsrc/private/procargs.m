function [Struct,Err]=procargs(VARARGIN,CellFields,CellValues)
%PROCARGS General function for argument processing.
%   Function to process/parse the input arguments of a function in a
%   standardized manner. Generally, this function will be used to process
%   the input arguments grouped together by "varargin".
%
%   [Struct,Err]=PROCARGS(VARARGIN,Args)
%   VARARGIN is a cell array of the arguments to be processed. The
%   argument list may contain either a number of keyword-less arguments
%   in a fixed order, or a list of keyword-value pairs. Args is a
%   structure array containing fields Name, HasDefault, Default and
%   optionally List. Each element of the structure array defines one
%   keyword/parameter:
%      Args(i).Name      : the name/keyword
%      Args(i).HasDefault: boolean that indicates whether a default value
%                          is associated with this parameter.
%      Args(i).Default   : the default value (when HasDefault is true)
%      Args(i).List      : cell array (strings) or vector of acceptable
%                          values for parameter i.
%   Keyword-less arguments are assigned to the keywords in the order in
%   which they occur in the structure array. Parameters/keywords that do
%   not have an associated default value should be specified first in the
%   structure array Args. Keywords in keyword-value pairs may be
%   abbreviated or they case may be changed as long as the intended
%   keyword can be identified uniquely. The same holds for values of
%   keywords that can be assigned only a limited number of possible
%   string values (List field is a cell array).
%
%   The function returns an error string if an error is detected while
%   processing the arguments (errors in the syntax of the PROCARGS call
%   are handled as normal exceptions). The normal output will be a
%   structure containing a field for every parameter in the structure
%   array Args.
%
%   Example
%      arg(1).Name='parent';
%      arg(1).HasDefault=0;
%      %
%      arg(2).Name='type';
%      arg(2).HasDefault=1;
%      arg(2).Default='bar';
%      arg(2).List={'area','bar'};
%      %
%      arg(3).Name='color';
%      arg(3).HasDefault=1;
%      arg(3).Default='none';
%      %
%      [options,err]=procargs({1,'color','b'},arg);
%
%      returns
%
%      options =
%
%          parent: 1               % the keyword-less value 1
%            type: 'bar'           % the default value for "type"
%           color: 'b'             % from the keyword-value pair by the user
%
%
%   Note on the abbreviation of keywords
%
%      [options,err]=procargs({1,'C','b'},arg);
%
%   would have given the same result since 'C' uniquely identifies the
%   keyword 'color' in the list of keywords 'parent','type' and 'color'.
%
%
%   ALTERNATIVE SYNTAX:
%
%   [Struct,Err]=PROCARGS(VARARGIN,CellFields,CellValues)
%   CellFields and CellValues arguments together define the structure
%   array as used by CELL2STRUCT.
%
%   Same example
%
%      [options,err]=procargs({1,'color','b'}, ...
%         {'Name'  ,'HasDefault','Default','List'}, ...
%         {'parent',0  ,[]     ,[]
%          'type'  ,1  ,'bar'  ,{'area','bar'}
%          'color' ,1  ,'none' ,[]             });
%
%
%   Limitations: keyword-less parameters should not be strings because
%   they can easily be confused with keywords.
%
%
%   See also VARARGIN, CELL2STRUCT.

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

Struct=[];
Err='';

if nargin<2
    error('Not enough input arguments');
end
if ~isstruct(CellFields)
    if ~iscell(CellFields)
        error('Invalid second argument');
    elseif nargin<3
        error('Not enough input arguments');
    elseif ~iscell(CellValues)
        error('Invalid third argument');
    end
    Argument=cell2struct(CellValues,CellFields,2);
else
    Argument=CellFields;
end

Keywords={};
Number=[];
for i=1:length(Argument)
    if iscell(Argument(i).Name)
    elseif ischar(Argument(i).Name)
        Keywords{end+1}=Argument(i).Name;
        Number(end+1)=i;
    end
end

i=1;
while i<=length(VARARGIN)
    if ischar(VARARGIN{i})
        j=ustrcmpi(VARARGIN{i},Keywords);
        if j>0
            if i==length(VARARGIN)
                Err=['Missing value for ',Keywords{j}];
                return
            end
            j=Number(j);
            if Argument(j).HasDefault==2
                Err=['"',Argument(j).Name '" has been specified twice.'];
                return
            end
            Val=VARARGIN{i+1};
            if isfield(Argument,'List')
                [Val,Err]=ListCheck(Val,Argument(j));
                if ~isempty(Err), return; end
            end
            Argument(j).Default=Val;
            Argument(j).HasDefault=2; % using 2 here to indicate that this variable has been specified
            VARARGIN(i:i+1)=[];
        else
            i=i+1;
        end
    else
        i=i+1;
    end
end

for i=1:length(VARARGIN)
    if i>length(Argument)
        Err='Too many arguments.';
        return
    elseif Argument(i).HasDefault==2
        if ischar(VARARGIN{i})
            Err=sprintf('"%s" has been specified twice, or invalid keyword "%s".',Argument(i).Name,VARARGIN{i});
            return
        else
            Err=sprintf('"%s" has been specified twice: once using keyword, once as keywordless argument %i.',Argument(i).Name,i);
            return
        end
    end
    Val=VARARGIN{i};
    if isfield(Argument,'List')
        [Val,Err]=ListCheck(Val,Argument(i));
        if ~isempty(Err), return; end
    end
    Argument(i).Default=Val;
    Argument(i).HasDefault=2; % using 2 here to indicate that this variable has been specified
end

for i=1:length(Argument)
    if Argument(i).HasDefault==0
        Err=['No value assigned to ',Argument(i).Name];
    end
end

Struct=cell2struct({Argument(:).Default},{Argument(:).Name},2);


function [Val,Err]=ListCheck(ValIn,Argument)
Err='';
Val=ValIn;
if ~isempty(Argument.List)
    if iscellstr(Argument.List)
        jj=ustrcmpi(Val,Argument.List);
        OK=jj>0;
        if OK, Val=Argument.List{jj}; end
    else
        jj=find(Val,Argument.List);
        OK=~isempty(jj);
        if OK, Val=Argument.List(jj); end
    end
    if ~OK
        if iscellstr(Argument.List)
            List=Argument.List;
            if Argument.HasDefault
                jj=strmatch(Argument.Default,List,'exact');
                List{jj}=['{',List{jj},'}'];
            end
            Ops=sprintf('%s | ',List{:});
            Ops=['[ ',Ops(1:end-2),']'];
        elseif ischar(Argument.List)
            List=Argument.List;
            List=mat2cell(List,1,ones(1,length(List)));
            if Argument.HasDefault
                jj=strmatch(Argument.Default,List,'exact');
                List{jj}=['{',List{jj},'}'];
            end
            Ops=sprintf('%s | ',List{:});
            Ops=['[ ',Ops(1:end-2),']'];
        else
            List=Argument.List;
            List=mat2cell(List,1,ones(1,length(List)));
            for l=1:length(List)
                List{l}=sprintf('%g',List{l});
            end
            if Argument.HasDefault
                jj=find(Argument.List==Argument.Default);
                List{jj}=['{',List{jj},'}'];
            end
            Ops=sprintf('%s | ',List{:});
            Ops=['[ ',Ops(1:end-2),']'];
        end
        Err=['Invalid value for ',Argument.Name,'.',char(10),'Valid options are: ',Ops];
        return
    end
end
