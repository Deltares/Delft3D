function Str=vec2str(OrigVec,varargin)
%VEC2STR Create a string representation of a vector.
%   S=VEC2STR(V)converts the rows vector V into a string representation S.
%   The string will contain opening and closing brackets. Depending on the
%   data the string may also contain the colon operator, and the ones and
%   zeros function calls.
%
%   ...,'nobrackets') prevents the output of the opening and closing
%   brackets.
%
%   ...,'noones') prevents the output of ones and zeros function calls.
%
%   Example
%      Str=vec2str([1 2 3 4 5 6 7 -1 -1 -1 -1 -1 NaN inf inf inf])
%      % returns '[ 1:7 -1*ones(1,5) NaN Inf Inf Inf ]'
%
%   See also STR2VEC, NUM2STR.

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

if all(OrigVec==round(OrigVec))
    % if all integers then use %i which is better for BIG INTEGERS
    fmt = '%i';
else
    fmt = '%g';
end
Brackets=1;
Ones=1;
for i=1:nargin-1
    switch lower(varargin{i})
        case {'nob','nobr','nobra','nobrac','nobrack','nobracke','nobracket','nobrackets'}
            Brackets=0;
        case {'noo','noon','noone','noones'}
            Ones=0;
        otherwise
            error('Unknown input argument %i.',i+1)
    end
end
if nargin<1
    error('Not enough input arguments.');
elseif isempty(OrigVec)
    if Brackets
        Str='[]';
    else
        Str='';
    end
elseif length(OrigVec)==1
    if Brackets
        Str=sprintf(['[ ',fmt,' ]'],OrigVec);
    else
        Str=sprintf(fmt,OrigVec);
    end
else
    ColVec = size(OrigVec,1)>1;
    if ColVec
        OrigVec = OrigVec.';
    end
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
    if ~isempty(E(2,~isfinite(E(1,:)))) % workaround for Matlab 5.2
        E(2,~isfinite(E(1,:)))=0;
    end
    E(3,~isfinite(E(1,:)))=E(1,~isfinite(E(1,:)));
    % create string
    if Brackets
        Str='[';
    else
        Str='';
    end
    for i=1:size(E,2)
        if E(4,i)==1
            Str=[Str sprintf([' ',fmt],E(1,i))];
        elseif E(4,i)==2
            Str=[Str sprintf([' ',fmt,' ',fmt],E([1 3],i))];
        elseif (E(2,i)==0) || isnan(E(2,i))
            if E(4,i)>3
                if Ones
                    if E(1,i)==0
                        Str=[Str sprintf(' zeros(1,%i)',E(4,i))];
                    else
                        Str=[Str sprintf([' ',fmt,'*ones(1,%i)'],E(1,i),E(4,i))];
                    end
                else
                    Str=[Str sprintf([' ',fmt],E(1,i)*ones(1,E(4,i)))];
                end
            else
                Str=[Str sprintf([' ',fmt],E(1,i)*ones(1,E(4,i)))];
            end
        else
            if E(2,i)==1
                Str=[Str sprintf([' ',fmt,':',fmt],E([1 3],i))];
            else
                Str=[Str sprintf([' ',fmt,':',fmt,':',fmt],E(1:3,i))];
            end
        end
    end
    if Brackets
        if ColVec
            Str=[Str ' ].'''];
        else
        Str=[Str ' ]'];
        end
    else
        Str(1)=[];
    end
end