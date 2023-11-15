function i=wildstrmatch(wcstr,strs)
%WILDSTRMATCH Find matching strings using wildcards.
%   I = WILDSTRMATCH(STR,STRS) looks through the rows of the character
%   array or cell array of strings STRS to find strings that match
%   STR which might contain wildcards (? and *), returning a logical
%   array with 1's for matching entries and 0's for non-matching
%   entries. Trailing blanks are ignored.
%   The following definition is used for the wildcards:
%      ? : one character
%      * : zero or more characters
%
%   Example:
%      i = wildstrmatch('*max',strvcat('max','minimax','maximum'))
%      % returns i=[1;1;0] since the first and second entry end on 'max'.
%
%   Example:
%      i = wildstrmatch('??x*',{'max','minimax','maximum'})
%      % returns i=[1;0;1] since the first and last entry have 'x' as the
%      % third character.
%
%   See also STRMATCH.

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

wcstr=deblank(wcstr);
Star=0;
TempType=0;
Frm=[];
for c=1:length(wcstr)
   switch wcstr(c)
      case '*'
         TempType=1;
      case '?'
         if length(Frm)==0 % first Frm
            Frm(1).Type=2;
            Frm(1).Param=1;
         elseif Frm(end).Type==2 % processing set of question marks
            Frm(end).Param=Frm(end).Param+1;
         else % first question mark after character
            Frm(end+1).Type=2;
            Frm(end).Param=1;
         end
      otherwise % character
         if (length(Frm)==0) | TempType>0 % first Frm or star read
            Frm(end+1).Type=TempType;
            Frm(end).Param=wcstr(c);
         elseif Frm(end).Type<2 % processing set of characters
            Frm(end).Param=[Frm(end).Param wcstr(c)];
         else % first character after question mark
            Frm(end+1).Type=TempType;
            Frm(end).Param=wcstr(c);
         end
         TempType=0;
   end
end
if TempType==1
   Frm(end+1).Type=TempType;
   Frm(end).Param='';
end

if iscellstr(strs)
   Nstrs=length(strs);
else
   Nstrs=size(strs,1);
end

i=logical(zeros(Nstrs,1));

for si=1:Nstrs
   if iscellstr(strs)
      Str={strs{si}};
   else
      Str={deblank(strs(si,:))};
   end
   for f=1:length(Frm)
      switch Frm(f).Type
         case 0 % set of characters starting immediately
            s=1;
            while s<=length(Str)
               if length(Str{s})<length(Frm(f).Param)
                  Str(s)=[];
               elseif isequal(Frm(f).Param,Str{s}(1:length(Frm(f).Param)))
                  TmpStr=Str{s};
                  Str{s}=TmpStr((length(Frm(f).Param)+1):end);
                  s=s+1;
               else % ~isequal
                  Str(s)=[];
               end
            end
         case 1 % set of characters somewhere
            if isempty(Frm(f).Param)
               if ~isempty(Str)
                  Str={''};
               end
            else
               s=1;
               lStr=length(Str);
               while s<=lStr
                  if length(Str{s})<length(Frm(f).Param)
                     L=[];
                  elseif length(Str{s})==length(Frm(f).Param)
                     if isequal(Str{s},Frm(f).Param)
                        L=1;
                     else
                        L=[];
                     end
                  else
                     L=strfind(Str{s},Frm(f).Param);
                  end
                  if isempty(L) % not found
                     Str(s)=[];
                     lStr=lStr-1;
                  else % one or more times found
                     for li=2:length(L)
                        TmpStr=Str{s};
                        Str{end+1}=TmpStr((L(li)+length(Frm(f).Param)):end);
                     end
                     TmpStr=Str{s};
                     Str{s}=TmpStr((L(1)+length(Frm(f).Param)):end);
                     s=s+1;
                  end
               end
            end
         case 2 % number of unspecified characters
            s=1;
            while s<=length(Str)
               if length(Str{s})<Frm(f).Param
                  Str(s)=[];
               else
                  TmpStr=Str{s};
                  Str{s}=TmpStr((Frm(f).Param+1):end);
                  s=s+1;
               end
            end
      end
   end
   if ~isempty(Str) % wildstrmatch if any search ended in an empty str
      i(si)=~isempty(strmatch('',Str,'exact'));
   end
end
