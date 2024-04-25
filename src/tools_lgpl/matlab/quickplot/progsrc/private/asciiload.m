function z=asciiload(filename,varargin)
%ASCIILOAD A compiler compatible version of LOAD -ASCII.
%   X=ASCIILOAD('FileName')
%   Load data from specified ASCII file into the
%   variable X.

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

fid=fopen(filename,'r','n','US-ASCII');
comment='%';
formatting={};
i=0; % line number in file
if nargin>2
    j=1;
    while j<=length(varargin)
        cmd = lower(varargin{j});
        switch cmd
            case 'seek' % seek but keep track of line number
                offset = varargin{j+1};
                while ftell(fid)<offset && ~feof(fid)
                    i=i+1;
                    fgetl(fid);
                end
                if ftell(fid)>offset
                    i=i-1;
                    fseek(fid,offset,-1);
                end
                j = j+2;
            case 'skiplines'
                nlines = varargin{j+1};
                if nlines>0 % if nlines=0, then i would become [] and during line counting it remains []
                    for i = 1:nlines
                        fgetl(fid);
                    end
                end
                j = j+2;
            case 'format'
                formatting = varargin{j+1};
                j = j+2;
            case 'comment'
                comment = varargin{j+1};
                j = j+2;
            otherwise
                error('Unkown command option: %s',cmd)
        end
    end
end
ll=0; % line length
nl=0; % number of data lines processed
z = zeros(1000,1);
%
ncval = 0;
ijcval = zeros(1000,2);
cval = cell(1000,1);
%
nlalloc = 1000; % number of data lines allocated
prevcomma=0;
tryquick=1;
while ~feof(fid)
    i = i+1;
    if ll~=0 && tryquick
        loc = ftell(fid);
        data = textscan(fid,repmat('%f ',1,ll), ...
            'CommentStyle',comment);
        if ~feof(fid)
            % reading problem encountered, go slowly
            tryquick = 0;
        else
            % if we encounter NaN in the results, verify whether there are
            % really NaNs in the file or that there are format conflicts.
            % Note: lines with N*LL numbers are not caught by this method.
            verify = 0;
            for cl = 1:length(data)
                if any(isnan(data{cl}))
                    verify = 1;
                    break
                end
            end
            if verify
                fseek(fid,loc,-1);
                data2 = textscan(fid,repmat('%f ',1,ll), ...
                    'CommentStyle',comment, ...
                    'EmptyValue',inf);
            end
            % end of file reached, but is data consistent?
            ndata = length(data{end});
            for cl = 1:length(data)
                if length(data{cl})>ndata
                    % data column length inconsistent
                    data{cl} = data{cl}(1:ndata);
                    tryquick = 0;
                    break
                elseif verify && any(isnan(data{cl}) & ~isnan(data2{cl}))
                    tryquick = 0;
                end
            end
        end
        if tryquick
            z(nl+(1:ndata),1:ll) = cat(2,data{:});
            nl = nl+ndata;
            i = i+ndata;
        else
            % attempt failed, go back
            fseek(fid,loc,-1);
        end
    end
    txt=fgetl(fid);
    if feof(fid) && (~ischar(txt) || (length(txt)==1 && txt==26)) % EOF signal
        break
    end
    cni=0;
    perc=strfind(txt,comment);
    if ~isempty(perc)
        str=txt(1:perc-1);
    else
        str=txt;
    end
    [values,n,err,ni]=sscanf(str,'%f%*[ \t]',[1 inf]);
    while ni<=length(str)
        cni=cni+ni-1;
        str=str(ni:end);
        if str(1)==','
            if ~prevcomma
                cni=cni+1;
                str=str(2:end);
                prevcomma=1;
                [values2,n,err,ni]=sscanf(str,'%f%*[ ]',[1 inf]);
                if n>0
                    prevcomma=0;
                    values=cat(2,values,values2);
                end
            else
                spaces=repmat(' ',1,cni-1);
                fclose(fid);
                error('Missing data between comma''s on line %i of ASCII file %s:\n%s\n%s^',i,filename,txt,spaces)
            end
        else
            %
            % In recent versions of MATLAB, the sscanf command above
            % will read Inf's and NaN's. The Inf and NaN checks below are
            % included for backward compatibility.
            %
            kyw=0;
            if length(str)>3
                if strcmpi(str(1:4),'-inf')
                    kyw=4;
                    kywval=-inf;
                end
            end
            if length(str)>2
                if strcmpi(str(1:3),'inf')
                    kyw=3;
                    kywval=inf;
                elseif strcmpi(str(1:3),'nan')
                    kyw=3;
                    kywval=NaN;
                end
            end
            if ~isempty(str)
                switch str(1)
                    case {'?','.'}
                        kyw=1;
                        kywval=NaN;
                    case {'/','-'} % YYYY/MM/DD or YYYY-MM-DD
                        icol = length(values);
                        if icol>numel(formatting) || isequal(formatting{icol},'datetime')
                            format = [str(1),'%d',str(1),'%d %d:%d:%f'];
                            what = 'datetime';
                        elseif isequal(formatting{icol},'date')
                            format = [str(1),'%d',str(1),'%d'];
                            what = 'date';
                        else
                            fclose(fid);
                            spaces=repmat(' ',1,cni);
                            error('Unknown text on line number %i of ASCII file %s:\n%s\n%s^',i,filename,txt,spaces)
                        end
                        [A,cnt,~,next] = sscanf(str,format,5);
                        if cnt==2
                            values(icol) = values(icol)*10000 + A(1)*100 + A(2);
                        elseif cnt==5
                            values(icol) = values(icol)*10000 + A(1)*100 + A(2) + A(3)*0.01 + A(4)*0.0001 + A(5)*0.000001;
                        else
                            fclose(fid);
                            spaces=repmat(' ',1,cni+cnt-1);
                            error('Incomplete %s specification on line number %i of ASCII file %s:\n%s\n%s^',what,i,filename,txt,spaces)
                        end
                        kyw=next-1;
                        kywval = [];
                    case ':'
                        [A,cnt,err,next] = sscanf(str,':%d:%d',2);
                        if cnt==2 && isempty(err) && length(values)==2 % only for first column
                            values(2) = values(2)*10000 + A(1)*100 + A(2);
                            kyw=next-1;
                            kywval = [];
                        end
                    case ''''
                        [A,cnt,err,next] = sscanf(str,' ''%[^'']'' ',1);
                        ncval = ncval+1;
                        ijcval(ncval,:) = [nl+1 length(values)+1];
                        cval{ncval} = A;
                        kyw=next-1;
                        kywval=NaN;
                end
            end
            if kyw>0
                if length(str)>kyw
                    switch str(kyw+1)
                        case {' ',char(9),','}
                        otherwise
                            kyw=0;
                    end
                end
            end
            if kyw>0
                prevcomma=0;
                values=cat(2,values,kywval);
                cni=cni+kyw;
                str=str(kyw+1:end);
                [values2,n,err,ni]=sscanf(str,'%f',[1 inf]);
                if n>0
                    values=cat(2,values,values2);
                end
            else
                fclose(fid);
                spaces=repmat(' ',1,cni);
                error('Unknown text on line number %i of ASCII file %s:\n%s\n%s^',i,filename,txt,spaces)
            end
        end
    end
    if prevcomma
        spaces=repmat(' ',1,cni);
        fclose(fid);
        error('Missing value after comma at end of line %i of ASCII file %s:\n%s\n%s^',i,filename,txt,spaces)
    end
    if ~isempty(values)
        if ll>0 && length(values)~=ll
            fclose(fid);
            error('Number of columns of ASCII file %s do not match.\nLine %i has %i columns, whereas the preceeding lines have %i columns:\n%s',filename,i,length(values),ll,txt)
        else
            ll=length(values);
            nl = nl+1;
            if nl>nlalloc
                nlalloc = 2*nlalloc;
                z(nlalloc,1) = 0;
            end
            z(nl,1:ll) = values;
        end
        %else
        % empty line, skip it
    end
end
fclose(fid);
z(nl+1:end,:)=[];
%
if ncval>0
    ijcval = ijcval(1:ncval,:);
    cval = cval(1:ncval);
    %
    cols = unique(ijcval(:,2));
    if length(cols)>1 || cols<size(z,2) || ~isequal(ijcval(:,1)',1:size(z,1))
        error('String column should be last data column.')
    else
        z = {z(:,1:cols-1) cval};
    end
end
