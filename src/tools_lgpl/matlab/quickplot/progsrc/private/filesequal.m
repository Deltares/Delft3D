function [Equal,Msg]=filesequal(File1,File2,varargin)
%FILESEQUAL Determines whether the contents of two files is the same.
%   FILESEQUAL(FILENAME1,FILENAME2) is 1 if the contents of the two files
%   are the same.
%
%   See Also ISEQUAL, VARDIFF, MATDIFF.

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

Equal = false;
Msg='';
i=1;
skip=0;
while i<=length(varargin)
    if ischar(varargin{i})
        switch lower(varargin{i})
            case 'skip'
                skip=varargin{i+1};
                i=i+1;
        end
    end
    i=i+1;
end
fid1  = fopen(File1,'r');
if fid1<0
    Msg=sprintf('Cannot open %s.',File1);
    return
end
fid2  = fopen(File2,'r');
if fid2<0
    Msg=sprintf('Cannot open %s.',File2);
    fclose(fid1);
    return
end
Equal = isequal(fid1,fid2);
if Equal
    fclose(fid1);
    return
end
fseek(fid1,0,1);
fseek(fid2,0,1);
FS1   = ftell(fid1);
FS2   = ftell(fid2);
Equal = FS1==FS2;
if ~Equal
    Msg='The file sizes are different.';
    fclose(fid1);
    fclose(fid2);
    return
end
fseek(fid1,skip,-1);
fseek(fid2,skip,-1);
while ~feof(fid1) && ~feof(fid2) && Equal
    kb1   = fread(fid1,[1 1000],'*uchar');
    kb2   = fread(fid2,[1 1000],'*uchar');
    Equal = isequal(kb1,kb2);
end
Equal = Equal & feof(fid1) & feof(fid2);
fclose(fid1);
fclose(fid2);
if ~Equal
    Msg='The file contents is different.';
end
