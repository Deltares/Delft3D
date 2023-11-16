function filetype=vs_type(vs)
%VS_TYPE Determines the type of the NEFIS file.
%   Type=VS_TYPE(NFStruct) returns the type of NEFIS file based on the file
%   contents. If the function cannot determine the file type, it will
%   return the string 'unknown'. Knowing the file type is not necessary for
%   reading the contents of the NEFIS file; however, it is necessary for
%   understanding the data in the file. If the NFStruct is not a structure
%   corresponding to a NEFIS file, it will return the string 'non-nefis'.
%
%   If the file (NFStruct) is not specified, the NEFIS that was last opened
%   by VS_USE will be used to read the data. A file structure NFStruct can
%   be obtained as output argument from the function VS_USE.
%
%   See also VS_USE, VS_DISP, VS_LET, VS_GET, VS_DIFF, VS_FIND.

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

if nargin==0
   vs=vs_use('lastread');
end
if ~isstruct(vs)
   filetype='non-nefis';
elseif ~isfield(vs,'GrpDat') | ~isfield(vs,'GrpDef') | ...
      ~isfield(vs,'CelDef') | ~isfield(vs,'ElmDef') | ...
      ~isfield(vs,'DatExt') | ~isfield(vs,'DefExt')
   filetype='non-nefis';
elseif isstruct(vs_disp(vs,'com-version',[]))
   filetype='Delft3D-com';
elseif isstruct(vs_disp(vs,'map-version',[]))
   filetype='Delft3D-trim';
elseif isstruct(vs_disp(vs,'his-version',[]))
   filetype='Delft3D-trih';
elseif isstruct(vs_disp(vs,'dro-version',[]))
   filetype='Delft3D-trid';
elseif isstruct(vs_disp(vs,'trk-series',[]))
   filetype='Delft3D-track';
elseif isstruct(vs_disp(vs,'map-series','HSIGN'))
   filetype='Delft3D-hwgxy';
   if isstruct(vs_disp(vs,'map-series','DATE_TIME'))
      filetype='Delft3D-wavm';
   end
elseif isstruct(vs_disp(vs,'MAPATRANNTR','NTR'))
   filetype='Delft3D-tram';
elseif isstruct(vs_disp(vs,'HISTRANNTRM','NTRM'))
   filetype='Delft3D-trah';
elseif isstruct(vs_disp(vs,'MAPBOT','NTMBOT'))
   filetype='Delft3D-botm';
elseif isstruct(vs_disp(vs,'HISBOT','NTHBOT'))
   filetype='Delft3D-both';
elseif isstruct(vs_disp(vs,'MAPBGREF',[]))
   filetype='Delft3D-bagr';
elseif isstruct(vs_disp(vs,'DELWAQ_PARAMS',[])) | isstruct(vs_disp(vs,'delwaq_params',[]))
   [X,Chk]=vs_get(vs,'DELWAQ_PARAMS','TYPE','quiet');
   if ~Chk
      [X,Chk]=vs_get(vs,'delwaq_params','type','quiet');
   end
   if ~Chk
      filetype='Delft3D-waq-unknown';
   else
      filetype=strcat('Delft3D-waq-',lower(deblank(X)));
   end
elseif isstruct(vs_disp(vs,'DELPAR_PARAMS',[])) | isstruct(vs_disp(vs,'delpar_params',[]))
   if isstruct(vs_disp(vs,'PLO-VERSION',[])) | isstruct(vs_disp(vs,'plo-version',[]))
      filetype='Delft3D-par-plot';
   elseif isstruct(vs_disp(vs,'PSF-VERSION',[])) | isstruct(vs_disp(vs,'psf-version',[]))
      filetype='Delft3D-par-psf';
   else
      [X,Chk]=vs_get(vs,'DELPAR_PARAMS','TYPE','quiet');
      if ~Chk
         [X,Chk]=vs_get(vs,'delpar_params','type','quiet');
      end
      if ~Chk
         filetype='Delft3D-par-unknown';
      else
         if isequal(lower(deblank(X)),'map-file[part]')
            filetype='Delft3D-par-map';
         elseif isequal(lower(deblank(X)),'his-file[part]')
            filetype='Delft3D-par-his';
         else
            filetype=strcat('Delft3D-par-',lower(deblank(X)));
         end
      end
   end
elseif isstruct(vs_disp(vs,'PARSE-REL-GRP','PARSE-REL'))
   filetype='sobek-m';
elseif isstruct(vs_disp(vs,'FLOW-DES-GROUP','NO_TIMES_MAP'))
   filetype='sobek-o';
elseif isstruct(vs_disp(vs,'FLOW-RES-GROUP','ISTEP'))
   filetype='sobek-r';
elseif isstruct(vs_disp(vs,'GEOMETRY','ICOD'))
   filetype='Skylla';
elseif isstruct(vs_disp(vs,'COARSEGRID','X-CG'))
   filetype='TRITON';
elseif isstruct(vs_disp(vs,'POINTS','X-PTS'))
   filetype='TRITON';
elseif isstruct(vs_disp(vs,'RAY01','XRAY-01'))
   filetype='TRITON';
elseif isstruct(vs_disp(vs,'GRID_coor','X_coor'))
   filetype='Pharos';
elseif isstruct(vs_disp(vs,'WANDA',[]))
    if any(strncmp(vs_disp(vs,[]),'OUTP_',5))
        filetype='WANDA-output';
    else
        filetype='WANDA-input';
    end
elseif isstruct(vs_disp(vs,'WAVNT','NTWAV')) % communication file by swan ...
   filetype='Delft3D-com';
elseif isstruct(vs_disp(vs,'CURTIM','TIMCUR')) % communication file by swan ...
   filetype='Delft3D-com';
elseif isstruct(vs_disp(vs,'map-info-series',[]))
   filetype='Delft3D-trim';
elseif isstruct(vs_disp(vs,'his-info-series',[]))
   filetype='Delft3D-trih';
else
   filetype='unknown';
end
