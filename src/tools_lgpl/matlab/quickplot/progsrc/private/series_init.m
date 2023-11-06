function SERINFO=series_init(varargin),
%SERIES_INIT Initiates the creation of a series of bitmap files.
%   SERIES_INIT('FileBase')
%   Initializes writing a series of TIFF files
%   The TIFF filenames will be FileBase000.tif, FileBase001.tif,
%   FileBase002.tif, ...
%
%   SERIES_INIT(...,'digits',M)
%   Uses M digits for numbering the files (default = 3).
%
%   SERIES_INIT(...,'subcase',CaseID)
%   Appends string CaseID after the number and before the extension,
%   e.g. SERIES_INIT('FileBase','subcase','A') will generate files
%   called FileBase000A.tif, FileBase001A.tif, ...
%
%   SERIES_INIT(...,N)
%   Start counting at N.
%
%   SERIES_INIT(...,FileFormat)
%   Writes image files of the specified file format. The files
%   are created using the PRINT command:
%
%     'tif_' or 'tiff_' Tagged Image File Format (TIFF)
%     'jpg_' or 'jpeg_' Joint Photographic Experts Group (JPEG)
%     'bmp_'            Windows Bitmap (BMP)
%     'png_'            Portable Network Graphics (PNG)
%     'hdf_'            Hierarchical Data Format (HDF)
%
%   or using the IMWRITE command:
%
%     'tif' or 'tiff'   Tagged Image File Format (TIFF)
%     'jpg' or 'jpeg'   Joint Photographic Experts Group (JPEG)
%     'bmp'             Windows Bitmap (BMP)
%     'png'             Portable Network Graphics (PNG)
%     'hdf'             Hierarchical Data Format (HDF)
%     'pcx'             Windows Paintbrush (PCX)
%     'xwd'             X Window Dump (XWD)
%
%   S=SERIES_INIT(...)
%   Return a record to be used with SERIES_FRAME.
%
%   See also SERIES_FRAME, IMWRITE, PRINT.

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

N=0;
M=3;
subcase='';
Type='tif_';

i=1;
while i<=nargin
    if (i==1)
        filebase=varargin{i};
    elseif ischar(varargin{i})
        switch lower(varargin{i})
            case {'digit','digits'}
                i=i+1;
                if i>nargin
                    error('Error while reading digits.');
                end
                M=varargin{i};
            case {'sub','subcase'}
                i=i+1;
                if i>nargin
                    error('Error while reading digits.');
                end
                subcase=varargin{i};
            case {'tif_','tiff_','bmp_','png_','hdf_','tif','tiff','jpg','jpeg','bmp','png','hdf','pcx','xwd'}
                Type=varargin{i};
            case 'tiff0' % backward compatibility
                Type='tiff_';
            otherwise
                Type=varargin{i};
                if isequal(Type(end),'_') & ~isempty(strmatch(lower(Type(Type>58)),{'jpg_','jpeg_'}))
                    % OK accept it, it is probably something like jpg_ or jpeg90_
                else
                    error('Unrecognized option or file format: %s',varargin{i})
                end
        end
    else
        N=varargin{i};
    end
    i=i+1;
end

SERIES_animation.Base   = filebase;
SERIES_animation.Type   = Type;
SERIES_animation.Number = N;
SERIES_animation.Digits = M;
SERIES_animation.SubCase= subcase;

series_frame('store',SERIES_animation); % update SERIES_animation field in SERIES_FRAME ...

if nargout>0
    SERINFO=SERIES_animation;
end
