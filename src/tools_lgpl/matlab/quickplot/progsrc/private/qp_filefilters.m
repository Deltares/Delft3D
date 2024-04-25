function filtertbl = qp_filefilters(filters)
%QP_FILEFILTERS Obtain a list of file filters.
%   FILTERS = QP_FILEFILTERS(FILTERS) returns
%   * the filter corresponding to the latest file type if FILTERS equals
%     'latest'
%   * the user selected filters if FILTERS equals 'selected'
%   * the user selected filters and the filter corresponding to the latest
%     file type if FILTERS equals 'selected+'
%   * all filters if FILTERS equals 'all'
%   * the filter corresponding to the file type specified by FILTERS

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

filtertbl={...
    '*.dat;*.ada;*.hda'                                    'Delft3D Output Files'              'nefis'              0
    '*.grd;*.rgf'                                          'Delft3D Grid Files'                'wlgrid'             0
    '*.mdf;*.mdu;*.md1d;*.mdw'                             'Delft3D Simulation Files'          'md*-file'           0
    '*.mesh'                                               'Mike Flexible Mesh Files'          'mikemesh'           0
    '*.14;*.gr3'                                           'Adcirc Mesh Files'                 'adcircmesh'         0
    '*.shy'                                                'SHYFEM Mesh Files'                 'SHYFEM mesh'        0
    '*_grd.dat;*.2dm'                                      'SMS Mesh Files'                    'smsmesh'            0
    '*.mesh;*.node;*.ele;*.n;*.e'                          'EasyMesh and Triangle Mesh Files'  'nodelemesh'         0
    '*.gem'                                                'GeoSystems Mesh Files'             'geomesh'            0
    '*.msh'                                                'Gmsh Files'                        'gmsh'               0
    '*.bct;*.bcc;*.bcb;*.bc'                               'Delft3D-FLOW Bound. Cond. Files'   'bct'                0
    'bagdpt*.*'                                            'Delft3D-MOR Ascii Dredging Output' 'bagdpt'             0
    '*.am?;*.spw;*.wnd'                                    'Delft3D/SOBEK Meteo Files'         'asciiwind'          0
    'gcmplt.*;gcmtsr.*'                                    'ECOMSED Binary Files'              'ecomsed-binary'     0
    '*.stu;*.pst'                                          'JSPost Files'                      'JSPost'             0
    '*.csv'                                                'CSV Files (Exported from QP)'      'samples'            0
    '*.xyz'                                                'Sample Files'                      'samples'            0
    '*.grb;*.grib;*.grib1;*.grib2'                         'GRIB Files'                        'grib'               0
    '*.nc'                                                 'NetCDF Files'                      'NetCDF'             0
    '*.hdf;*.hdf5'                                         'HDF5 Files'                        'HDF5'               0
    'sds-*'                                                'SIMONA SDS Files'                  'waquasds'           0
    '*.*'                                                  'SIMONA Box Files'                  'boxfile'            0
    '*.his;*.map;*.plo;*.psf;*.lga'                        'Delwaq Binary Files'               'delwaqbin'          0
    '*.tim'                                                'Delwaq Time Series Input Files'    'DelwaqTimFile'      0
    '*.arc;*.am?;*.asc'                                    'ARC/INFO Ascii Grid Files'         'arcgrid'            0
    '*.grd'                                                'Surfer Ascii or Binary Grid Files' 'surfer'             0
    '*.map'                                                'PC-Raster Files'                   'pcraster'           0
    '*.hdr'                                                'BIL/HDR Files'                     'bil/hdr'            0
    '*.ldb;*.pol'                                          'Land Boundary and Polygon Files'   '>tekal'             1
    '*.tek;*.ann;*.ldb;*.pol;*.spl;*.tka;*.tkp;*.tkf'      'Tekal Data Files'                  'tekal'              0
    '*.dxf'                                                'AutoCAD DXF Files'                 'AutoCAD DXF'        1
    '*.geojson'                                            'GeoJSON Files'                     'GeoJSON'            1
    '*.shp'                                                'Shape Files'                       'shape'              1
    '*.gen'                                                'ArcInfo Ungenerate Files'          'ArcInfoUngenerate'  1
    '*.bna'                                                'BNA Files'                         'BNA File'           1
    '*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.png;*.pcx;*.xwd'    'Bitmap Files'                      'bitmap'             0
    '*.fun;*.daf'                                          'Unibest Files'                     'unibest'            0
    '*.sp1;*.sp2;*.s1d;*.s2d'                              'SWAN Spectral Files'               'SWAN spectral'      0
    '*.slf;*.out;*.res'                                    'Telemac Files'                     'telemac'            0
    '*.dt0;*.dt1;*.dt2;*.dfs0;*.dfs1;*.dfs2;*.dfs3;*.dfsu' 'Mike Data Files'                   'mike0'              0
    'DEFTOP.1;NETWORK.NTW'                                 'Sobek Networks'                    'sobek1d'            0
    '*.inc;*.crs;*.bin'                                    'FLS Files'                         'fls'                0
    '*.seq'                                                'AukePC Files'                      'aukepc'             0
    '*.mat'                                                'MATLAB Files (Exported from QP)'   'matlab'             0
    '*.qpses'                                              'QUICKPLOT Session Files'           'qpsession'          0
    '*.sma'                                                'Shipma Project Files'              'shipma'             0
    '*.dmp'                                                'CFX4 Dump Files'                   'CFX dmp'            0
    '*.noos'                                               'NOOS and MATROOS Files'            'NOOS time series'   0
    '*.wml'                                                'WaterML Files'                     'WaterML2'           0
    };

if nargin<1
    filters = '';
end
switch filters
    case 'all'
    case 'files-with-lines' % used by gridview to open files with "line" data
        isldb = cat(1,filtertbl{:,4});
        filtertbl(isldb~=1,:) = [];
        return % don't want to sort this one
    case 'latest'
        lasttp  = qp_settings('LastFileType','nefis');
        iFull = strncmp(lasttp,filtertbl(:,3),length(lasttp));
        filtertbl = filtertbl(iFull,:);
    case {'selected','selected+'}
        filterstring = qp_settings('filefilterselection');
        iquotes = findstr('"',filterstring);
        selected = cell(1,length(iquotes)/2);
        for i = 1:length(iquotes)/2
            selected{i} = filterstring(iquotes((i-1)*2+1)+1:iquotes(i*2)-1);
        end
        [selected,iFull] = intersect(filtertbl(:,2),selected);
        %
        % add last file type
        %
        if strcmp(filters,'selected+')
            lasttp  = qp_settings('LastFileType','nefis');
            ilasttp = find(strncmp(lasttp,filtertbl(:,3),length(lasttp)));
            ilasttp = ilasttp(~ismember(ilasttp,iFull));
            iFull = [iFull;ilasttp(:)];
        end
        %
        filtertbl = filtertbl(iFull,:);
    otherwise % filters is name of one of the file types
        lasttp = filters;
        iFull = strncmp(lasttp,filtertbl(:,3),length(lasttp));
        filtertbl = filtertbl(iFull,:);
end
[dum,Reorder] = sort(lower(filtertbl(:,2)));
filtertbl = filtertbl(Reorder,1:3);
