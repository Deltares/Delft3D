function Fcn=qp_file2function(Info)
%QP_FILE2FUNCTION Retrieve function name associated with file structure.

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

persistent MissingFileTypes
%
% Define for each supported file type the appropriate function to be called.
%
F={'Delft3D-com'                'd3d_comfil'
    'Delft3D-trim'               'd3d_trimfil'
    'Delft3D-trih'               'd3d_trihfil'
    'Delft3D-trid'               'd3d_tridfil'
    'Delft3D-track'              'd3d_tridfil'
    'Delft3D-tram'               'd3d_tramfil'
    'Delft3D-trah'               'd3d_trahfil'
    'Delft3D-botm'               'd3d_botmfil'
    'Delft3D-both'               'd3d_bothfil'
    'Delft3D-hwgxy'              'd3d_hwgxyfil'
    'Delft3D-wavm'               'd3d_hwgxyfil'
    'Delft3D-bagr'               'd3d_bagrfil'
    'Delft3D-waq-history'        'd3d_waqfil'
    'Delft3D-waq-his'            'd3d_waqfil'
    'Delft3D-waq-map'            'd3d_waqfil'
    'Delft3D-par-detailed_plot'  'd3d_waqfil'
    'Delft3D-par-history'        'd3d_waqfil'
    'Delft3D-par-his'            'd3d_waqfil'
    'Delft3D-par-map'            'd3d_waqfil'
    'Delft3D-par-plot'           'd3d_waqfil'
    'Delft3D-par-psf'            'd3d_waqfil'
    'DelwaqMAP'                  'd3d_waqfil'
    'DelwaqHIS'                  'd3d_waqfil'
    'DelwaqLGA'                  'd3d_waqfil'
    'DelparPLOT'                 'd3d_waqfil'
    'Delft3D D-Flow FM'          'd3d_simfil'
    'Delft3D D-Wave'             'd3d_simfil'
    'Delft3D D-Flow1D'           'd3d_simfil'
    'Delft3D D-Flow2D3D'         'd3d_simfil'
    'Delft3D Coupled Model'      'd3d_simfil'
    'iMOD-idf'                   'imodfil'
    'Skylla'                     'skyllafil'
    'Pharos'                     'pharosfil'
    'arcgrid'                    'arcgridfil'
    'SURFER'                     'arcgridfil'
    'asciiwind'                  'asciiwindfil'
    'PCraster'                   'pcrasterfil'
    'SIMONA SDS FILE'            'waquafil'
    'MikeCTDT'                   'mikezerofil'
    'MikeDFS'                    'mikezerofil'
    'MikeXFS'                    'mikezerofil'
    'FLS-inc'                    'flsfil'
    'FLS-bin'                    'flsfil'
    'FLS-his'                    'flsfil'
    'FLS-cross'                  'flsfil'
    'GRIB'                       'gribfil'
    'wlgrid'                     'gridfil'
    'tekal'                      'tekalfil'
    'AutoCAD DXF'                'tekalfil'
    'GeoJSON'                    'tekalfil'
    'BNA File'                   'tekalfil'
    'ArcInfoUngenerate'          'tekalfil'
    'ESRI-Shape'                 'tekalfil'
    'DelwaqTimFile'              'tekalfil'
    'LexYacc_TimeTable'          'tekalfil'
    'NOOS time series'           'tekalfil'
    'WaterML2'                   'tekalfil'
    'Unibest'                    'unibestfil'
    'Samples'                    'samplesfil'
    'MorfTree'                   'morftreefil'
    'AukePC'                     'aukepcfil'
    'Bct'                        'bctfil'
    'CFX dmp'                    'cfxfil'
    'telemac'                    'telemacfil'
    'JSPost'                     'jspostfil'
    'bagdpt'                     'bagdptfil'
    'bitmap'                     'bitmapfil'
    'video'                      'bitmapfil'
    'matlab'                     'matlabfil'
    'SOBEK network'              'sobekfil'
    'SOBEK River network'        'sobekfil'
    'NetCDF'                     'netcdffil'
    'netCDF-PHAROS'              'pharosfil'
    'HDF5'                       'hdffil'
    'XMDF'                       'hdffil'
    'SWAN spectral'              'swanfil'
    'TRITON'                     'nfs_tritonfil'
    'bil/hdr'                    'bilhdrfil'
    'QP Data Resource Object'    'resourceobject'
    'ecomsed-binary'             'ecomsedfil'
    'MikeFM mesh'                'flexmeshfil'
    'EasyMesh mesh'              'flexmeshfil'
    'Triangle mesh'              'flexmeshfil'
    'Adcirc 14 mesh'             'flexmeshfil'
    'SHYFEM mesh'                'flexmeshfil'
    'GeoSystems mesh'            'flexmeshfil'
    'Gmsh'                       'flexmeshfil'
    'SMS mesh'                   'flexmeshfil'
    'SMS mesh2d'                 'flexmeshfil'
    'diff'                       'difffil'
    'shipma'                     'shipmafil'
    'geodata'                    'geodatafil'
    'analytical'                 'analytical_solution'
    '<user defined variables>'   'usrdeffil'   };

%
% For each function to be called, list the appropriate pragma for the
% compiler. Without these lines the compiler does not know that these
% functions may be called.
%
%#function d3d_comfil
%#function d3d_trimfil
%#function d3d_trihfil
%#function d3d_tridfil
%#function d3d_tramfil
%#function d3d_trahfil
%#function d3d_botmfil
%#function d3d_bothfil
%#function d3d_hwgxyfil
%#function d3d_bagrfil
%#function d3d_waqfil
%#function d3d_simfil
%#function imodfil
%#function skyllafil
%#function arcgridfil
%#function asciiwindfil
%#function pcrasterfil
%#function waquafil
%#function mikezerofil
%#function flsfil
%#function gribfil
%#function gridfil
%#function tekalfil
%#function pharosfil
%#function samplesfil
%#function morftreefil
%#function aukepcfil
%#function bctfil
%#function cfxfil
%#function jspostfil
%#function bagdptfil
%#function bitmapfil
%#function telemacfil
%#function unibestfil
%#function matlabfil
%#function sobekfil
%#function netcdffil
%#function hdffil
%#function swanfil
%#function usrdeffil
%#function nfs_tritonfil
%#function bilhdrfil
%#function resourceobject
%#function ecomsedfil
%#function difffil
%#function shipmafil
%#function flexmeshfil
%#function geodatafil
%#function analytical_solution

tp=qp_gettype(Info);
%
% Compare file type with types available in the F table defined above. If
% the file type is not included in the table, show an appropriate error
% message and return with empty function name.
%
id = strcmpi(tp,F(:,1));
if ~any(id)
    previousMessage = 0;
    if isempty(MissingFileTypes)
        MissingFileTypes = {tp};
    else
        if any(strcmpi(tp,MissingFileTypes))
            previousMessage = 1;
        else
            MissingFileTypes{end+1}=tp;
        end
    end
    if ~previousMessage
        ui_message('warning','No function associated with a file of type "%s".',tp)
    end
    Fcn = [];
    return
end
FunName = F{id,2};
Fcn = str2func(FunName);
