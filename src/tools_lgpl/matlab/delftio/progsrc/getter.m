function getter
%GETTER  Example of receiving data via DelftIO.
%   Run this function in one MATLAB session while PUTTER runs in another on
%   the same machine.
%
%   See also DIO_DEFINE, DIO_READ, DIO_DELETE.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2025 Stichting Deltares.                                 
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

% initialize constants known to both putter and getter
NUM_DATA = 20;
NUM_TIME = 2;
MAT_SIZE = 100;

% open DelftIO connection
fprintf('\n--- Opening DelftIO stream. ---\n');
dsh = dio_define('MatlabPutter2Getter');
fprintf('Address of communication channel: %u\n', dsh);

% for various data types get data vector
for type = {'double', 'single', 'int32', 'uint8'}
    fprintf('\n--- Receiving data 1x%ivector as %s. ---\n', NUM_DATA, type{:});

    for t = 0:NUM_TIME-1
        fprintf('GETTER waits for data\n');

        if ~dio_startread(dsh), break; end
        data = double(dio_read(dsh, [1 NUM_DATA], type{:}));
        dio_endread(dsh);

        fprintf('GETTER has received data for t = %d\n', t, type{:});
        fprintf('getdata: %d is %f\n', [1:NUM_DATA;data]);
    end
end

% receive a single precision matrix
fprintf('\n--- Receiving single %ix%i matrix ---\n', MAT_SIZE, MAT_SIZE);
fprintf('GETTER waits for matrix.\n');
if dio_startread(dsh)
    matrix = dio_read(dsh, [MAT_SIZE MAT_SIZE], 'single=>double');
    dio_endread(dsh);
    fprintf('GETTER has received matrix.\n');
    fprintf('matrix = \n\n');
    fprintf('   %7.4f   %7.4f   %7.4f   %7.4f    ...\n', matrix(1:4, 1:4)');
    fprintf('    ...       ...       ...       ...       ...\n\n');
end

% clean up connection
fprintf('GETTER is closing DelftIO connection.\n');
dio_delete(dsh);
fprintf('GETTER is finished.\n');

