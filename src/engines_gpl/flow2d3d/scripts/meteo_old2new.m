function meteo_old2new(fileIn,fileOut,filetype,itdate,timezone,quantity,nodata,mmax,nmax)
%METEO_OLD2NEW convert old meteo files to new file format
%   METEO_OLD2NEW(fileIn,fileOut,filetype,itdate,timezone,quantity,nodata,mmax,nmax)
%   reads data from the input file fileIn and writes the same data supplemented
%   with the data from the other input arguments to the output file fileOut.
%   Detailed description of function arguments:
%
%   fileIn   = path and name of file to be converted
%   fileOut  = path and name of file to write to 
%   filetype = 'arcinfo','curvi','spiderweb' or 'svwp' (data on flow grid)
%   itdate   = itdate specified as yyyymmdd.hhmm (numerical)
%   timezone = string specifying timezone (e.g. '+00:00')
%
%   quantity =  'windu', 'windv', 'pressure', 'temperature',
%               'relative_humidity', 'evaporation', 'precipitation',
%               'sea_surface_temperature', 'cloudiness' or leave empty ('')
%               when 3 quantities (windu,windv and pressure) are to be
%               specified in 1 file (for svwp and spiderweb wind only)
%
%   nodata   = no data value (numerical). Only needed if filetype is spiderweb
%              or svwp, else leave empty ('')
%
%   mmax     = number of columns in dataset (m-direction), only needed when
%              filetype is svwp, else leave empty.
%              Can be read from the MDF file (keyword MNKmax).
%   nmax     = number of rows in dataset    (n-direction), only needed when
%              filetype is svwp, else leave empty.
%              Can be read from the MDF file (keyword MNKmax).
%
%   NOTE: the pressure correction on open boundaries as could previously be
%         specified in the pressure file, should now be specified in the MDF
%         file using keyword PavBnd, e.g. PavBnd = 100000.0. This must be done
%         in Pascals

%   $Id: meteo_old2new.m 13275 2010-10-22 11:58:03Z jagers $

fileversion = '1.03';
itdate      = itdate * 10000;

%gridunit = string specifying units of grid ('m' or 'degree'). Only needed 
%           if filetype is spiderweb or arcinfo, else leave empty (''))
if strcmp(filetype,'arcinfo') || strcmp(filetype,'spiderweb');
    gridunit = questdlg('What grid unit is used?', ...
                        'Grid unit','m','degree','m');
end
           
%timeunit = 'minutes' or 'hours', the unit in which the time is given
timeUnit = questdlg('What time unit is used?', ...
                    'Time unit','minutes','hours','minutes');

grid = struct();

% logicals for faster use in all loops further on.
forSVWP      = strcmp(filetype,'svwp');
forSpiderweb = strcmp(filetype,'spiderweb');
forCurvi     = strcmp(filetype,'curvi');
forArcinfo   = strcmp(filetype,'arcinfo');

switch filetype
    case 'svwp'
        filetypeN        = 'meteo_on_flow_grid';
    case 'arcinfo'
        filetypeN        = 'meteo_on_equidistant_grid';
    case 'curvi'
        filetypeN        = 'meteo_on_curvilinear_grid';
    case 'spiderweb'
        filetypeN        = 'meteo_on_spiderweb_grid';
end

switch filetype
    case 'spiderweb'
        n_quantity = 3;
        quantity1 = 'wind_speed';
        unit1 = 'm s-1';
        quantity2 = 'wind_from_direction';
        unit2 = 'degree';
        quantity3 = 'p_drop';
        unit3 = questdlg('What pressure unit is used?', ...
            'Pressure unit','Pa','mbar','Pa');
        warndlg({'Pressure correction at boundaries is no longer read from meteo-file.','ok','Correction should now be specified in MDF using keyword PavBnd'});
    case 'svwp'
        n_quantity = 3;
        quantity1 = 'x_wind';
        unit1 = 'm s-1';
        quantity2 = 'y_wind';
        unit2 = 'm s-1';
        quantity3 = 'air_pressure';
        unit3 = questdlg('What pressure unit is used?', ...
            'Pressure unit','Pa','mbar','Pa');
        warndlg({'Pressure correction at boundaries is no longer read from meteo-file.','ok','Correction should now be specified in MDF using keyword PavBnd'});
    otherwise
        n_quantity = 1;
        % Convert the quantity to the correct name and assign the unit
        switch quantity
            case {'pressure','air_pressure'}
                quantity1 = 'air_pressure';
                unit1 = questdlg('What pressure unit is used?', ...
                    'Pressure unit','Pa','mbar','Pa');
                warndlg({'Pressure correction at boundaries is no longer read from meteo-file.','ok','Correction should now be specified in MDF using keyword PavBnd'});
            case {'windu','x_wind'}
                quantity1 = 'x_wind';
                unit1     = 'm s-1';
            case {'windv','y_wind'}
                quantity1 = 'y_wind';
                unit1     = 'm s-1';
            case {'temperature','air_temperature'}
                quantity1 = 'air_temperature';
                unit1     = 'Celsius';
            case 'cloudiness'
                quantity1 = 'cloudiness';
                unit1     = '%';
            case 'relative_humidity'
                quantity1 = 'relative_humidity';
                unit1     = '%';
            case 'evaporation'
                quantity1 = 'evaporation';
                unit1     = 'mm';
            case 'precipitation'
                quantity1 = 'precipitation';
                unit1     = 'mm';
            case 'sea_surface_temperature'
                quantity1 = 'sea_surface_temperature';
                unit1     = 'Celsius';
        end
end

%% read data
%[path Name Ext]  = fileparts(fileIn);
fin = fopen(fileIn);
if fin == -1
    error(['Could not open file ''', fileIn, '''.']);
end

% filout          = strcat(path,filesep,Name,'_new',Ext);
filout          = fileOut;
fout = fopen(filout, 'wt'); % t handles carriage returns at newlines automatically
if fout == -1
    error(['Could not open file ''', filout, ''' for writing.']);
end


%% Read input file's header and write the new header to output file.

% First parse the input file header

% Obtain the full header block for arcinfo and curvi
if strcmp(filetype,'arcinfo') || strcmp(filetype,'curvi')
    header = {};
    curLinePos = ftell(fin);
    lineIn = fgetl(fin);
    while ischar(lineIn) && isempty(strmatch('/* TIME',lineIn));
        header{end+1} = lineIn;
        curLinePos = ftell(fin);
        lineIn = fgetl(fin);
    end
    fseek(fin, curLinePos, -1); % Rewind file pointer to beginning of current line.
end

switch filetype
case 'arcinfo'
    grid.n_cols       = sscanf(getLineEnd(header,'ncols'),'%f',1);
    grid.n_rows       = sscanf(getLineEnd(header,'nrows'),'%f',1);
    idx  = strfind(header, 'xllcentre');
    arr1 = cell2mat(idx);
    idx  = strfind(header, 'xllcorner');
    arr2 = cell2mat(idx);
    if ~isempty(arr1)
        grid.x_llcentre   = sscanf(getLineEnd(header,'xllcentre'),'%f',1);
        grid.y_llcentre   = sscanf(getLineEnd(header,'yllcentre'),'%f',1);
        grid.x_llcorner   = 0;
        grid.y_llcorner   = 0;
    elseif ~isempty(arr2)
        grid.x_llcentre   = 0;
        grid.y_llcentre   = 0;
        grid.x_llcorner   = sscanf(getLineEnd(header,'xllcorner'),'%f',1);
        grid.y_llcorner   = sscanf(getLineEnd(header,'yllcorner'),'%f',1);
    else
        error(['Could not find xllcorner (and yllcorner) or xllcentre (and yllcentre).']);
    end
    dx                = sscanf(getLineEnd(header,'cellsize'),'%f',2);
    grid.dx           = dx(1);
    grid.dy           = dx(2);
    grid.nodata_value = sscanf(getLineEnd(lower(header),'nodata_value'),'%f',1);
case 'curvi'
    grid.gridfile     = sscanf(getLineEnd(header,'/* gridfile'),'%*s%s',1); % First '%*s' gets rid of '='-character
    grid.firstrow     = sscanf(getLineEnd(header,'/* firstrow'),'%*s%s',1);
    grid.lastrow      = sscanf(getLineEnd(header,'/* lastrow'),'%*s%s',1);
    grid.firstcol     = sscanf(getLineEnd(header,'/* firstcol'),'%*s%s',1);
    grid.lastcol      = sscanf(getLineEnd(header,'/* lastcol'),'%*s%s',1);
    grid.nodata_value = sscanf(getLineEnd(lower(header),'nodata_value'),'%f',1);

    % determine the orientation of the data set on the grid

    if strcmp(grid.firstrow,'mmax');     data_row = 'grid_col'; rowstart = 'right';
    elseif strcmp(grid.firstrow,'nmax'); data_row = 'grid_row'; rowstart = 'top';
    elseif strcmp(grid.lastrow,'mmax');  data_row = 'grid_col'; rowstart = 'left';
    elseif strcmp(grid.lastrow,'nmax');  data_row = 'grid_row'; rowstart = 'bottom';
    end
    if strcmp(grid.firstcol,'mmax');     data_row = 'grid_row'; colstart = 'right';
    elseif strcmp(grid.firstcol,'nmax'); data_row = 'grid_col'; colstart = 'top';
    elseif strcmp(grid.lastcol,'mmax');  data_row = 'grid_row'; colstart = 'left';
    elseif strcmp(grid.lastcol,'nmax');  data_row = 'grid_col'; colstart = 'bottom';
    end
    if strcmp(rowstart,'bottom')
        if strcmp(colstart,'right');    first_data_value  = 'grid_lrcorner';
        elseif strcmp(colstart,'left'); first_data_value  = 'grid_llcorner';
        end
    elseif strcmp(rowstart,'top')
        if strcmp(colstart,'right');    first_data_value  = 'grid_urcorner';
        elseif strcmp(colstart,'left'); first_data_value  = 'grid_ulcorner';
        end
    elseif strcmp(rowstart,'left')
        if strcmp(colstart,'top');       first_data_value  = 'grid_ulcorner';
        elseif strcmp(colstart,'bottom');first_data_value  = 'grid_llcorner';
        end
    elseif strcmp(rowstart,'right')
        if strcmp(colstart,'top');       first_data_value  = 'grid_urcorner';
        elseif strcmp(colstart,'bottom');first_data_value  = 'grid_lrcorner';
        end
    end
case 'spiderweb'
    % Header lines all start with #
    itdateORradius = [];  

    curLinePos = ftell(fin);
    lineIn = fgetl(fin);
    while ischar(lineIn) && strcmp(lineIn(1), '#')
        % Read header information
        temp = sscanf(lineIn,'# %f %f',2);
        if length(temp)==1
            itdateORradius(end+1) = temp(1);
        elseif length(temp)==2
            grid.n_cols = temp(2);
            grid.n_rows = temp(1);
        end

        curLinePos = ftell(fin);
        lineIn = fgetl(fin);

        % itdate found in old file is not always correct. Better to let user specify it.
        %itdate            = spw.itdate;
        %warndlg({'ITDate found and read from old format file',' ','Please check if ITDate in new file is correct!'})
    end
    fseek(fin, curLinePos, -1); % Rewind file pointer to beginning of current line.

    spw.itdate        = max(itdateORradius);
    spw.radius        = min(itdateORradius);
    grid.nodata_value = nodata;
    spw.radunit       = 'm';

case 'svwp'
    % Just skip first line (second line is start of first time block)
    fgetl(fin);
    grid.nodata_value = nodata;
end % switch filetype

% Now write the standardized output file header 
    fprintf(fout,'%s\n','### START OF HEADER');
    fprintf(fout,'%s\n','### This file is created by Deltares');
    fprintf(fout,'%s\n','### Additional commments');
    fprintf(fout,'%s%-50s%s\n'    ,'FileVersion     =    ',fileversion,'# Version of meteo input file, to check if the newest file format is used');
    fprintf(fout,'%s%-50s%s\n'    ,'filetype        =    ',filetypeN,'# Type of meteo input file: meteo_on_flow_grid, meteo_on_equidistant_grid, meteo_on_curvilinear_grid or meteo_on_spiderweb_grid');
    fprintf(fout,'%s%-50.3f%s\n'  ,'NODATA_value    =    ',grid.nodata_value,'# Value used for undefined or missing data');
    if strcmp(filetype,'spiderweb') || strcmp(filetype,'arcinfo')
        fprintf(fout,'%s%-50i%s\n','n_cols          =    ',grid.n_cols,'# Number of columns used for wind datafield');
        fprintf(fout,'%s%-50i%s\n','n_rows          =    ',grid.n_rows,'# Number of rows used for wind datafield');
        fprintf(fout,'%s%-50s%s\n','grid_unit       =    ',gridunit,'# Unit used for grid dimensions');
    end
    if strcmp(filetype,'arcinfo')
        if grid.x_llcorner > 0 || grid.y_llcorner > 0
            fprintf(fout,'%s%-50.4f%s\n','x_llcorner      =    ',grid.x_llcorner,'# Xcoordinate of lower left corner of grid (in units specified in grid_unit)');
            fprintf(fout,'%s%-50.4f%s\n','y_llcorner      =    ',grid.y_llcorner,'# Ycoordinate of lower left corner of grid (in units specified in grid_unit)');
        else
            fprintf(fout,'%s%-50.4f%s\n','x_llcenter      =    ',grid.x_llcentre,'# Xcoordinate of lower left corner of grid (in units specified in grid_unit)');
            fprintf(fout,'%s%-50.4f%s\n','y_llcenter      =    ',grid.y_llcentre,'# Ycoordinate of lower left corner of grid (in units specified in grid_unit)');
        end
        fprintf(fout,'%s%-50.4f%s\n','dx              =    ',grid.dx,'# Grid dimension dx (in units specified in grid_unit)');
        fprintf(fout,'%s%-50.4f%s\n','dy              =    ',grid.dy,'# Grid dimension dy (in units specified in grid_unit)');
    end
    if strcmp(filetype,'spiderweb')
        fprintf(fout,'%s%-50.4f%s\n','spw_radius      =    ',spw.radius,'# Radius of spiderweb (in units specified in spw_rad_unit');
        fprintf(fout,'%s%-50s%s\n'  ,'spw_rad_unit    =    ',spw.radunit,'# Unit of radius (m or degree)');
    end
    if strcmp(filetype,'curvi')
        fprintf(fout,'%s%-50s%s\n'  ,'grid_file       =    ',grid.gridfile,'# Separate (curvi-linear) grid on which the wind can be specified');
        fprintf(fout,'%s%-50s%s\n'  ,'first_data_value=    ',first_data_value,'# Options: grid_llcorner, grid_ulcorner, grid_lrcorner, grid_urcorner');
        fprintf(fout,'%s%-50s%s\n'  ,'data_row        =    ',data_row,'# Options: grid_row or grid_col, for switching rows and colums');
    end
    fprintf(fout,'%s%-50i%s\n','n_quantity      =    ',n_quantity,'# Number of quantities prescribed in the file');
    if n_quantity == 1
        fprintf(fout,'%s%-50s%s\n','quantity1       =    ',quantity1,'# Name of quantity1');
        fprintf(fout,'%s%-50s%s\n','unit1           =    ',unit1,'# Unit of quantity1');
    elseif n_quantity == 3
        fprintf(fout,'%s%-50s%s\n','quantity1       =    ',quantity1,'# Name of quantity1');
        fprintf(fout,'%s%-50s%s\n','quantity2       =    ',quantity2,'# Name of quantity2');
        fprintf(fout,'%s%-50s%s\n','quantity3       =    ',quantity3,'# Name of quantity3');
        fprintf(fout,'%s%-50s%s\n','unit1           =    ',unit1,'# Unit of quantity1');
        fprintf(fout,'%s%-50s%s\n','unit2           =    ',unit2,'# Unit of quantity2');
        fprintf(fout,'%s%-50s%s\n','unit3           =    ',unit3,'# Unit of quantity3');
    end
    fprintf(fout,'%s\n','### END OF HEADER');

%--------------------------------------------------------------------------
% We may assume that fin still points to the first line, first character of
% the first time block and that fout points to the end of the output file.
%% Convert all time+data blocks
lineIn = fgetl(fin);
while ischar(lineIn)
    % Start scanning a new time+data block

    % --- For current time line: extract actual time (and process
    % subsequent additional lines (depending on format)
    if forSpiderweb
        TimeNum            = sscanf(lineIn,'%f',1);
        lineIn = fgetl(fin); % Also parse the cyclone eye's params
        temp = sscanf(lineIn,'%f');
        spw.x_spw_eye      = temp(1);
        spw.y_spw_eye      = temp(2);
        spw.p_drop_spw_eye = temp(3);
    elseif forCurvi || forArcinfo
        time = [];
        timeLine = lineIn;
        while isempty(time) && ~isempty(timeLine)
            [timestr,timeLine] = strtok(timeLine);
            time = str2num(timestr);
        end
        if ~isempty(time)
            TimeNum       = time;
        end
    elseif forSVWP
        TimeNum           = sscanf(lineIn,'%f',1);
    end

    % Print the new time line
    % date functions of old MATLAB versions are not as flexible as those of
    % more recent MATLAB versions, hence we do the manipulations here the
    % hard way...
    refdate = sprintf('%.0f',itdate);
    refdate = sprintf('%4d-%2.2d-%2.2d %2.2d:%2.2d:00 %s',sscanf(refdate,'%4d%2d%2d%2d%2d',5),timezone);
    fprintf(fout,'%s\n',['TIME            =    ' num2str(TimeNum,'%12.2f') ' ' timeUnit ' since ' refdate '  # Fixed format: time unit since date time time difference (time zone)']);

	if  forSpiderweb
        fprintf(fout,'%s%-50.4f%s\n','x_spw_eye       =    ',spw.x_spw_eye,'# Xcoordinate of cyclone eye (Spiderweb centre) at specified TIME');
        fprintf(fout,'%s%-50.4f%s\n','y_spw_eye       =    ',spw.y_spw_eye,'# Ycoordinate of cyclone eye (Spiderweb centre) at specified TIME');
        fprintf(fout,'%s%-50.4f%s\n','p_drop_spw_eye  =    ',spw.p_drop_spw_eye,'# Pressure drop at cyclone eye (Spiderweb centre) at specified TIME');
    end

    lineIn = fgetl(fin);

    % --- Now read all data lines for this time
    blockFinished = false;
    nr_of_entries = 0;

    while ~blockFinished
        % Simply copy the current line to output file.
        fprintf(fout, '%s\n', lineIn);

        tem = sscanf(lineIn,'%f');
        nr_of_entries = nr_of_entries + length(tem);
        
        % Check whether time-data block is now finished
        lineIn = fgetl(fin); % Check using next line
        if forCurvi || forArcinfo
            blockFinished = (~ischar(lineIn) || ~isempty(strmatch('/* TIME',lineIn)));
        elseif forSpiderweb
            blockFinished = (nr_of_entries >= grid.n_cols*grid.n_rows*3);
        elseif forSVWP
            blockFinished = (nr_of_entries >= mmax*nmax*3);
        end

    end % time-data block finished
        
end % while not EOF fin

%--------------------------------------------------------------------------
fclose(fin);
fclose(fout);


function strEnd = getLineEnd(strList,strStart)
%getLineEnd Search for matching line, and return end section of that line.
strEnd = strList{strmatch(strStart,strList)};
strEnd = strEnd(length(strStart)+1:end);
