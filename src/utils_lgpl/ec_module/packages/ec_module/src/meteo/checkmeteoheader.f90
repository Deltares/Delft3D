function checkmeteoheader(meteoitem) result(success)
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo_data
    use precision
    implicit none
!
! Return variable
!
    logical          :: success
!
! Global variables
!
    type(tmeteoitem) :: meteoitem
!
! Local variables
!
    real(fp)      :: pi
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialise conversion factor for pressure
    !
    meteoitem%p_conv    = 1.0_hp
    !
    ! Check the grid_unit only for meteo_on_equidistant_grid and meteo_on_spiderweb_grid
    ! For meteo_on_curvilinear_grid, the grid type is read from the curvilinear grid file
    ! Compliance with FLOW grid is checked in function addmeteoitem
    !
    if (meteoitem%meteotype == 'meteo_on_equidistant_grid' .or. meteoitem%meteotype == 'meteo_on_spiderweb_grid') then
       if (meteoitem%grid_unit == 'degree') then
          !
          ! Grid distances given in spherical coordinates (latitute-longitude), necessary when FLOW grid is spherical
          !
       elseif (meteoitem%grid_unit == 'm') then
          !
          ! Grid distances given in cartesian coordinates: nothing to be done
          !
       else
          write(meteomessage, '(2a)') 'Meteo input: Incorrect grid_unit given, expecting m or degree, but getting ', &
              & trim(meteoitem%grid_unit)
          success = .false.
          return
       endif
    endif
    !
    if (meteoitem%meteotype == 'meteo_on_curvilinear_grid' .and. meteoitem%grid_file == ' ') then
       !
       ! Wind on a separate curvilinear grid
       !
       meteomessage = 'Meteo input: No grid file found for wind on a separate curvilinear grid' 
       success = .false.
       return
    endif
    !
    if (meteoitem%meteotype == 'uniuvp' .or. meteoitem%meteotype == 'meteo_on_computational_grid') then
       !
       ! OPTION 1: Uniform in space, varying in time, wind input 
       !           Pressure is default or specified as a constant
       !
       !
       ! OPTION 2: Space varying wind and pressure on the FLOW grid
       !
       ! Check quantity names
       !
       if (meteoitem%n_quantity == 1) then 
          if (meteoitem%quantities(1) /= 'bedrock_surface_elevation') then
             write(meteomessage, '(2a)') 'Field input: Incorrect quantity given, expecting bedrock_surface_elevation, but getting ', &
                 & trim(meteoitem%quantities(1))
             success = .false.
             return
          endif
       elseif (meteoitem%n_quantity == 3) then 
          if (meteoitem%quantities(1) /= 'x_wind') then
             write(meteomessage, '(2a)') 'Meteo input: Incorrect quantity given, expecting x_wind, but getting ', &
                 & trim(meteoitem%quantities(1))
             success = .false.
             return
          endif
          if (meteoitem%quantities(2) /= 'y_wind' ) then
             write(meteomessage, '(2a)') 'Meteo input: Incorrect quantity given, expecting y_wind, but getting ', &
                 & trim(meteoitem%quantities(2))
             success = .false.
             return
          endif
          if (meteoitem%quantities(3) /= 'air_pressure' ) then
             write(meteomessage, '(2a)') 'Meteo input: Incorrect quantity given, expecting air_pressure, but getting ', &
                 & trim(meteoitem%quantities(3))
             success = .false.
             return
          endif
       else
          write(meteomessage, '(a)') 'Meteo input: Incorrect number of quantities, expecting 1 or 3.'
          success = .false.
          return
       endif 
       !
       ! Check units for bedrock_surface_elevation
       !
       if (meteoitem%quantities(1) == 'bedrock_surface_elevation' .and. meteoitem%units(1) /= 'm' ) then
          write(meteomessage, '(2a)') 'Field input: Incorrect unit given for bedrock_surface_elevation, expecting m, but getting ', &
              & trim(meteoitem%units(1))
          success = .false.
          return
       endif       
       !
       ! Check units for x_wind
       !
       if (meteoitem%quantities(1) == 'x_wind' .and. meteoitem%units(1) /= 'm s-1' ) then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for x_wind, expecting m s-1, but getting ', &
              & trim(meteoitem%units(1))
          success = .false.
          return
       endif
       !
       ! Check units for y_wind
       !
       if (meteoitem%n_quantity == 3) then 
          if (meteoitem%units(2) /= 'm s-1') then
              write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for y_wind, expecting degree, but getting ', &
                 & trim(meteoitem%units(2))
              success = .false.
              return
           endif
       endif    
       !
       ! Check units for air_pressure
       !
       if (meteoitem%n_quantity == 3) then 
          if     (meteoitem%units(3) == 'Pa'  ) then
             meteoitem%p_conv = 1.0_hp
          elseif (meteoitem%units(3) == 'mbar') then
             meteoitem%p_conv = 100.0_hp
          else
             write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for air_pressure, expecting Pa or mbar, but getting ', &
             & trim(meteoitem%units(3))
             success = .false.
             return
          endif
       endif   
    elseif (meteoitem%meteotype == 'meteo_on_equidistant_grid') then
       !
       ! Wind on a separate equidistant grid. If the FLOW grid is spherical, the meteo grid must be as well.
       ! In older meteo format this was known as Wind on a Pseudo ArcInfo grid
       !
       ! Check if starting point of grid (x0_arc,y0_arc) is given in cell centre or corner
       !
       if (meteoitem%x_llcorner > -9.980e+20_hp) then
          if (meteoitem%y_llcorner > -9.980e+20_hp) then
             if (meteoitem%x_llcenter > -9.980e+20_hp .or. meteoitem%y_llcenter > -9.980e+20_hp) then
                write(meteomessage, '(3a)') 'Meteo input: Overspecification of the grid location in file: ', trim(meteoitem%filename), '. Either use x_llcorner and y_llcorner or x_llcenter and y_llcenter.'
                success = .false.
                return
             else
                !
                ! Coordinate of lower left corner of lower left cell specified. Shift grid by half dx and dy.
                !
                meteoitem%x_llcenter = meteoitem%x_llcorner + meteoitem%dx/2.0_hp
                meteoitem%y_llcenter = meteoitem%y_llcorner + meteoitem%dy/2.0_hp
             endif
          else
             write(meteomessage, '(3a)') 'Meteo input: Incorrect specification of the grid location in file: ', trim(meteoitem%filename), '. Either use x_llcorner and y_llcorner or x_llcenter and y_llcenter.'
             success = .false.
             return
          endif
       elseif (meteoitem%x_llcenter > -9.980e+20_hp) then
          if (meteoitem%y_llcenter > -9.980e+20_hp) then
             if (meteoitem%x_llcorner > -9.980e+20_hp .or. meteoitem%y_llcorner > -9.980e+20_hp) then
                write(meteomessage, '(3a)') 'Meteo input: Overspecification of the grid location in file: ', trim(meteoitem%filename), '. Either use x_llcorner and y_llcorner or x_llcenter and y_llcenter.'
                success = .false.
                return
             else
                !
                ! Coordinate of centre of lower left cell specified. Do nothing
                !
             endif
          else
             write(meteomessage, '(3a)') 'Meteo input: Incorrect specification of the grid location in file: ', trim(meteoitem%filename), '. Either use x_llcorner and y_llcorner or x_llcenter and y_llcenter.'
             success = .false.
             return
          endif
       else
          write(meteomessage, '(3a)') 'Meteo input: Incomplete specification of the grid location in file: ', trim(meteoitem%filename), '. Either use x_llcorner and y_llcorner or x_llcenter and y_llcenter.'
          success = .false.
          return
       endif
       !
       ! Check quantity names
       !
       if ( meteoitem%quantities(1) == 'x_wind'            .or. &
          & meteoitem%quantities(1) == 'y_wind'            .or. &
          & meteoitem%quantities(1) == 'air_pressure'      .or. &
          & meteoitem%quantities(1) == 'relative_humidity' .or. &
          & meteoitem%quantities(1) == 'air_temperature'   .or. &
          & meteoitem%quantities(1) == 'precipitation'     .or. &
          & meteoitem%quantities(1) == 'sw_radiation_flux' .or. &
          & meteoitem%quantities(1) == 'cloudiness'        .or. &
          & meteoitem%quantities(1) == 'Secchi_depth'      .or. &
          & meteoitem%quantities(1) == 'sea_ice_area_fraction' .or. &
          & meteoitem%quantities(1) == 'sea_ice_thickness' .or. &
          & meteoitem%quantities(1) == 'floe_diameter'               )   then 
          !
          ! Correct quantity specified
          !
       else   
          write(meteomessage, '(4a)') 'Meteo input: unsupported quantity "', &
              & trim(meteoitem%quantities(1)), '"while reading: ', &
              & trim(meteoitem%filename)
          success = .false.
          return
       endif
       !
       ! Check units of wind velocities and air pressure
       !
       if ((trim(meteoitem%quantities(1)) == 'x_wind' .or. trim(meteoitem%quantities(1)) == 'y_wind') .and. .not. &
          & trim(meteoitem%units(1)) == 'm s-1') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for wind velocity, expecting m s-1, but getting ', &
              & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'air_pressure') then
          !
          ! Check units of air_pressure
          !
          if     (meteoitem%units(1) == 'Pa'  ) then
             meteoitem%p_conv = 1.0_hp
          elseif (meteoitem%units(1) == 'mbar') then
             meteoitem%p_conv = 100.0_hp
          else
             write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for air_pressure, expecting Pa or mbar, but getting ', &
                 & trim(meteoitem%units(1))
             success = .false.
             return
          endif
       elseif (meteoitem%quantities(1) == 'relative_humidity' .and. meteoitem%units(1) /= '%') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for relative_humidity, expecting %, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'air_temperature' .and. meteoitem%units(1) /= 'Celsius') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for air_temperature, expecting Celsius, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'cloudiness' .and. meteoitem%units(1) /= '%') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for cloudiness, expecting %, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'precipitation' .and. meteoitem%units(1) /= 'mm/h') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for precipitation, expecting mm/h, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'Secchi_depth' .and. meteoitem%units(1) /= 'm') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for Secchi depth, expecting m, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'sw_radiation_flux' .and. meteoitem%units(1) /= 'W/m2') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for precipitation, expecting W/m2, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       elseif (meteoitem%quantities(1) == 'sea_ice_area_fraction' .and. meteoitem%units(1) /= '-') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for sea ice area fraction, expecting -, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       elseif (meteoitem%quantities(1) == 'sea_ice_thickness' .and. meteoitem%units(1) /= 'm') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for sea ice thickness, expecting m, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       elseif (meteoitem%quantities(1) == 'floe_diameter' .and. meteoitem%units(1) /= 'm') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for floe diameter, expecting m, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       endif
    elseif (meteoitem%meteotype == 'meteo_on_spiderweb_grid') then
       !
       ! Wind and pressure on a Spiderweb grid
       !
       ! Note: one extra column for 360 deg = 0 deg was added in function addmeteoitem
       !
       pi           = acos(-1.0_fp)
       meteoitem%dx = 2.0_fp * pi / real(meteoitem%n_cols-1, fp)          ! n_cols-1 intervals in angular direction
       meteoitem%dy = meteoitem%spw_radius / real(meteoitem%n_rows-1, fp) ! n_rows-1 intervals in radial direction
       !
       ! Check merge frac
       !
       if (comparereal(meteoitem%spw_merge_frac,0.0_hp) == -1 &
         & .or. comparereal(meteoitem%spw_merge_frac,1.0_hp) == 1) then
          write(meteomessage, '(a,e10.3)') 'Meteo input: spw_merge_frac must be between 0.0 and 1.0, but getting ', meteoitem%spw_merge_frac
          success = .false.
          return
       endif
       !
       ! Check quantity names
       !
       if (meteoitem%quantities(1) /= 'wind_speed' ) then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect quantity given, expecting wind_speed, but getting ', &
              & trim(meteoitem%quantities(1))
          success = .false.
          return
       endif
       if (meteoitem%quantities(2) /= 'wind_from_direction') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect quantity given, expecting wind_from_direction, but getting ', &
              & trim(meteoitem%quantities(2))
          success = .false.
          return
       endif
       if (meteoitem%quantities(3) /= 'p_drop' ) then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect quantity given, expecting p_drop, but getting ', &
              & trim(meteoitem%quantities(3))
          success = .false.
          return
       endif
       !
       ! Check unit of spiderweb radius
       !
       if (meteoitem%spw_rad_unit /= 'm' ) then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for spiderweb radius, expecting m, but getting ', &
              & trim(meteoitem%units(1))
          success = .false.
          return
       endif
       !
       ! Check units for wind_speed
       !
       if (meteoitem%units(1) /= 'm s-1' ) then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for wind_speed, expecting m s-1, but getting ', &
              & trim(meteoitem%units(1))
          success = .false.
          return
       endif
       !
       ! Check units for wind_from_direction
       !
       if (meteoitem%units(2) /= 'degree') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for wind_from_direction, expecting degree, but getting ', &
              & trim(meteoitem%units(2))
          success = .false.
          return
       endif
       !
       ! Check units for p_drop
       !
       if     (meteoitem%units(3) == 'Pa'  ) then
          meteoitem%p_conv = 1.0_hp
       elseif (meteoitem%units(3) == 'mbar') then
          meteoitem%p_conv = 100.0_hp
       else
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for p_drop, expecting Pa or mbar, but getting ', &
              & trim(meteoitem%units(3))
          success = .false.
          return
       endif
    elseif (meteoitem%meteotype == 'meteo_on_curvilinear_grid') then
       !
       ! Wind and pressure on a separate curvilinear grid
       !
       ! Check quantity names
       !
       if ( meteoitem%quantities(1) == 'x_wind'            .or. &
          & meteoitem%quantities(1) == 'y_wind'            .or. &
          & meteoitem%quantities(1) == 'air_pressure'      .or. &
          & meteoitem%quantities(1) == 'relative_humidity' .or. &
          & meteoitem%quantities(1) == 'air_temperature'   .or. &
          & meteoitem%quantities(1) == 'precipitation'     .or. &
          & meteoitem%quantities(1) == 'sw_radiation_flux' .or. &
          & meteoitem%quantities(1) == 'cloudiness'        .or. &
          & meteoitem%quantities(1) == 'sea_ice_area_fraction' .or. &
          & meteoitem%quantities(1) == 'sea_ice_thickness' .or. &
          & meteoitem%quantities(1) == 'floe_diameter'               )   then 
          !
          ! Correct quantity specified
          !
       else   
          write(meteomessage, '(4a)') 'Meteo input: unsupported quantity "', &
              & trim(meteoitem%quantities(1)), '"while reading: ', &
              & trim(meteoitem%filename)
          success = .false.
          return
       endif
       !
       ! Check units of velocities, pressure, relative humidity, air temperature and cloudiness
       !
       if ((meteoitem%quantities(1) == 'x_wind' .or. meteoitem%quantities(1) == 'y_wind') .and. &
          & meteoitem%units(1)      /= 'm s-1') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for velocity, expecting m s-1, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'air_pressure') then
          if     (meteoitem%units(1) == 'Pa'  ) then
             meteoitem%p_conv = 1.0_hp
          elseif (meteoitem%units(1) == 'mbar') then
             meteoitem%p_conv = 100.0_hp
          else
             write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for air_pressure, expecting Pa or mbar, but getting ', &
                 & trim(meteoitem%units(1))
             success = .false.
             return
          endif
       elseif (meteoitem%quantities(1) == 'relative_humidity' .and. meteoitem%units(1) /= '%') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for relative_humidity, expecting %, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'air_temperature' .and. meteoitem%units(1) /= 'Celsius') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for air_temperature, expecting Celsius, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'cloudiness' .and. meteoitem%units(1) /= '%') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for cloudiness, expecting %, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'precipipation' .and. meteoitem%units(1) /= 'mm/h') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for precipitation, expecting mm/h, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
          return
       elseif (meteoitem%quantities(1) == 'sw_radiation_flux' .and. meteoitem%units(1) /= 'W/m2') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for precipitation, expecting W/m2, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       elseif (meteoitem%quantities(1) == 'sea_ice_area_fraction' .and. meteoitem%units(1) /= '-') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for sea ice area fraction, expecting -, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       elseif (meteoitem%quantities(1) == 'sea_ice_thickness' .and. meteoitem%units(1) /= 'm') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for sea ice thickness, expecting m, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       elseif (meteoitem%quantities(1) == 'floe_diameter' .and. meteoitem%units(1) /= 'm') then
          write(meteomessage, '(2a)') 'Meteo input: Incorrect unit given for floe diameter, expecting m, but getting ', &
                 & trim(meteoitem%units(1))
          success = .false.
       endif
       !
       ! For wind on a separate curvilinear grid the order of input may differ from the numbering
       ! of the grid. In this case the order of reading has to be altered using keywords first_data_value and data_row.
       !
       meteoitem%mfirst = 0
       meteoitem%nfirst = 0
       meteoitem%mlast  = 0
       meteoitem%nlast  = 0
       meteoitem%mrow   = .false.
       !
       if (meteoitem%data_row == 'grid_row') then
          meteoitem%mrow   = .false.
       elseif (meteoitem%data_row == 'grid_column') then
          meteoitem%mrow   = .true.
       else
          write(meteomessage, '(2a)') 'Meteo input: Inconsistent input given for data_row. Expecting grid_row or grid_column, but getting ', &
              & trim(meteoitem%data_row)
          success = .false.
          return
       endif
       if     (meteoitem%first_data_value == 'grid_llcorner') then
          meteoitem%mfirst = 1
          meteoitem%nfirst = 1
          meteoitem%mlast  = meteoitem%grid%num_columns
          meteoitem%nlast  = meteoitem%grid%num_rows
       elseif (meteoitem%first_data_value == 'grid_ulcorner') then
          meteoitem%mfirst = 1
          meteoitem%nfirst = meteoitem%grid%num_rows
          meteoitem%mlast  = meteoitem%grid%num_columns
          meteoitem%nlast  = 1
       elseif (meteoitem%first_data_value == 'grid_lrcorner') then
          meteoitem%mfirst = meteoitem%grid%num_columns
          meteoitem%nfirst = 1
          meteoitem%mlast  = 1
          meteoitem%nlast  = meteoitem%grid%num_rows
       elseif (meteoitem%first_data_value == 'grid_urcorner') then
          meteoitem%mfirst = meteoitem%grid%num_columns
          meteoitem%nfirst = meteoitem%grid%num_rows
          meteoitem%mlast  = 1
          meteoitem%nlast  = 1
       else
          write(meteomessage, '(2a)') 'Meteo input: Inconsistent input given for first_data_value. Expecting grid_ulcorner, grid_llcorner, grid_urcorner or grid_lrcorner, but getting ', &
              & trim(meteoitem%data_row)
          success = .false.
          return
       endif
       !
       if (meteoitem%mfirst==0) then
          !
          ! This should not be possible, but was in previous meteo version:
          ! Assume 'first_data_value' == 'grid_llcorner' and 'data_row' == 'grid_row'
          !
          meteoitem%mfirst = 1
          meteoitem%nfirst = 1
          meteoitem%mlast  = meteoitem%grid%num_columns
          meteoitem%nlast  = meteoitem%grid%num_rows
          meteoitem%mrow   = .false.
       endif
       !
    else if (meteoitem%meteotype == 'field_on_computational_grid') then
       if ( meteoitem%quantities(1) == 'uplift_rate_reference_elevation' ) then
          !
          ! Correct quantity specified
          !
       else   
          write(meteomessage, '(2a)') 'Meteo input: Incorrect quantity given, expecting uplift_rate_reference_elevation, but getting ', &
              & trim(meteoitem%quantities(1))
          success = .false.
          return    
       end if   
    else
       write(meteomessage, '(2a)') 'Meteo input: Inconsistent filetype, expecting uniuvp, meteo_on_computational_grid, field_on_computational_grid, meteo_on_equidistant_grid, meteo_on_curvilinear_grid or meteo_on_spiderweb_grid, but getting ', &
           & trim(meteoitem%meteotype)
       success = .false.
       return
    endif
    !
    ! Convert the read quantities to windu, windv, patm, relhum, cloud and airtemp
    !
    if (       meteoitem%quantities(1) == 'x_wind'              &
        &.or.  meteoitem%quantities(1) == 'wind_speed'           ) then
       meteoitem%quantities(1) = 'windu'
    elseif (   meteoitem%quantities(1) == 'y_wind'               ) then
       meteoitem%quantities(1) = 'windv'
    elseif (   meteoitem%quantities(1) == 'air_pressure'         ) then
       meteoitem%quantities(1) = 'patm'
    elseif (   meteoitem%quantities(1) == 'relative_humidity'    ) then
       meteoitem%quantities(1) = 'relhum'
    elseif (   meteoitem%quantities(1) == 'cloudiness'           ) then
       meteoitem%quantities(1) = 'cloud'
    elseif (   meteoitem%quantities(1) == 'air_temperature'      ) then
       meteoitem%quantities(1) = 'airtemp'
    elseif (   meteoitem%quantities(1) == 'precipitaion'      ) then
       meteoitem%quantities(1) = 'precip'
    elseif (   meteoitem%quantities(1) == 'secchi_depth'      ) then
       meteoitem%quantities(1) = 'Secchi_depth'
    elseif (   meteoitem%quantities(1) == 'sw_radiation_flux'      ) then
       meteoitem%quantities(1) = 'swrf'
    elseif (   meteoitem%quantities(1) == 'bedrock_surface_elevation'    ) then
       meteoitem%quantities(1) = 'sdu'
    endif
    !
    if (       meteoitem%quantities(2) == 'wind_from_direction' &
        & .or. meteoitem%quantities(2) == 'y_wind'               ) then
       meteoitem%quantities(2) = 'windv'
    endif
    !
    if (       meteoitem%quantities(3) == 'p_drop'              &
        & .or. meteoitem%quantities(3) == 'air_pressure'         ) then
       meteoitem%quantities(3) = 'patm'
    endif
    !
    ! All meteo input read correctly and checked for consistency
    !
    success = .true.
    !
end function checkmeteoheader
