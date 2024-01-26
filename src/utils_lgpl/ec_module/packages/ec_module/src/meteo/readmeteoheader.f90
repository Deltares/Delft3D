function readmeteoheader(minp, meteoitem) result(success)
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
    use string_module
    implicit none
!
! Global variables
!
    type(tmeteoitem) :: meteoitem
    integer          :: minp
    logical          :: success

!
! Local variables
!
    integer                   :: ierr           ! Error indicator used for reading
    integer                   :: il             ! Position on the read line behind the =
    integer                   :: ir             ! Position on the read line before the # (comments)
    integer                   :: istat          ! error code of GetLine
    character(:), allocatable :: rec            ! string for reading a whole line
    character(len=10)         :: newest_version ! newest version of format of meteo input files
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialise parameters
    !
    meteoitem%fileversion      = ' '
    meteoitem%meteotype        = ' '
    meteoitem%grid_file        = ' '
    meteoitem%grid_unit        = ' '
    meteoitem%spw_rad_unit     = ' '
    meteoitem%time_unit        = ' '
    meteoitem%quantities       = ' '
    meteoitem%units            = ' '
    meteoitem%first_data_value = ' '
    meteoitem%data_row         = ' '
    meteoitem%filetype         = 0
    meteoitem%n_quantity       = 0
    meteoitem%n_cols           = 0
    meteoitem%n_rows           = 0
    meteoitem%pref_option      = opt_not_defined
    meteoitem%nodata_value     = 0.0_fp
    meteoitem%spw_radius       = 0.0_fp
    meteoitem%spw_merge_frac   = 0.5_fp
    meteoitem%dx               = 0.0_hp
    meteoitem%dy               = 0.0_hp
    meteoitem%x_llcorner       = -9.990e+20_hp
    meteoitem%y_llcorner       = -9.990e+20_hp
    meteoitem%x_llcenter       = -9.990e+20_hp
    meteoitem%y_llcenter       = -9.990e+20_hp
    meteoitem%pref             = -9.990e+20_hp
    !
    ! Define the newest version of the format of the input files.
    ! A check is performed on the correctness using keyword FileVersion
    !
    newest_version = '1.03'
    !
    do
       call GetLine(minp, rec, istat)
       if (istat /= 0) goto 100
       il = index(rec, '=') + 1
       ir = index(rec, '#') - 1
       if (ir == -1) then
          if (il == 1 .and. rec /= '') then
             !
             ! No '=' and no '#' found, but line not empty
             ! File has no header 
             ! Contains only a time series for uniform (in space) wind
             !
             meteoitem%n_quantity    = 3
             meteoitem%meteotype     = 'uniuvp'
             meteoitem%filetype      = uniuvp
             meteoitem%quantities(1) = 'x_wind'
             meteoitem%quantities(2) = 'y_wind'
             meteoitem%quantities(3) = 'air_pressure'
             meteoitem%units(1)      = 'm s-1'
             meteoitem%units(2)      = 'm s-1'
             meteoitem%units(3)      = 'Pa'
             success                 = .true.
             rewind(minp)
             return
          else
             ir = len(rec)
          endif
       endif
       if ( rec(1:ir) ==  ' ' ) then
          !
          ! Line contains only commentary
          !
          cycle
       endif
       !
       ! Make the keyword case insensitive
       !
       call small(rec,il)
       !
       if     ( index(rec(1:il-2), 'fileversion'     ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%fileversion
          meteoitem%fileversion      = adjustl(meteoitem%fileversion     )  
       elseif ( index(rec(1:il-2), 'filetype'        ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%meteotype
          meteoitem%meteotype        = adjustl(meteoitem%meteotype       )
       elseif ( index(rec(1:il-2), 'nodata_value'    ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%nodata_value
       elseif ( index(rec(1:il-2), 'n_cols'          ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%n_cols
       elseif ( index(rec(1:il-2), 'n_rows'          ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%n_rows
       elseif ( index(rec(1:il-2), 'grid_unit'       ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%grid_unit
          meteoitem%grid_unit        = adjustl(meteoitem%grid_unit       )
       elseif ( index(rec(1:il-2), 'x_llcorner'      ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%x_llcorner
       elseif ( index(rec(1:il-2), 'y_llcorner'      ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%y_llcorner
       elseif ( index(rec(1:il-2), 'x_llcenter'      ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%x_llcenter
       elseif ( index(rec(1:il-2), 'y_llcenter'      ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%y_llcenter
       elseif ( index(rec(1:il-2), 'value_pos'       ) /=0)  then
          meteomessage = 'Using old meteo input version, Keyword value_pos is outdated. Check the DELFT3D-FLOW manual for updates.'
          success = .false.
          return
       elseif ( index(rec(1:il-2), 'dx'              ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%dx
       elseif ( index(rec(1:il-2), 'dy'              ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%dy
       elseif ( index(rec(1:il-2), 'spw_radius'      ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%spw_radius
       elseif ( index(rec(1:il-2), 'spw_merge_frac'  ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%spw_merge_frac
       elseif ( index(rec(1:il-2), 'spw_rad_unit'    ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%spw_rad_unit
          meteoitem%spw_rad_unit     = adjustl(meteoitem%spw_rad_unit    )
       elseif ( index(rec(1:il-2), 'air_pressure_reference') /=0)  then
          if (index(rec(il:ir), 'air_pressure_default_from_computational_engine') /= 0) then
             meteoitem%pref_option = opt_use_from_kernel
             meteoitem%pref        = patm_default
          else
             meteoitem%pref_option = opt_value_specified
             read( rec(il:ir), *, iostat = ierr         )             meteoitem%pref
          endif
       elseif ( index(rec(1:il-2), 'n_quantity'      ) /=0)  then
          read( rec(il:ir), *, iostat = ierr         )             meteoitem%n_quantity
       elseif ( index(rec(1:il-2), 'quantity1'       ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%quantities(1)
          meteoitem%quantities(1)    = adjustl(meteoitem%quantities(1)   )
       elseif ( index(rec(1:il-2), 'quantity2'       ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%quantities(2)
          meteoitem%quantities(2)    = adjustl(meteoitem%quantities(2)   )
       elseif ( index(rec(1:il-2), 'quantity3'       ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%quantities(3)
          meteoitem%quantities(3)    = adjustl(meteoitem%quantities(3)   )
       elseif ( index(rec(1:il-2), 'unit1'           ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%units(1)
          meteoitem%units(1)         = adjustl(meteoitem%units(1)        )
       elseif ( index(rec(1:il-2), 'unit2'           ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%units(2)
          meteoitem%units(2)         = adjustl(meteoitem%units(2)        )
       elseif ( index(rec(1:il-2), 'unit3'           ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%units(3)
          meteoitem%units(3)         = adjustl(meteoitem%units(3)        )
       elseif ( index(rec(1:il-2), 'grid_file'       ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%grid_file
          meteoitem%grid_file        = adjustl(meteoitem%grid_file       )
       elseif ( index(rec(1:il-2), 'first_data_value') /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%first_data_value
          meteoitem%first_data_value = adjustl(meteoitem%first_data_value)
       elseif ( index(rec(1:il-2), 'data_row'        ) /=0)  then
          read( rec(il:ir),'(a)', iostat = ierr      )             meteoitem%data_row
          meteoitem%data_row         = adjustl(meteoitem%data_row        )
          if (meteoitem%data_row == 'grid_col') meteoitem%data_row = 'grid_column'
       elseif ( index(rec(1:il-2), 'time'            ) /=0)  then
          rewind(minp)
          exit
       else
          write(meteomessage,'(2a)') 'Meteo input: Found unexpected keyword: ', trim(rec(1:il-2))
          exit
       endif
    enddo
    !
    ! Check for keyword FileVersion and if the value is correct
    !
    if     (meteoitem%fileversion == newest_version) then
       !
       ! Correct usage of newest format for meteo input
       ! 
       if ( meteomessage == ' ' ) then
          success = .true.
       else
          !
          ! Found unexpected keyword during reading
          !
          success = .false.
          return
       endif
    elseif ( meteoitem%fileversion == ' ' ) then
       meteomessage = 'Using old meteo input version, keyword FileVersion not found in meteofile. File header should be keyword based. Check the DELFT3D-FLOW User Manual for changes.'
       success = .false.
       return
    else
       write (meteomessage, '(5a)') 'Meteo input version ', trim(meteoitem%fileversion), ' is not supported. Expecting version ', trim(newest_version), '. Check the DELFT3D-FLOW User Manual for changes.'
       success = .false.
       return
    endif
    !
    ! Meteo_on_computational_grid was called meteo_on_flow_grid in previous versions of the meteo module.
    ! For backwards compatibility, both entries are accepted.
    ! The following if-statement is moved from checkmeteoheader to here:
    ! meteoitem%meteotype is used before the call to checkmeteoheader
    !
    if (meteoitem%meteotype == 'meteo_on_flow_grid') then
       meteoitem%meteotype = 'meteo_on_computational_grid'
    endif
    !
    ! Error handling
    !
    if (ierr == 0 .and. success) then
       !
       ! All meteo wind input read
       !
    else
       return
    endif
    !
    return
   100 continue
    meteomessage = 'Unexpected end of file while reading header meteo file'
    success = .false.
end function readmeteoheader
