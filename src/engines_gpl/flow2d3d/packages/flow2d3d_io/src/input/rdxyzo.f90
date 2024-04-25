subroutine rdxyzo(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & kmax      ,zbot      ,ztop      ,dx        ,dy        , &
                & filrgf    ,fmtrgf    ,thick     ,anglat    ,anglon    , &
                & grdang    ,sphere    ,sferic    ,zmodel    ,mmax      , &
                & nmax      ,xcor      ,ycor      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!
!    Function: - Initialises local en global parameters for the
!                dimensions, ie. THREED,PROCESSES,DX,DY & THICK
!              - Reads records from the MD-file related to the
!                grid dimensions : MNKMAX, FILCCO, FMTCCO,
!                DXDY, THICK
!              - Reads SPHERE
!              - Reads ANGLAT, ANGLON & GRDANG
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    use dfparall
    use system_utils, only: exifil
    use m_rdrgf_paral
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'pardef.igd'
!
! Global variables
!
    integer                  , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                :: lundia !  Description and declaration in inout.igs
    integer                                :: lunmd  !  Description and declaration in inout.igs
    integer                  , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                  , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                :: nrrec  !!  Pointer to the record number in the
                                                     !!  MD-file
    logical                  , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    logical                  , intent(out) :: sferic !  Description and declaration in tricom.igs
    logical                  , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)                               :: anglat !!  - Angle of latitude of the model
                                                     !!    centre (used to determine the coef.
                                                     !!    for the coriolis force)
                                                     !!  - In spherical coordinates this para-
                                                     !!    meter equals the angle of latitude
                                                     !!    for the origin (water level point)
    real(fp)                               :: anglon !!  - Angle of longitude of the model
                                                     !!    centre (used to determine solar
                                                     !!    radiation)
    real(fp)                 , intent(out) :: dx     !!  Uniform grid-distance in the x-dir.
                                                     !!  in meters & in decimals
                                                     !!  if sferic = T dx := dphi
                                                     !!  in degrees & in decimals
    real(fp)                 , intent(out) :: dy     !!  Uniform grid-distance in the y-dir.
                                                     !!  in meters & in decimals
                                                     !!  if sferic = T dy := dtheta
                                                     !!  in degrees & in decimals
    real(fp)                 , intent(out) :: grdang !  Description and declaration in tricom.igs
    real(fp)                               :: zbot   !  Description and declaration in zmodel.igs
    real(fp)                               :: ztop   !  Description and declaration in zmodel.igs
    real(fp), dimension(kmax), intent(out) :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) :: xcor !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(1 - gdp%d%ddbound:nmax + gdp%d%ddbound,1 - gdp%d%ddbound:mmax + gdp%d%ddbound) :: ycor !  Description and declaration in esm_alloc_real.f90
    character(*)                           :: filrgf !!  File name for the curvi-linear grid !!  file (telmcrgf.xxx)
                                                     !!  file will be read formatted
    character(*)                           :: mdfrec !!  Standard rec. length in MD-file (300)
    character(1)                           :: sphere !!  Flag Yes / No spherical coordinates
    character(2)                           :: fmtrgf !!  File format for the curvi-linear grid. file will be read formatted
!
! Local variables
!
    integer                          :: k        ! Help var.
    integer                          :: lenc     ! Help var. (length of var. chulp to be looked for in the MD-file)
    integer                          :: lfile    ! Help var. specifying the length of character variables for file names
    integer                          :: m        ! Help var.
    integer                          :: n        ! Help var.
    integer                          :: nlook    ! Help var.: nr. of data to look for in the MD-file
    integer                          :: ntrec    ! Help. var to keep track of NRREC
    logical                          :: defaul   ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook)
    logical                          :: lerror   ! Flag=TRUE if a local error is encountered
    logical                          :: newkw    ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line
    logical                          :: nodef    ! Flag set to YES if default value may NOT be applied in case var. read is empty (ier <= 0, or nrread < nlook)
    real(fp)                         :: rdef     ! Help var. containing default value(s) for real variable
    real(fp)                         :: rmissval
    real(fp)     , dimension(kmax)   :: rval     ! Help array (real) where the data, recently read from the MD-file, are stored temporarily
    character(1)                     :: cdef     ! Default value for chulp
    character(11)                    :: fmtdef   ! Default file format (usually=blank)
    character(11)                    :: fmttmp   ! Character string defining the format of the curvi-linear grid file, file will be read formatted
    character(12)                    :: fildef   ! Default file name (usually = blank)
    character(6)                     :: keyw     ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM)
    character(6)                     :: cval     ! String value
!
!! executable statements -------------------------------------------------------
!
    lerror   = .false.
    newkw    = .true.
    defaul   = .true.
    nodef    = .not.defaul
    fildef   = ' '
    fmtdef   = 'FRformatted'
    fmttmp   = ' '
    rdef     = 0.0
    cdef     = 'N'
    nlook    = 1
    rmissval = -999.0
    !
    ! initialize parameters that are to be read
    !
    sferic = .false.
    dx     = 0.0
    dy     = 0.0
    grdang = 0.0
    anglat = 0.0
    anglon = 0.0
    sphere = 'N'
    fmtrgf = 'FR'
    do k = 1, kmax
       thick(k) = 0.
    enddo
    !
    filrgf = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'Filcco', filrgf)
    if (filrgf /= fildef) then
       !
       ! Grid specified in a file
       !
       ! format definition:
       !
       keyw  = 'Fmtcco'
       ntrec = nrrec
       lenc  = 2
       call read2c(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,fmttmp    ,fmtdef    ,lenc      ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          lerror = .false.
          fmttmp = fmtdef(3:)
       else
          !
          ! determine file format (unformatted / freeformatted)
          !
          call filfmt(lundia    ,keyw      ,fmttmp    ,lerror    ,gdp       )
          if (lerror) then
             error = .true.
             lerror = .false.
             fmttmp = fmtdef(3:)
          endif
       endif
       !
       fmtrgf = 'FR'
       if (fmttmp(:2)=='un') fmtrgf = 'UN'
       !
       ! test file existence
       !
       lfile = len(filrgf)
       error = .not.exifil(filrgf, lundia)
       !
       if (fmtrgf/='FR') then
          write(lundia, '(a)') '*** error inconsistent format for grid file'
          error = .true.
       endif
       !
       ! Read sferic, xcor and ycor
       !
       call rdrgf_paral(filrgf    ,lundia    ,error     ,nmax      ,mmax      , &
                      & xcor      ,ycor      ,sferic    ,gdp       )
    else
       !
       ! No grid file
       !
       ! locate and read 'DxDy' record for DX and DY
       ! default value not allowed => nodef
       !
       rval = rmissval
       call prop_get(gdp%mdfile_ptr, '*', 'DxDy', rval, 2)
       if (comparereal(rval(1),rmissval) == 0 .or. &
         & comparereal(rval(2),rmissval) == 0       ) then
          error = .true.
          call prterr(lundia, 'P004', 'No grid file defined')
       else
          cdef = ' '
          call prop_get_string(gdp%mdfile_ptr,'*','Sphere',cdef)
          if (cdef /= 'y' .and. cdef /= 'Y') then
             if (parll) then
                call prterr(lundia, 'P004', 'The combination of constant dx and dy and parallel is not available')
                write (lundia,*) '           Use a grid file instead'
                error = .true.
             else
                call prterr(lundia, 'Z013', 'The use of constant dx and dy is deprecated')
                write (lundia,*) '           Use a grid file instead'
                dx = rval(1)
                dy = rval(2)
             endif
          else
             call prterr(lundia, 'P004', 'The use of constant dx and dy for ' // &
                       & 'spherical models is not supported anymore')
             write (lundia,*) '           Use a grid file instead'
             error = .true.
          endif
       endif
       !
       ! set xcor and ycor for Rectilinear model
       !
       do m = 1, mmax
          do n = 1, nmax
             xcor(n, m) = (m-0.5) * dx
             ycor(n, m) = (n-0.5) * dy
          enddo
       enddo
    endif
    !
    if (error) return
    !
    ! Spherical coordinate definition was read here
    ! Replaced by reading it from grid file
    !
    ! Z definition
    !
    cval = ' '
    call prop_get_string(gdp%mdfile_ptr, '*', 'laydis', cval)
    if (cval == ' ') then
       !
       ! locate and read 'Thick' record
       ! default value not allowed => nodef
       !
       keyw  = 'Thick'
       ntrec = nrrec
       nlook = kmax
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,nodef     ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          error = .true.
          lerror = .false.
       else
          do k = 1, kmax
             thick(k) = rval(k)
          enddo
       endif
    else
       call rdlaydis(lerror, thick, kmax, cval, gdp)
       if (lerror) call d3stop(1, gdp)
    endif
    !
    ! Check whether the ratio in layer thickness of the two bottom layers is not too large
    !
    if (kmax>1) then
       if (thick(2)/thick(1) > 1.5_fp) then
          call prterr(lundia, 'U190', 'Ratio in layer thickness of the two bottom layers is larger than 1.5.')
          write (lundia, *)'           May lead to inaccurate representation of the bottom boundary layer.'
       endif
    endif
    !
    if (zmodel) then
       !
       ! locate and read 'Zbot' record
       ! default value NOT allowed
       !
       keyw  = 'Zbot'
       ntrec = nrrec
       nlook = 1
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          error = .true.
       else
          zbot = rval(1)
       endif
       !
       ! locate and read 'ztop' record
       ! default value NOT allowed
       !
       keyw  = 'Ztop'
       ntrec = nrrec
       nlook = 1
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       ! reading error?
       !
       if (lerror) then
          error = .true.
       else
          ztop = rval(1)
          if (ztop<=zbot) then
             error = .true.
             call prterr(lundia, 'Z003', ' ')
          endif
       endif
    endif
    !
    ! Orientation definition
    !
    ! 'Anglat'
    !
    anglat = rdef
    call prop_get(gdp%mdfile_ptr, '*', 'Anglat', anglat)
    !
    ! 'Anglon'
    !
    anglon = rdef
    call prop_get(gdp%mdfile_ptr, '*', 'Anglon', anglon)
    !
    ! The check on anglat==0.0 is moved to subroutine iniphy
    !
    ! 'Grdang'
    !
    grdang = rdef
    call prop_get(gdp%mdfile_ptr, '*', 'Grdang', grdang)
end subroutine rdxyzo
