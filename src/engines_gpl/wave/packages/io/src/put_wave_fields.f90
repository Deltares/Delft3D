subroutine put_wave_fields (fg, fof, itide, wavedata, swflux, flowgridfile, netcdf_sp)
!
! Head routine for calling crewav
!
use wave_data
use swan_flow_grid_maps
!
implicit none
!
integer                    :: itide        ! time step number of wave group on com-file
logical                    :: swflux       ! switch to indicate if mass flux is taken into account
logical                    :: netcdf_sp    ! true: write netcdf files in single precision
character(*)               :: flowgridfile ! ' ' when using nefis com-file; '<name>.nc' when using d-flowfm NetCDF file
type (grid)                :: fg           ! flow grid
type (output_fields)       :: fof          ! wave output fields defined on flow grid
type (wave_data_type)      :: wavedata
   !
   if (flowgridfile == ' ') then
      call crewav(fg%grid_name ,itide    ,fof%hrms  ,fof%tp      ,fof%dir     , &
                & fof%dissip   ,fof%fx   ,fof%fy    ,fof%mx      ,fof%my      , &
                & fof%tps      ,fof%ubot ,fof%wlen  ,fof%wsbodyu ,fof%wsbodyv , &
                & fof%mmax     ,fof%nmax ,swflux    ,wavedata%time)
   else
      call crewav_netcdf(fg           ,itide    ,fof%hrms  ,fof%tp      ,fof%dir     , &
                       & fof%dissip   ,fof%fx   ,fof%fy    ,fof%mx      ,fof%my      , &
                       & fof%tps      ,fof%ubot ,fof%wlen  ,fof%wsbodyu ,fof%wsbodyv , &
                       & fof%mmax     ,fof%nmax ,swflux    ,wavedata    , netcdf_sp)
   endif
end subroutine put_wave_fields

subroutine putgtl(filnam    ,grpnam    ,nelems    ,elmnms    ,elmdms    , &
                & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                & elmnam    ,celidt    ,wrilog    ,error     ,buffr     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! Read or writes character buffer to nefis files
! Tests values input consistency for elmnam and
! elmnms and for local and global dimensions
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    integer, parameter :: start = 1
    integer, parameter :: stopp = 2
    integer, parameter :: incr  = 3
!
! Global variables
!
    integer               , intent(in)  :: celidt
    integer                             :: error
    integer                             :: nelems
    integer, dimension(*)               :: nbytsg
    integer, dimension(6, *)            :: elmdms
    logical, intent(in)                 :: wrilog
    logical, dimension(*)               :: buffr
    character(*)                        :: elmnam
    character(*)          , intent(in)  :: filnam
    character(*)                        :: grpnam
    character(*), dimension(*)          :: elmdes
    character(*), dimension(*)          :: elmnms
    character(*), dimension(*)          :: elmqty
    character(*), dimension(*)          :: elmtps
    character(*), dimension(*)          :: elmunt
!
! Local variables
!
    integer                        :: buflen
    integer                        :: elmndm
    integer                        :: fd_nef
    integer                        :: i
    integer                        :: ierror
    integer                        :: j
    integer                        :: lelmnr
    integer                        :: n
    integer, dimension(5)          :: elmdim
    integer, dimension(5)          :: uindex
    integer, external              :: clsnef
    integer, external              :: credat
    integer, external              :: crenef
    integer, external              :: defcel
    integer, external              :: defelm
    integer, external              :: defgrp
    integer, external              :: getelt
    integer, external              :: inqelm
    integer, external              :: neferr
    integer, external              :: putelt
    character(1)                   :: coding
    character(16)                  :: elmant
    character(16)                  :: elmqta
    character(2)                   :: access
    character(256)                 :: datnam
    character(256)                 :: defnam
    character(256)                 :: errmsg
    character(64)                  :: elmdas
    character(8)                   :: elmtap
!
!! executable statements -------------------------------------------------------
!
    !-----Initialization
    !
    coding = 'N'
    uindex(start) = celidt
    uindex(stopp) = celidt
    uindex(incr) = 1
    fd_nef = -1
    elmndm = 5
    !
    !-----aggregate file names
    !
    datnam = trim(adjustl(filnam))//'.dat'
    defnam = trim(adjustl(filnam))//'.def'
    !
    !-----write or read data from nefis files
    !
    if (wrilog) then
       access = 'u'
    else
       access = 'r'
    endif
    !
    error = crenef(fd_nef, trim(adjustl(datnam)), trim(adjustl(defnam)), coding, access)
    if (error/=0 .and. .not.wrilog) then
       error = -211
       goto 10000
    endif
    if (error/=0) goto 9999
    !
    if (wrilog) then
       error = putelt(fd_nef, grpnam, elmnam, uindex, 1, buffr)
    else
       j = 0
  123  continue
       j = j + 1
       if (elmnam==elmnms(j)) then
          !-------size single precision integer
          buflen = nbytsg(j)
          do i = 1, elmdms(1, j)
             buflen = buflen*elmdms(i + 1, j)
          enddo
          error = getelt(fd_nef, grpnam, elmnam, uindex, 1, buflen, buffr)
          if (error/=0) goto 9999
          goto 100
       endif
       goto 123
    endif
    !
    !-----error:
    !     writing: most likely error non existing group, so define it
    !     reading: error, no error expected
    !
  100 continue
    if (error/=0 .and. wrilog) then
       !-------Create elements
       do lelmnr = 1, nelems
          error = defelm(fd_nef, elmnms(lelmnr), elmtps(lelmnr), nbytsg(lelmnr),&
                & elmqty(lelmnr), elmunt(lelmnr), elmdes(lelmnr),               &
                & elmdms(1, lelmnr), elmdms(2, lelmnr))
          !---------most likely error, element already exist
          error = 0
       enddo
       !-------Create cells
       error = defcel(fd_nef, grpnam, nelems, elmnms)
       if (error/=0) goto 9999
       !-------Create group on definition file
       error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
       if (error/=0) goto 9999
       !-------Create group on data file
       error = credat(fd_nef, grpnam, grpnam)
       if (error/=0) goto 9999
       !-------try again to write data
       error = putelt(fd_nef, grpnam, elmnam, uindex, 1, buffr)
       if (error/=0) goto 9999
    endif
    !
    !-----No error when reading elements
    !
    if (error==0 .and. .not.wrilog) then
       error = inqelm(fd_nef, elmnam, elmtap, buflen, elmqta, elmant, elmdas,   &
             & elmndm, elmdim)
       !
       if (error/=0) goto 9999
       lelmnr = 0
       do n = 1, nelems
          if (elmnam==elmnms(n)) then
             lelmnr = n
             exit
          endif
       enddo
       if (lelmnr/=0) goto 9999
       !
       do i = 1, elmndm
          !
          !---------Compare local and global dimensions, not equal
          !         => new error number and exit
          !
          if (elmdim(i)/=elmdms(1 + i, lelmnr)) then
             error = -15025
             goto 9999
          endif
       enddo
    endif
    goto 10000
    !
 9999 continue
    if (error/=0) ierror = neferr(1, errmsg)
10000 continue
    ierror = clsnef(fd_nef)
end subroutine putgtl

subroutine crewav(filnam   ,itide    ,hrms     ,tp       ,dir      , &
                & diss     ,fx       ,fy       ,mx       ,my       , &
                & tps      ,ubot     ,wlen     ,wsbu     ,wsbv     , &
                & mmax     ,nmax     ,swflux   ,wavetime )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use wave_data
    !
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx = 19
!
! Global variables
!
    character(37)                 , intent(in)  :: filnam
    integer                       , intent(in)  :: itide
    integer                       , intent(in)  :: mmax
    integer                       , intent(in)  :: nmax
    real, dimension(mmax, nmax)   , intent(in)  :: dir
    real, dimension(mmax, nmax, 4), intent(in)  :: diss
    real, dimension(mmax, nmax)   , intent(in)  :: fx
    real, dimension(mmax, nmax)   , intent(in)  :: fy
    real, dimension(mmax, nmax)   , intent(in)  :: hrms
    real, dimension(mmax, nmax)   , intent(in)  :: mx
    real, dimension(mmax, nmax)   , intent(in)  :: my
    real, dimension(mmax, nmax)   , intent(in)  :: tp
    real, dimension(mmax, nmax)   , intent(in)  :: tps
    real, dimension(mmax, nmax)   , intent(in)  :: ubot
    real, dimension(mmax, nmax)   , intent(in)  :: wlen
    real, dimension(mmax, nmax)   , intent(in)  :: wsbu
    real, dimension(mmax, nmax)   , intent(in)  :: wsbv
    logical                       , intent(in)  :: swflux
    type (wave_time_type)                       :: wavetime

!
! Local variables
!
    integer                                 :: celidt
    integer                                 :: error
    integer                                 :: ielem
    integer                                 :: ierr
    integer                                 :: iwave
    integer                                 :: m
    integer                                 :: n
    integer                                 :: nelems
    integer, dimension(1)                   :: ival
    integer, dimension(6, nelmx)            :: elmdms
    integer, dimension(nelmx)               :: nbytsg
    real   , dimension(:,:), allocatable    :: rbuff
    logical                                 :: wrswch
    logical, dimension(1)                   :: lval
    character(10), dimension(nelmx)         :: elmunt
    character(16), dimension(2)             :: grpnam
    character(16), dimension(nelmx)         :: elmnms
    character(16), dimension(nelmx)         :: elmqty
    character(16), dimension(nelmx)         :: elmtps
    character(64), dimension(nelmx)         :: elmdes
    !
    data grpnam/'WAVNT', 'WAVTIM'/
    data elmnms/'NTWAV'  , 'SWFLUX' , 'TIMWAV', 'HRMS', 'TP', 'DIR' , 'DISTOT', &
              & 'DISSURF', 'DISWCAP', 'DISBOT', 'FX'  , 'FY', 'WSBU', 'WSBV'  , &
              & 'MX'     , 'MY'     , 'TPS'   , 'UBOT', 'WLEN'/
    data elmdes/'Number of wave fields in group WAVTIM                         ', &
              & 'Mass flux written to comm. file (.true. or .false.)           ', &
              & 'Time of wave field rel. to reference date/time                ', &
              & 'Root mean square wave height                                  ', &
              & 'Peak wave period                                              ', &
              & 'Mean direction of wave propagation relative to ksi-dir. ccw   ', &
              & 'Total wave energy dissipation rate                            ', &
              & 'Wave energy dissipation rate at the free surface              ', &
              & 'Wave energy dissipation rate due to white capping             ', &
              & 'Wave energy dissipation rate at the bottom                    ', &
              & 'Wave forcing term at the free surface in u-point              ', &
              & 'Wave forcing term at the free surface in v-point              ', &
              & 'Wave forcing term in water body in u-point                    ', &
              & 'Wave forcing term in water body in v-point                    ', &
              & 'Wave-induced volume flux in u-point                           ', &
              & 'Wave-induced volume flux in v-point                           ', &
              & 'Smoothed peak period                                          ', &
              & 'Orbital motion near the bottom                                ', &
              & 'Mean wave length                                              '/
    data elmqty/nelmx*' '/
    data elmunt/'[   -   ]', '[   -   ]', '[ TSCALE]', '[   M   ]', '[   S   ]', &
              & '[  DEG  ]', '[  W/M2 ]', '[  W/M2 ]', '[  W/M2 ]', '[  W/M2 ]', &
              & '[ N/M2  ]', '[ N/M2  ]', '[ N/M2  ]', '[ N/M2  ]', '[ M3/SM ]', &
              & '[ M3/SM ]', '[   S   ]', '[  M/S  ]', '[   M   ]'/
    data elmtps/'INTEGER', 'LOGICAL', 'INTEGER', 16*'REAL'/
    data nbytsg/nelmx*4/
!
!! executable statements -------------------------------------------------------
!
    ! Allocate temporary buffer array for switching of dimensions
    !
    allocate (rbuff (nmax,mmax))
    !
    celidt = 1
    call filldm(elmdms    ,1         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,3         ,1         ,1   , 0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,5         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,6         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,7         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,8         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,9         ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,10        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,11        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,12        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,13        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,14        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,15        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,16        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,17        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,18        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,19        ,2         ,nmax, mmax      , &
              & 0         ,0         ,0         )
    !
    !        Write all elements to file, or read them from file; all
    !        definition and creation of files, data groups, cells and
    !        elements is handled by PUTGET
    !
    iwave = itide
    if (iwave<0) then
       nelems = 2
       wrswch = .false.
       ielem = 1
       ival(1) = 0
       call putgti(filnam    ,grpnam(1) ,nelems    ,elmnms    ,elmdms    , &
                 & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
                 & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
       iwave = ival(1)+1
    endif
    !
    nelems = 2
    wrswch = .true.
    ielem = 1
    ival(1) = iwave
    call putgti(filnam    ,grpnam(1) ,nelems    ,elmnms    ,elmdms    , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
    ielem = 2
    lval(1) = swflux
    call putgtl(filnam    ,grpnam(1) ,nelems    ,elmnms    ,elmdms    , &
              & elmqty(1) ,elmunt(1) ,elmdes(1) ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,lval      )
    nelems = 17
    celidt = iwave
    ielem = 3
    ival(1) = wavetime%timtscale
    call putgti(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival      )
!
!    Hrms
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = hrms (m,n)
       enddo
    enddo

    ielem = 4
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Tp
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = tp (m,n)
       enddo
    enddo

    ielem = 5
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Dir
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = dir (m,n)
       enddo
    enddo

    ielem = 6
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Distot
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,1)
       enddo
    enddo

    ielem = 7
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Dissurf
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,2)
       enddo
    enddo

    ielem = 8
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Diswcap
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,3)
       enddo
    enddo

    ielem = 9
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Disbot
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = diss (m,n,4)
       enddo
    enddo

    ielem = 10
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Fx
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = fx (m,n)
       enddo
    enddo

    ielem = 11
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Fy
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = fy (m,n)
       enddo
    enddo

    ielem = 12
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Wsbu
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = wsbu (m,n)
       enddo
    enddo

    ielem = 13
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Wsbv
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = wsbv (m,n)
       enddo
    enddo

    ielem = 14
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Mx
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = mx (m,n)
       enddo
    enddo

    ielem = 15
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    My
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = my (m,n)
       enddo
    enddo

    ielem = 16
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Tps
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = tps(m,n)
       enddo
    enddo

    ielem = 17
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Ubot
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = ubot(m,n)
       enddo
    enddo

    ielem = 18
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!    Wlen
!
    do m = 1, mmax
       do n = 1, nmax
          rbuff (n,m) = wlen(m,n)
       enddo
    enddo

    ielem = 19
    call putgtr(filnam    ,grpnam(2) ,nelems    ,elmnms(3) ,elmdms(1, 3)         , &
              & elmqty(3) ,elmunt(3) ,elmdes(3) ,elmtps(3) ,nbytsg(3) , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
!
!   Deallocate temporary buffer array for switching of dimensions
!
    deallocate (rbuff, stat=ierr)
end subroutine crewav




subroutine crewav_netcdf(fg       ,itide    ,hrms     ,tp       ,dir      , &
                       & diss     ,fx       ,fy       ,mx       ,my       , &
                       & tps      ,ubot     ,wlen     ,wsbu     ,wsbv     , &
                       & mmax     ,nmax     ,swflux   ,wavedata ,netcdf_sp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use wave_data
    use swan_flow_grid_maps
    use netcdf
    use precision_basics
    !
    implicit none
!
! Global variables
!
    type (grid)                                 :: fg           ! flow grid
    integer                         :: itide
    integer                         :: mmax
    integer                         :: nmax
    real, dimension(mmax, nmax)     :: dir
    real, dimension(mmax, nmax, 4)  :: diss
    real, dimension(mmax, nmax)     :: fx
    real, dimension(mmax, nmax)     :: fy
    real, dimension(mmax, nmax)     :: hrms
    real, dimension(mmax, nmax)     :: mx
    real, dimension(mmax, nmax)     :: my
    real, dimension(mmax, nmax)     :: tp
    real, dimension(mmax, nmax)     :: tps
    real, dimension(mmax, nmax)     :: ubot
    real, dimension(mmax, nmax)     :: wlen
    real, dimension(mmax, nmax)     :: wsbu
    real, dimension(mmax, nmax)     :: wsbv
    logical                         :: swflux
    logical                         :: netcdf_sp
    type (wave_data_type)                       :: wavedata

!
! Local variables
!
    integer                                     :: idfile
    integer                                     :: iddim_n
    integer                                     :: iddim_time
    integer                                     :: idvar_x
    integer                                     :: idvar_y
    integer                                     :: idvar_time
    integer                                     :: idvar_hrms
    integer                                     :: idvar_tp
    integer                                     :: idvar_dir
    integer                                     :: idvar_distot
    integer                                     :: idvar_dissurf
    integer                                     :: idvar_diswcap
    integer                                     :: idvar_disbot
    integer                                     :: idvar_fx
    integer                                     :: idvar_fy
    integer                                     :: idvar_wsbu
    integer                                     :: idvar_wsbv
    integer                                     :: idvar_mx
    integer                                     :: idvar_my
    integer                                     :: idvar_tps
    integer                                     :: idvar_ubot
    integer                                     :: idvar_wlen
    integer                                     :: ierror
    integer                                     :: ind
    integer                                     :: localcomcount
    integer                                     :: mmax_read
    integer                                     :: precision
    integer, external                           :: nc_def_var
    real(hp), dimension(1)                      :: time_read
    character(50)                               :: string
    character(256)                              :: filename
    character(256)                              :: gridnam
!
!! executable statements -------------------------------------------------------
!
    localcomcount = wavedata%output%comcount
    ! localcomcount might be changed in this subroutine.
    ! wavedata%output%comcount is not allowed to be changed in this subroutine.
    !
    if (netcdf_sp) then
       precision = nf90_float
       write(*,*) "Writing data to netcdf file in single precision (except the grid)"
    else
       ! default
       precision = nf90_double
    endif
    !
    ! define name of output file
    !
    ! output-file = input-file = com-file
    ! Assumption: it already exists and already contains global information
    !
    filename = fg%grid_name
    !
    ! open file
    !
    ierror = nf90_open(filename, NF90_WRITE, idfile); call nc_check_err(ierror, "opening file", filename)
    !
    ierror = nf90_inq_dimid(idfile, 'nFlowElemWithBnd', iddim_n); call nc_check_err(ierror, "inq_dimid nFlowElemWithBnd", filename)
    ierror = nf90_inq_dimid(idfile, 'time' , iddim_time); call nc_check_err(ierror, "inq_dimid time", filename)
    ierror = nf90_inq_varid(idfile, 'time' , idvar_time   ); call nc_check_err(ierror, "inq_varid time   ", filename)
    if (localcomcount == 0) then
       localcomcount = 1
       !
       ! dimensions
       !
       !ierror = nf90_inq_dimid(idfile, 'nNetElem', iddim_n); call nc_check_err(ierror, "inq_dimid nNetElem", filename)
       ierror = nf90_inquire_dimension(idfile, iddim_n, string, mmax_read); call nc_check_err(ierror, "inq_dim nFlowElemWithBnd", filename)
       if (mmax /= mmax_read) then
          write(*,'(a,i0,a,i0,a)') "ERROR: dimension on com-file (", mmax_read, ") is not the same as the dimension to write (", mmax, ")"
          call wavestop(1, "ERROR: dimension on com-file is not the same as the dimension to write")
       endif
       !
       ierror = nf90_redef(idfile); call nc_check_err(ierror, "redef file", filename)
       !
       ! define vars
       !
       if (fg%sferic) then
          string = 'deg'
       else
          string = 'm'
       endif
       ! name, type, dims, standardname, longname, unit, xycoordinates
       idvar_x       = nc_def_var(idfile, 'x'       , nf90_double, 1, (/iddim_n/), '', 'x coordinate output grid', trim(string), .false., filename)
       idvar_y       = nc_def_var(idfile, 'y'       , nf90_double, 1, (/iddim_n/), '', 'y coordinate output grid', trim(string), .false., filename)
       idvar_hrms    = nc_def_var(idfile, 'hrms'   , precision  , 2, (/iddim_n, iddim_time/), '', 'Root mean square wave height'                               , 'm'         , .true., filename)
       idvar_tp      = nc_def_var(idfile, 'tp'     , precision  , 2, (/iddim_n, iddim_time/), '', 'Peak wave period'                                           , 's'         , .true., filename)
       idvar_dir     = nc_def_var(idfile, 'dir'    , precision  , 2, (/iddim_n, iddim_time/), '', 'Mean direction of wave propagation relative to ksi-dir. ccw', 'deg'       , .true., filename)
       idvar_distot  = nc_def_var(idfile, 'distot' , precision  , 2, (/iddim_n, iddim_time/), '', 'Total wave energy dissipation rate'                         , 'w m-2'     , .true., filename)
       idvar_dissurf = nc_def_var(idfile, 'dissurf', precision  , 2, (/iddim_n, iddim_time/), '', 'Wave energy dissipation rate at the free surface'           , 'w m-2'     , .true., filename)
       idvar_diswcap = nc_def_var(idfile, 'diswcap', precision  , 2, (/iddim_n, iddim_time/), '', 'Wave energy dissipation rate due to white capping'          , 'w m-2'     , .true., filename)
       idvar_disbot  = nc_def_var(idfile, 'disbot' , precision  , 2, (/iddim_n, iddim_time/), '', 'Wave energy dissipation rate at the bedlevel'               , 'w m-2'     , .true., filename)
       idvar_fx      = nc_def_var(idfile, 'fx'     , precision  , 2, (/iddim_n, iddim_time/), '', 'Wave forcing term at the free surface in x-direction'       , 'n m-2'     , .true., filename)
       idvar_fy      = nc_def_var(idfile, 'fy'     , precision  , 2, (/iddim_n, iddim_time/), '', 'Wave forcing term at the free surface in y-direction'       , 'n m-2'     , .true., filename)
       idvar_wsbu    = nc_def_var(idfile, 'wsbu'   , precision  , 2, (/iddim_n, iddim_time/), '', 'Wave forcing term in water body in x-direction'             , 'n m-2'     , .true., filename)
       idvar_wsbv    = nc_def_var(idfile, 'wsbv'   , precision  , 2, (/iddim_n, iddim_time/), '', 'Wave forcing term in water body in y-direction'             , 'n m-2'     , .true., filename)
       idvar_mx      = nc_def_var(idfile, 'mx'     , precision  , 2, (/iddim_n, iddim_time/), '', 'Wave-induced volume flux in x-direction'                    , 'm3 s-1 m-1', .true., filename)
       idvar_my      = nc_def_var(idfile, 'my'     , precision  , 2, (/iddim_n, iddim_time/), '', 'Wave-induced volume flux in y-direction'                    , 'm3 s-1 m-1', .true., filename)
       idvar_tps     = nc_def_var(idfile, 'tps'    , precision  , 2, (/iddim_n, iddim_time/), '', 'Smoothed peak period'                                       , 's'         , .true., filename)
       idvar_ubot    = nc_def_var(idfile, 'ubot'   , precision  , 2, (/iddim_n, iddim_time/), '', 'Orbital motion near the bottom'                             , 'm s-1'     , .true., filename)
       idvar_wlen    = nc_def_var(idfile, 'wlen'   , precision  , 2, (/iddim_n, iddim_time/), '', 'Mean wave length'                                           , 'm'         , .true., filename)
       !
       ierror = nf90_enddef(idfile); call nc_check_err(ierror, "enddef", filename)
       !
       ! put vars (time independent)
       !
       ierror = nf90_put_var(idfile, idvar_x  , fg%x  , start=(/ 1 /), count = (/ mmax /)); call nc_check_err(ierror, "put_var x", filename)
       ierror = nf90_put_var(idfile, idvar_y  , fg%y  , start=(/ 1 /), count = (/ mmax /)); call nc_check_err(ierror, "put_var y", filename)
    else
       ierror = nf90_inq_varid(idfile, 'hrms'   , idvar_hrms   ); call nc_check_err(ierror, "inq_varid hrms   ", filename)
       ierror = nf90_inq_varid(idfile, 'tp'     , idvar_tp     ); call nc_check_err(ierror, "inq_varid tp     ", filename)
       ierror = nf90_inq_varid(idfile, 'dir'    , idvar_dir    ); call nc_check_err(ierror, "inq_varid dir    ", filename)
       ierror = nf90_inq_varid(idfile, 'distot' , idvar_distot ); call nc_check_err(ierror, "inq_varid distot ", filename)
       ierror = nf90_inq_varid(idfile, 'dissurf', idvar_dissurf); call nc_check_err(ierror, "inq_varid dissurf", filename)
       ierror = nf90_inq_varid(idfile, 'diswcap', idvar_diswcap); call nc_check_err(ierror, "inq_varid diswcap", filename)
       ierror = nf90_inq_varid(idfile, 'disbot' , idvar_disbot ); call nc_check_err(ierror, "inq_varid disbot ", filename)
       ierror = nf90_inq_varid(idfile, 'fx'     , idvar_fx     ); call nc_check_err(ierror, "inq_varid fx     ", filename)
       ierror = nf90_inq_varid(idfile, 'fy'     , idvar_fy     ); call nc_check_err(ierror, "inq_varid fy     ", filename)
       ierror = nf90_inq_varid(idfile, 'wsbu'   , idvar_wsbu   ); call nc_check_err(ierror, "inq_varid wsbu   ", filename)
       ierror = nf90_inq_varid(idfile, 'wsbv'   , idvar_wsbv   ); call nc_check_err(ierror, "inq_varid wsbv   ", filename)
       ierror = nf90_inq_varid(idfile, 'mx'     , idvar_mx     ); call nc_check_err(ierror, "inq_varid mx     ", filename)
       ierror = nf90_inq_varid(idfile, 'my'     , idvar_my     ); call nc_check_err(ierror, "inq_varid my     ", filename)
       ierror = nf90_inq_varid(idfile, 'tps'    , idvar_tps    ); call nc_check_err(ierror, "inq_varid tps    ", filename)
       ierror = nf90_inq_varid(idfile, 'ubot'   , idvar_ubot   ); call nc_check_err(ierror, "inq_varid ubot   ", filename)
       ierror = nf90_inq_varid(idfile, 'wlen'   , idvar_wlen   ); call nc_check_err(ierror, "inq_varid wlen   ", filename)
    endif
    ierror = nf90_get_var(idfile, idvar_time , time_read        , start=(/ localcomcount /)   , count=(/ 1 /))
    if (localcomcount == 1) then
       ! When localcomcount>1, get_var(time) will result in an error, but this error can be ignored
       call nc_check_err(ierror, "get_var time", filename)
       if (time_read(1) /= wavedata%time%timsec) then
          write(*,'(2(a,e20.5),a)') "ERROR: time_read(", time_read, ") is not equal to curtime (", wavedata%time%timsec, ")"
       endif
    endif
    !
    ! put vars (time dependent)
!hrms = 5.0E-2
!tp   = 0.9
!dir  = 270.0
!diss(:,:,1) = 0.0
!diss(:,:,2) = 0.0
!diss(:,:,3) = 0.0
!diss(:,:,4) = 0.0
!fx   = 2.0e-3
!fy   = -9.0e-3
!wsbu = 5.0e-4
!wsbv = -4.0e-4
!mx   = 7.0e-4
!my   = -2.0e-3
!tps  = 0.0
!ubot = 0.0
!wlen = 0.0
!
!hrms = 0.0
!tp   = 0.0
!dir  = 0.0
!diss(:,:,1) = 0.0
!diss(:,:,2) = 0.0
!diss(:,:,3) = 0.0
!diss(:,:,4) = 0.0
!fx   = 0.0
!fy   = 0.0
!wsbu = 0.0
!wsbv = 0.0
!mx   = 0.0
!my   = 0.0
!tps  = 0.0
!ubot = 0.0
!wlen = 0.0
    !
    ierror = nf90_put_var(idfile, idvar_time   , wavedata%time%timsec, start=(/ localcomcount /)); call nc_check_err(ierror, "put_var time", filename)
    ierror = nf90_put_var(idfile, idvar_hrms   , hrms           , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var hrms   ", filename)
    ierror = nf90_put_var(idfile, idvar_tp     , tp             , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var tp     ", filename)
    ierror = nf90_put_var(idfile, idvar_dir    , dir            , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var dir    ", filename)
    ierror = nf90_put_var(idfile, idvar_distot , diss(:,:,1)    , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var distot ", filename)
    ierror = nf90_put_var(idfile, idvar_dissurf, diss(:,:,2)    , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var dissurf", filename)
    ierror = nf90_put_var(idfile, idvar_diswcap, diss(:,:,3)    , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var diswcap", filename)
    ierror = nf90_put_var(idfile, idvar_disbot , diss(:,:,4)    , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var disbot ", filename)
    ierror = nf90_put_var(idfile, idvar_fx     , fx             , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var fx     ", filename)
    ierror = nf90_put_var(idfile, idvar_fy     , fy             , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var fy     ", filename)
    ierror = nf90_put_var(idfile, idvar_wsbu   , wsbu           , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var wsbu   ", filename)
    ierror = nf90_put_var(idfile, idvar_wsbv   , wsbv           , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var wsbv   ", filename)
    ierror = nf90_put_var(idfile, idvar_mx     , mx             , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var mx     ", filename)
    ierror = nf90_put_var(idfile, idvar_my     , my             , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var my     ", filename)
    ierror = nf90_put_var(idfile, idvar_tps    , tps            , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var tps    ", filename)
    ierror = nf90_put_var(idfile, idvar_ubot   , ubot           , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var ubot   ", filename)
    ierror = nf90_put_var(idfile, idvar_wlen   , wlen           , start=(/ 1, localcomcount /), count = (/ mmax, 1 /)); call nc_check_err(ierror, "put_var wlen   ", filename)
    !
    ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", filename)
end subroutine crewav_netcdf
