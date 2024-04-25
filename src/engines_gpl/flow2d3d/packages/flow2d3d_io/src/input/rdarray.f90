module rdarray
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
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use dffunctionals, only: FILTER_LAST, FILTER_SUM
private FILTER_LAST
private FILTER_SUM

interface rdvar
    module procedure rdarray_int_0d
    module procedure rdarray_int_1d
    module procedure rdarray_int_2d
    module procedure rdarray_int_3d
    module procedure rdarray_hp_0d
    module procedure rdarray_hp_1d
    module procedure rdarray_hp_2d
    module procedure rdarray_hp_3d
    module procedure rdarray_hp_4d
    module procedure rdarray_sp_0d
    module procedure rdarray_sp_1d
    module procedure rdarray_sp_2d
    module procedure rdarray_sp_3d
    module procedure rdarray_sp_4d
    module procedure rdarray_char_0d
    module procedure rdarray_char_1d
end interface rdvar

interface rdarray_nm
    module procedure rdarray_nm_sp
    module procedure rdarray_nm_hp
    module procedure rdarray_nm_int
end interface rdarray_nm

interface rdarray_nm_ptr
    module procedure rdarray_nm_sp_1d_ptr
    module procedure rdarray_nm_hp_1d_ptr
    module procedure rdarray_nm_sp_2d_ptr
    module procedure rdarray_nm_hp_2d_ptr
end interface rdarray_nm_ptr

interface rdarray_nml_ptr
    module procedure rdarray_nml_2d_ptr
    module procedure rdarray_nml_3d_ptr
end interface rdarray_nml_ptr

contains

subroutine rdarray_int_0d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer                                                                      , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    ! body
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4, var)
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ itime /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32,FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_int_0d


subroutine rdarray_int_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer, dimension(:)                                                        , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    ! body
    !
    u1 = size(var,1)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1, var)
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, itime /), count = (/u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32,FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_int_1d


subroutine rdarray_int_2d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer, dimension(:,:)                                                      , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2, var)
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             u1 = size(var,1)
             u2 = size(var,2)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/u1, u2, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32,FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_int_2d


subroutine rdarray_int_3d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer, dimension(:,:,:)                                                    , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    integer                                       :: u3
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    u3 = size(var,3)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2*u3, var)
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             u1 = size(var,1)
             u2 = size(var,2)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, 1, itime /), count = (/u1, u2, u3, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32,FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_int_3d


subroutine rdarray_hp_0d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(hp)                                                                     , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    real(sp)                                      :: lvar
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==hp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8, var)
                else
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4, lvar)
                   var  = real(lvar,hp)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ itime /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             read (fds, iostat=ierr) lvar
             var = real(lvar,hp)
          case (FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_hp_0d


subroutine rdarray_hp_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(hp)     , dimension(:)                                                  , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    real(sp)     , dimension(:)    , allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==hp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1, var)
                else
                   allocate(lvar(u1))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1, lvar)
                   do i1 = 1,u1
                      var(i1) = real(lvar(i1),hp)
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, itime /), count = (/u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             allocate(lvar(u1))
             read (fds, iostat=ierr) lvar
             do i1 = 1,u1
                 var(i1) = real(lvar(i1),hp)
             enddo
             deallocate(lvar)
          case (FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_hp_1d


subroutine rdarray_hp_2d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(hp)     , dimension(:,:)                                                , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    real(sp)     , dimension(:,:)  , allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: i2
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==hp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1*u2, var)
                else
                   allocate(lvar(u1,u2))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2, lvar)
                   do i2 = 1,u2
                      do i1 = 1,u1
                         var(i1,i2) = real(lvar(i1,i2),hp)
                      enddo
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/u1, u2, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             allocate(lvar(u1,u2))
             read (fds, iostat=ierr) ((lvar(i1,i2), i2 = 1,u2), i1 = 1,u1)
             do i2 = 1,u2
                 do i1 = 1,u1
                     var(i1,i2) = real(lvar(i1,i2),hp)
                 enddo
             enddo
             deallocate(lvar)
          case (FTYPE_UNFORM64)
             read (fds, iostat=ierr) ((var(i1,i2), i2 = 1,u2), i1 = 1,u1)
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_hp_2d


subroutine rdarray_hp_3d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(hp)     , dimension(:,:,:)                                              , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    integer                                       :: u3
    real(sp)     , dimension(:,:,:), allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: i2
    integer                                       :: i3
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    u3 = size(var,3)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==hp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1*u2*u3, var)
                else
                   allocate(lvar(u1,u2,u3))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2*u3, lvar)
                   do i3 = 1,u3
                      do i2 = 1,u2
                         do i1 = 1,u1
                            var(i1,i2,i3) = real(lvar(i1,i2,i3),hp)
                         enddo
                      enddo
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, 1, itime /), count = (/u1, u2, u3, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             allocate(lvar(u1,u2,1))
             do i3 = 1,u3
                read (fds, iostat=ierr) ((lvar(i1,i2,1), i2 = 1,u2), i1 = 1,u1)
                if (ierr /= 0) return
                do i2 = 1,u2
                    do i1 = 1,u1
                        var(i1,i2,i3) = real(lvar(i1,i2,1),hp)
                    enddo
                enddo
             enddo
             deallocate(lvar)
          case (FTYPE_UNFORM64)
             do i3 = 1,u3
                read (fds, iostat=ierr) ((var(i1,i2,i3), i2 = 1,u2), i1 = 1,u1)
                if (ierr /= 0) return
             enddo
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_hp_3d


subroutine rdarray_hp_4d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(hp)     , dimension(:,:,:,:)                                            , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                         :: u1
    integer                                         :: u2
    integer                                         :: u3
    integer                                         :: u4
    real(sp)     , dimension(:,:,:,:), allocatable  :: lvar
    integer                                         :: i1
    integer                                         :: i2
    integer                                         :: i3
    integer                                         :: i4
    integer                                         :: idvar
    integer                                         :: ierrdum
    integer                                         :: namlen
    integer      , dimension(3,5)                   :: uindex
    character(16)                                   :: varnam_nfs
    character(16)                                   :: grpnam_nfs
    character(256)                                  :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                          , external     :: inqelm
    integer                          , external     :: neferr
    integer                          , external     :: getelt
    !
    character(8)                                    :: elmtyp
    integer                                         :: nbytsg
    character(16)                                   :: elmqty
    character(16)                                   :: elmunt
    character(64)                                   :: elmdes
    integer                                         :: elmndm
    integer, dimension(5)                           :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    u3 = size(var,3)
    u4 = size(var,4)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==hp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1*u2*u3*u4, var)
                else
                   allocate(lvar(u1,u2,u3,u4))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2*u3*u4, lvar)
                   do i4 = 1,u4
                      do i3 = 1,u3
                         do i2 = 1,u2
                            do i1 = 1,u1
                               var(i1,i2,i3,i4) = real(lvar(i1,i2,i3,i4),hp)
                            enddo
                         enddo
                      enddo
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, 1, 1, itime /), count = (/u1, u2, u3, u4, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             allocate(lvar(u1,u2,1,1))
             do i4 = 1,u4
                do i3 = 1,u3
                   read (fds, iostat=ierr) ((lvar(i1,i2,1,1), i2 = 1,u2), i1 = 1,u1)
                   if (ierr /= 0) return
                   do i2 = 1,u2
                       do i1 = 1,u1
                           var(i1,i2,i3,i4) = lvar(i1,i2,1,1)
                       enddo
                   enddo
                enddo
             enddo
             deallocate(lvar)
          case (FTYPE_UNFORM64)
             do i4 = 1,u4
                do i3 = 1,u3
                   read (fds, iostat=ierr) ((var(i1,i2,i3,i4), i2 = 1,u2), i1 = 1,u1)
                   if (ierr /= 0) return
                enddo
             enddo
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_hp_4d

subroutine rdarray_sp_0d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(sp)                                                                     , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    real(hp)                                      :: lvar
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==sp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4, var)
                else
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8, lvar)
                   var = real(lvar,sp)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ itime /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             read (fds, iostat=ierr) var
          case (FTYPE_UNFORM64)
             read (fds, iostat=ierr) lvar
             var = real(lvar,sp)
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_sp_0d


subroutine rdarray_sp_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(sp)     , dimension(:)                                                  , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    real(hp)     , dimension(:)    , allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==sp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1, var)
                else
                   allocate(lvar(u1))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1, lvar)
                   do i1 = 1,u1
                      var(i1) = real(lvar(i1),sp)
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, itime /), count = (/u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             read (fds, iostat=ierr) var
          case (FTYPE_UNFORM64)
             allocate(lvar(u1))
             read (fds, iostat=ierr) lvar
             do i1 = 1,u1
                 var(i1) = real(lvar(i1),sp)
             enddo
             deallocate(lvar)
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_sp_1d


subroutine rdarray_sp_2d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(sp)     , dimension(:,:)                                                , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    real(hp)     , dimension(:,:)  , allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: i2
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==sp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2, var)
                else
                   allocate(lvar(u1,u2))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1*u2, lvar)
                   do i2 = 1,u2
                      do i1 = 1,u1
                         var(i1,i2) = real(lvar(i1,i2),sp)
                      enddo
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/u1, u2, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             read (fds, iostat=ierr) ((var(i1,i2), i2 = 1,u2), i1 = 1,u1)
          case (FTYPE_UNFORM64)
             allocate(lvar(u1,u2))
             read (fds, iostat=ierr) ((lvar(i1,i2), i2 = 1,u2), i1 = 1,u1)
             do i2 = 1,u2
                 do i1 = 1,u1
                     var(i1,i2) = real(lvar(i1,i2),sp)
                 enddo
             enddo
             deallocate(lvar)
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_sp_2d


subroutine rdarray_sp_3d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(sp)     , dimension(:,:,:)                                              , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    integer                                       :: u3
    real(hp)     , dimension(:,:,:), allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: i2
    integer                                       :: i3
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: getelt
    !
    character(8)                                  :: elmtyp
    integer                                       :: nbytsg
    character(16)                                 :: elmqty
    character(16)                                 :: elmunt
    character(64)                                 :: elmdes
    integer                                       :: elmndm
    integer, dimension(5)                         :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    u3 = size(var,3)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==sp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2*u3, var)
                else
                   allocate(lvar(u1,u2,u3))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1*u2*u3, lvar)
                   do i3 = 1,u3
                      do i2 = 1,u2
                         do i1 = 1,u1
                            var(i1,i2,i3) = real(lvar(i1,i2,i3),sp)
                         enddo
                      enddo
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, 1, itime /), count = (/u1, u2, u3, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             do i3 = 1,u3
                read (fds, iostat=ierr) ((var(i1,i2,i3), i2 = 1,u2), i1 = 1,u1)
                if (ierr /= 0) return
             enddo
          case (FTYPE_UNFORM64)
             allocate(lvar(u1,u2,1))
             do i3 = 1,u3
                read (fds, iostat=ierr) ((lvar(i1,i2,1), i2 = 1,u2), i1 = 1,u1)
                if (ierr /= 0) return
                do i2 = 1,u2
                    do i1 = 1,u1
                        var(i1,i2,i3) = real(lvar(i1,i2,1),sp)
                    enddo
                enddo
             enddo
             deallocate(lvar)
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_sp_3d


subroutine rdarray_sp_4d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    real(sp)     , dimension(:,:,:,:)                                            , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                         :: u1
    integer                                         :: u2
    integer                                         :: u3
    integer                                         :: u4
    real(hp)     , dimension(:,:,:,:), allocatable  :: lvar
    integer                                         :: i1
    integer                                         :: i2
    integer                                         :: i3
    integer                                         :: i4
    integer                                         :: idvar
    integer                                         :: ierrdum
    integer                                         :: namlen
    integer      , dimension(3,5)                   :: uindex
    character(16)                                   :: varnam_nfs
    character(16)                                   :: grpnam_nfs
    character(256)                                  :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                          , external     :: inqelm
    integer                          , external     :: neferr
    integer                          , external     :: getelt
    !
    character(8)                                    :: elmtyp
    integer                                         :: nbytsg
    character(16)                                   :: elmqty
    character(16)                                   :: elmunt
    character(64)                                   :: elmdes
    integer                                         :: elmndm
    integer, dimension(5)                           :: elmdms
    !
    ! body
    !
    u1 = size(var,1)
    u2 = size(var,2)
    u3 = size(var,3)
    u4 = size(var,4)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             elmndm = 5
             ierr = inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             if (ierr == 0) then
                if (nbytsg==sp) then
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 4*u1*u2*u3*u4, var)
                else
                   allocate(lvar(u1,u2,u3,u4))
                   ierr = getelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, 8*u1*u2*u3*u4, lvar)
                   do i4 = 1,u4
                      do i3 = 1,u3
                         do i2 = 1,u2
                            do i1 = 1,u1
                               var(i1,i2,i3,i4) = real(lvar(i1,i2,i3,i4),sp)
                            enddo
                         enddo
                      enddo
                   enddo
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, 1, 1, itime /), count = (/u1, u2, u3, u4, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32)
             do i4 = 1,u4
                do i3 = 1,u3
                   read (fds, iostat=ierr) ((var(i1,i2,i3,i4), i2 = 1,u2), i1 = 1,u1)
                   if (ierr /= 0) return
                enddo
             enddo
          case (FTYPE_UNFORM64)
             allocate(lvar(u1,u2,1,1))
             do i4 = 1,u4
                do i3 = 1,u3
                   read (fds, iostat=ierr) ((lvar(i1,i2,1,1), i2 = 1,u2), i1 = 1,u1)
                   if (ierr /= 0) return
                   do i2 = 1,u2
                       do i1 = 1,u1
                           var(i1,i2,i3,i4) = real(lvar(i1,i2,1,1),sp)
                       enddo
                   enddo
                enddo
             enddo
             deallocate(lvar)
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_sp_4d


subroutine rdarray_char_0d(fds, filename, filetype, grpnam, &
                          & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    character(*)                                                                 , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: getels
    !
    ! body
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             ierr = getels(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, itime /), count = (/len(var), 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32,FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_char_0d


subroutine rdarray_char_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_get_var
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    character(*), dimension(:)                                                   , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: idvar
    integer                                       :: ierrdum
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: getels
    !
    ! body
    !
    u1 = size(var,1)
    !
    if (inode == master) then
       select case (filetype)
          case (FTYPE_NEFIS)
             uindex = 0
             uindex(1,1) = itime
             uindex(2,1) = itime
             uindex(3,1) = 1
             !
             namlen = min (16,len(varnam))
             varnam_nfs = varnam(1:namlen)
             namlen = min (16,len(grpnam))
             grpnam_nfs = grpnam(1:namlen)
             !
             ierr = getels(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierrdum = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_get_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/len(var), u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'reading '//varnam, filename)
          case (FTYPE_UNFORM32,FTYPE_UNFORM64)
             read (fds, iostat=ierr) var
       endselect
    else
       ierr = 0
    endif
end subroutine rdarray_char_1d


subroutine rdarray_nmkl_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & lk, uk, ul, ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer                                                                      , intent(in)  :: lk            ! lowerbound dim3(0 or 1)
    integer                                                                      , intent(in)  :: uk            ! upperbound dim3(kmax or kmax+1)
    integer                                                                      , intent(in)  :: ul            ! upperbound dim4
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(fp)     , dimension(:,:,:)                                              , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nmkl(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & lk, uk, ul, ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nmkl_ptr

subroutine rdarray_nmkl(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & lk, uk, ul,ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: dfscatter, dfscatter_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)            :: fds
    integer                                                                      , intent(in)            :: filetype
    integer                                                                      , intent(out)           :: ierr
    integer                                                                      , intent(in)            :: itime
    integer                                                                      , intent(in)            :: lundia
    integer                                                                      , intent(in)            :: lk            ! lowerbound dim3(0 or 1)
    integer                                                                      , intent(in)            :: uk            ! upperbound dim3(kmax or kmax+1)
    integer                                                                      , intent(in)            :: ul            ! upperbound dim4
    integer      , dimension(0:nproc-1)                                          , intent(in)            :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)            :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)            :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)            :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)            :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                     , intent(in)            :: fillval       !< value used for initialization of array
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lk:uk, ul), intent(out)           :: var
    character(*)                                                                 , intent(in)            :: varnam
    character(*)                                                                 , intent(in)            :: grpnam
    character(*)                                                                 , intent(in)            :: filename
    !
    ! local
    !
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4gl
    !
    ! body
    !
    if (inode==master) then
        allocate(rbuff4gl(gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl,lk:uk,ul))
        rbuff4gl = fillval
    else
       ! Needed for debugging
       allocate(rbuff4gl(1,1,1,1))
    endif
    var = fillval
    call rdvar(fds, filename, filetype, grpnam, &
              & itime, gdp, ierr, lundia, rbuff4gl, varnam)
    if (parll) then
       call dfscatter(rbuff4gl, var, nf, nl, mf, ml, iarrc, fillval, gdp)
    else
       call dfscatter_seq(rbuff4gl, var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(rbuff4gl)
end subroutine rdarray_nmkl


subroutine rdarray_nmll(fds, filename, filetype, grpnam, &
                    & itime, nf, nl, mf, ml, iarrc, gdp, &
                    & u3, u4, ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: dfscatter, dfscatter_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                   , intent(in)  :: fds
    integer                                                                   , intent(in)  :: filetype
    integer                                                                   , intent(out) :: ierr
    integer                                                                   , intent(in)  :: itime
    integer                                                                   , intent(in)  :: lundia
    integer                                                                   , intent(in)  :: u3            ! upperbound dim3
    integer                                                                   , intent(in)  :: u4            ! upperbound dim4
    integer      , dimension(0:nproc-1)                                       , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                       , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                       , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                       , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                     , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                  , intent(in)  :: fillval       !< value used for initialization of array
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, u3, u4), intent(out) :: var
    character(*)                                                              , intent(in)  :: varnam
    character(*)                                                              , intent(in)  :: grpnam
    character(*)                                                              , intent(in)  :: filename
    !
    ! local
    !
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4gl
    !
    ! body
    !
    if (inode==master) then
        allocate(rbuff4gl(gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl,u3,u4))
        rbuff4gl = fillval
    else
       ! Needed for debugging
       allocate(rbuff4gl(1,1,1,1))
    endif
    var = fillval
    call rdvar(fds, filename, filetype, grpnam, &
              & itime, gdp, ierr, lundia, rbuff4gl, varnam)
    if (parll) then
       call dfscatter(rbuff4gl, var, nf, nl, mf, ml, iarrc, fillval, gdp)
    else
       call dfscatter_seq(rbuff4gl, var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(rbuff4gl)
end subroutine rdarray_nmll


subroutine rdarray_nmk_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & lk, uk, ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer                                                                      , intent(in)  :: lk            ! lowerbound dim3(0 or 1)
    integer                                                                      , intent(in)  :: uk            ! upperbound dim3(kmax or kmax+1)
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(fp)     , dimension(:,:,:)                                              , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nmk(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & lk, uk, ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nmk_ptr

subroutine rdarray_nmk(fds, filename, filetype, grpnam, &
                    & itime, nf, nl, mf, ml, iarrc, gdp, &
                    & lk, uk, ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: dfscatter, dfscatter_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                  , intent(in)            :: fds
    integer                                                                  , intent(in)            :: filetype
    integer                                                                  , intent(out)           :: ierr
    integer                                                                  , intent(in)            :: itime
    integer                                                                  , intent(in)            :: lundia
    integer                                                                  , intent(in)            :: lk            ! lowerbound dim3(0 or 1)
    integer                                                                  , intent(in)            :: uk            ! upperbound dim3(kmax or kmax+1)
    integer      , dimension(0:nproc-1)                                      , intent(in)            :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)            :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)            :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)            :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                    , intent(in)            :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                 , intent(in)            :: fillval       !< value used for initialization of array
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lk:uk), intent(out)           :: var
    character(*)                                                             , intent(in)            :: varnam
    character(*)                                                             , intent(in)            :: grpnam
    character(*)                                                             , intent(in)            :: filename
    !
    ! local
    !
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3gl
    !
    ! body
    !
    if (inode==master) then
        allocate(rbuff3gl(gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl,lk:uk))
        rbuff3gl = fillval
    else
       ! Needed for debugging
       allocate(rbuff3gl(1,1,1))
    endif
    var = fillval
    call rdvar(fds, filename, filetype, grpnam, &
              & itime, gdp, ierr, lundia, rbuff3gl, varnam)
    if (parll) then
       call dfscatter(rbuff3gl, var, nf, nl, mf, ml, iarrc, fillval, gdp)
    else
       call dfscatter_seq(rbuff3gl, var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(rbuff3gl)
end subroutine rdarray_nmk


subroutine rdarray_nml_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: nproc
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer                                                                      , intent(in)  :: ul            ! upperbound dim3
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(fp)     , dimension(:,:)                                                , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nml(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nml_2d_ptr

subroutine rdarray_nml_3d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer                                                                      , intent(in)  :: ul            ! upperbound dim3
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(fp)     , dimension(:,:,:)                                              , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nml(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nml_3d_ptr

subroutine rdarray_nml(fds, filename, filetype, grpnam, &
                    & itime, nf, nl, mf, ml, iarrc, gdp, &
                    & ul, ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: dfscatter, dfscatter_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                  , intent(in)  :: fds
    integer                                                                  , intent(in)  :: filetype
    integer                                                                  , intent(out) :: ierr
    integer                                                                  , intent(in)  :: itime
    integer                                                                  , intent(in)  :: lundia
    integer                                                                  , intent(in)  :: ul            ! upperbound dim3
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                    , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                 , intent(in)  :: fillval       !< value used for initialization of array
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, ul)   , intent(out) :: var
    character(*)                                                             , intent(in)  :: varnam
    character(*)                                                             , intent(in)  :: grpnam
    character(*)                                                             , intent(in)  :: filename
    !
    ! local
    !
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3gl
    !
    ! body
    !
    if (inode==master) then
        allocate(rbuff3gl(gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl,ul))
        rbuff3gl = fillval
    else
       ! Needed for debugging
       allocate(rbuff3gl(1,1,1))
    endif
    var = fillval
    call rdvar(fds, filename, filetype, grpnam, &
              & itime, gdp, ierr, lundia, rbuff3gl, varnam)
    if (parll) then
       call dfscatter(rbuff3gl, var, nf, nl, mf, ml, iarrc, fillval, gdp)
    else
       call dfscatter_seq(rbuff3gl, var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(rbuff3gl)
end subroutine rdarray_nml

subroutine rdarray_nm_sp_1d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: nproc
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(sp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(sp)     , dimension(:)                                                  , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nm_sp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nm_sp_1d_ptr

subroutine rdarray_nm_hp_1d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: nproc
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(hp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(hp)     , dimension(:)                                                  , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nm_hp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nm_hp_1d_ptr

subroutine rdarray_nm_sp_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(sp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(sp)     , dimension(:,:)                                                , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nm_sp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nm_sp_2d_ptr

subroutine rdarray_nm_hp_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(hp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(hp)     , dimension(:,:)                                                , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    if (associated(varptr)) then
       call rdarray_nm_hp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam, fillval)
    else
       ! not allowed!
    endif
end subroutine rdarray_nm_hp_2d_ptr

subroutine rdarray_nm_2d(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: nproc
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(fp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! --NONE--
    !
    ! body
    !
    call rdarray_nm(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam, fillval)
end subroutine rdarray_nm_2d

subroutine rdarray_nm_sp(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: dfscatter, dfscatter_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(sp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(sp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    real(sp)   , dimension(:,:)    , allocatable  :: rbuff2gl
    !
    ! body
    !
    if (inode==master) then
        allocate(rbuff2gl(gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl))
        rbuff2gl = fillval
    else
       ! Needed for debugging
       allocate(rbuff2gl(1,1))
    endif
    var = fillval
    call rdvar(fds, filename, filetype, grpnam, &
              & itime, gdp, ierr, lundia, rbuff2gl, varnam)
    if (parll) then
       call dfscatter(rbuff2gl, var, nf, nl, mf, ml, iarrc, fillval, gdp)
    else
       call dfscatter_seq(rbuff2gl, var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(rbuff2gl)
end subroutine rdarray_nm_sp

subroutine rdarray_nm_hp(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: dfscatter, dfscatter_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    real(hp)                                                                     , intent(in)  :: fillval       !< value used for initialization of array
    real(hp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    real(hp)   , dimension(:,:)    , allocatable  :: rbuff2gl
    integer :: n,m
    !
    ! body
    !
    if (inode==master) then
        allocate(rbuff2gl(gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl))
        rbuff2gl = fillval
    else
       ! Needed for debugging
       allocate(rbuff2gl(1,1))
    endif
    var = fillval
    call rdvar(fds, filename, filetype, grpnam, &
              & itime, gdp, ierr, lundia, rbuff2gl, varnam)
    if (parll) then
       call dfscatter(rbuff2gl, var, nf, nl, mf, ml, iarrc, fillval, gdp)
    else
       call dfscatter_seq(rbuff2gl, var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(rbuff2gl)
end subroutine rdarray_nm_hp

subroutine rdarray_nm_int(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam, fillval)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: dfscatter, dfscatter_seq
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                                                      , intent(in)  :: fds
    integer                                                                      , intent(in)  :: filetype
    integer                                                                      , intent(out) :: ierr
    integer                                                                      , intent(in)  :: itime
    integer                                                                      , intent(in)  :: lundia
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    integer                                                                      , intent(in)  :: fillval       !< value used for initialization of array
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(out) :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    integer    , dimension(:,:)    , allocatable  :: ibuff2gl
    !
    ! body
    !
    if (inode==master) then
        allocate(ibuff2gl(gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl))
        ibuff2gl = fillval
    else
       ! Needed for debugging
       allocate(ibuff2gl(1,1))
    endif
    var = fillval
    call rdvar(fds, filename, filetype, grpnam, &
              & itime, gdp, ierr, lundia, ibuff2gl, varnam)
    if (parll) then
       call dfscatter(ibuff2gl, var, nf, nl, mf, ml, iarrc, fillval, gdp)
    else
       call dfscatter_seq(ibuff2gl, var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(ibuff2gl)
end subroutine rdarray_nm_int

end module rdarray
