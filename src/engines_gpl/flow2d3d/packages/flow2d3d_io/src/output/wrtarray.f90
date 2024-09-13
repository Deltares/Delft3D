module wrtarray
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: wrtarray.f90 5990 2016-04-11 13:28:37Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/wrtarray.f90 $
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

interface wrtvar
    module procedure wrtarray_int_0d
    module procedure wrtarray_int_1d
    module procedure wrtarray_int_2d
    module procedure wrtarray_int_3d
    module procedure wrtarray_hp_0d
    module procedure wrtarray_hp_1d
    module procedure wrtarray_hp_2d
    module procedure wrtarray_hp_3d
    module procedure wrtarray_sp_0d
    module procedure wrtarray_sp_1d
    module procedure wrtarray_sp_2d
    module procedure wrtarray_sp_3d
    module procedure wrtarray_char_0d
    module procedure wrtarray_char_1d
end interface wrtvar

interface wrtarray_n
    module procedure wrtarray_n_char
    module procedure wrtarray_n_fp
    module procedure wrtarray_nk_fp
    module procedure wrtarray_nl_int
    module procedure wrtarray_nl_fp
    module procedure wrtarray_nkl_fp
end interface wrtarray_n

interface wrtarray_nm
    module procedure wrtarray_nm_sp
    module procedure wrtarray_nm_hp
    module procedure wrtarray_nm_int
end interface wrtarray_nm

interface wrtarray_nm_ptr
    module procedure wrtarray_nm_sp_1d_ptr
    module procedure wrtarray_nm_hp_1d_ptr
    module procedure wrtarray_nm_sp_2d_ptr
    module procedure wrtarray_nm_hp_2d_ptr
end interface wrtarray_nm_ptr

interface wrtarray_nml_ptr
    module procedure wrtarray_nml_2d_ptr
    module procedure wrtarray_nml_3d_ptr
end interface wrtarray_nml_ptr

integer, parameter :: station = FILTER_LAST
integer, parameter :: transec = FILTER_SUM

contains

subroutine wrtarray_int_0d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer                                                                      , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
             !ierr= inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ itime /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_int_0d


subroutine wrtarray_int_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer, dimension(:)                                                        , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
             !ierr= inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, itime /), count = (/u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_int_1d


subroutine wrtarray_int_2d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer, dimension(:,:)                                                      , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
             !ierr= inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             u1 = size(var,1)
             u2 = size(var,2)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/u1, u2, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_int_2d


subroutine wrtarray_int_3d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer, dimension(:,:,:)                                                    , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: u2
    integer                                       :: u3
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
             !ierr= inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             u1 = size(var,1)
             u2 = size(var,2)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, 1, 1, itime /), count = (/u1, u2, u3, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_int_3d


subroutine wrtarray_hp_0d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(hp)                                                                     , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    real(sp)                                      :: lvar
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   lvar = real(var,sp)
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ itime /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_hp_0d


subroutine wrtarray_hp_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(hp)     , dimension(:)                                                  , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    real(sp)     , dimension(:)    , allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   allocate(lvar(u1))
                   do i1 = 1,u1
                      lvar(i1) = real(var(i1),sp)
                   enddo
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, itime /), count = (/u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_hp_1d


subroutine wrtarray_hp_2d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(hp)     , dimension(:,:)                                                , intent(in)  :: var
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
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   allocate(lvar(u1,u2))
                   do i2 = 1,u2
                      do i1 = 1,u1
                         lvar(i1,i2) = real(var(i1,i2),sp)
                      enddo
                   enddo
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/u1, u2, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_hp_2d


subroutine wrtarray_hp_3d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(hp)     , dimension(:,:,:)                                              , intent(in)  :: var
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
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   allocate(lvar(u1,u2,u3))
                   do i3 = 1,u3
                      do i2 = 1,u2
                         do i1 = 1,u1
                            lvar(i1,i2,i3) = real(var(i1,i2,i3),sp)
                         enddo
                      enddo
                   enddo
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, 1, 1, itime /), count = (/u1, u2, u3, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_hp_3d


subroutine wrtarray_sp_0d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(sp)                                                                     , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    real(hp)                                      :: lvar
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   lvar = real(var,hp)
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ itime /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_sp_0d


subroutine wrtarray_sp_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(sp)     , dimension(:)                                                  , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    real(hp)     , dimension(:)    , allocatable  :: lvar
    integer                                       :: i1
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   allocate(lvar(u1))
                   do i1 = 1,u1
                      lvar(i1) = real(var(i1),hp)
                   enddo
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, itime /), count = (/u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_sp_1d


subroutine wrtarray_sp_2d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(sp)     , dimension(:,:)                                                , intent(in)  :: var
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
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   allocate(lvar(u1,u2))
                   do i2 = 1,u2
                      do i1 = 1,u1
                         lvar(i1,i2) = real(var(i1,i2),hp)
                      enddo
                   enddo
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/u1, u2, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_sp_2d


subroutine wrtarray_sp_3d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(sp)     , dimension(:,:,:)                                              , intent(in)  :: var
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
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: inqelm
    integer                        , external     :: neferr
    integer                        , external     :: putelt
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
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
                else
                   allocate(lvar(u1,u2,u3))
                   do i3 = 1,u3
                      do i2 = 1,u2
                         do i1 = 1,u1
                            lvar(i1,i2,i3) = real(var(i1,i2,i3),hp)
                         enddo
                      enddo
                   enddo
                   ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, lvar)
                   deallocate(lvar)
                endif
             endif
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, 1, 1, itime /), count = (/u1, u2, u3, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_sp_3d


subroutine wrtarray_char_0d(fds, filename, filetype, grpnam, &
                          & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    character(*)                                                                 , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putels
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
             !ierr= inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             ierr = putels(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, itime /), count = (/len(var), 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_char_0d


subroutine wrtarray_char_1d(fds, filename, filetype, grpnam, &
                       & itime, gdp, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    character(*), dimension(:)                                                   , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: u1
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putels
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
             !ierr= inqelm (fds, varnam_nfs, elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
             ierr = putels(fds, grpnam_nfs, varnam_nfs, uindex, 1, var)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, var, start=(/ 1, 1, itime /), count = (/len(var), u1, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_char_1d


subroutine wrtarray_n_char(fds, filename, filetype, grpnam, &
                    & itime, ub1, ub1sum, ub1global, order, gdp, &
                    & ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master, parll
    use dffunctionals, only: dfgather_filter
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                         :: fds        ! file handle
    integer                                         :: itime      ! output (time) index
    character(*)                                    :: filename   ! file name
    integer                                         :: filetype   ! NEFIS or NetCDF
    integer                                         :: ub1        ! upperbound dim1 (local)
    integer                                         :: ub1sum     ! upperbound dim1 (sum of locals)
    integer                                         :: ub1global  ! upperbound dim1 (global)
    integer      , dimension(ub1)                   :: order      ! order dim1 (local)
    integer                                         :: ierr       ! error flag
    character(*) , dimension(ub1)                   :: var        ! actual data
    character(*)                                    :: grpnam     ! name of data group
    character(*)                                    :: varnam     ! name of variable
    integer                                         :: lundia     ! Description and declaration in inout.igs
    ! local
    integer                                         :: i
    character(len(var)) , dimension(:)       , allocatable :: rbuff1gl   ! work array
    ! body
    if (inode == master) then
       allocate( rbuff1gl(ub1global) )
    else
       allocate( rbuff1gl(1) )
    endif
    if (parll) then
       call dfgather_filter(lundia, ub1, ub1sum, ub1global, order, var, rbuff1gl, gdp)
    else
       do i = 1,ub1
          rbuff1gl(order(i)) = var(i)
       enddo
    endif  
    if (inode == master) then
       call wrtvar(fds, filename, filetype, grpnam, itime, &
                 & gdp, ierr, lundia, rbuff1gl, varnam)
    endif
    deallocate(rbuff1gl)
end subroutine wrtarray_n_char


subroutine wrtarray_n_fp(fds, filename, filetype, grpnam, &
                    & itime, ub1, ub1sum, ub1global, order, gdp, &
                    & ierr, lundia, var, varnam, operation)
    use precision
    use dfparall, only: inode, master, parll
    use dffunctionals, only: dfgather_filter
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                         :: fds        ! file handle
    integer                                         :: itime      ! output (time) index
    character(*)                                    :: filename   ! file name
    integer                                         :: filetype   ! NEFIS or NetCDF
    integer                                         :: operation  ! FILTER_SUM (cross sec) / FILTER_LAST (station)
    integer                                         :: ub1        ! upperbound dim1 (local)
    integer                                         :: ub1sum     ! upperbound dim1 (sum of locals)
    integer                                         :: ub1global  ! upperbound dim1 (global)
    integer      , dimension(ub1)                   :: order      ! order dim1 (local)
    integer                                         :: ierr       ! error flag
    real(fp)     , dimension(ub1)                   :: var        ! actual data
    character(*)                                    :: grpnam     ! name of data group
    character(*)                                    :: varnam     ! name of variable
    integer                                         :: lundia     ! Description and declaration in inout.igs
    ! local
    integer                                           :: i
    real(fp)       , dimension(:)       , allocatable :: rbuff1gl      ! work array
    ! body
    if (inode == master) then
       allocate( rbuff1gl(ub1global) )
    else
       allocate( rbuff1gl(1) )
    endif
    if (parll) then
       call dfgather_filter(lundia, ub1, ub1sum, ub1global, order, var, rbuff1gl, gdp, filter_op=operation)
    else
       do i = 1,ub1
          rbuff1gl(order(i)) = var(i)
       enddo
    endif  
    if (inode == master) then
       call wrtvar(fds, filename, filetype, grpnam, itime, &
                 & gdp, ierr, lundia, rbuff1gl, varnam)
    endif
    deallocate(rbuff1gl)
end subroutine wrtarray_n_fp


subroutine wrtarray_nk_fp(fds, filename, filetype, grpnam, &
                   & itime, ub1, ub1sum, ub1global, order, gdp, &
                   & shlay, kmaxout, lb2, ub2, &
                   & ierr, lundia, var, varnam, operation)
    use precision
    use dfparall, only: inode, master, parll
    use dffunctionals, only: dfgather_filter
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                         :: fds        ! file handle
    integer                                         :: itime      ! output (time) index
    character(*)                                    :: filename   ! file name
    integer                                         :: filetype   ! NEFIS or NetCDF
    integer                                         :: operation  ! FILTER_SUM (cross sec) / FILTER_LAST (station)
    integer                                         :: ub1        ! upperbound dim1 (local)
    integer                                         :: ub1sum     ! upperbound dim1 (sum of locals)
    integer                                         :: ub1global  ! upperbound dim1 (global)
    integer                                         :: lb2        ! lowerbound dim2 (layer numbers)
    integer                                         :: ub2        ! upperbound dim2 (layer numbers)
    integer      , dimension(ub1)                   :: order      ! order dim1 (local)
    integer                                         :: ierr       ! error flag
    real(fp)     , dimension(ub1, lb2:ub2)          :: var        ! actual data
    character(*)                                    :: grpnam     ! name of data group
    character(*)                                    :: varnam     ! name of variable
    integer                                         :: lundia     ! Description and declaration in inout.igs
    integer                                         :: kmaxout    ! number of output layers
    integer       , dimension(kmaxout)              :: shlay      ! indices of output layers
    ! local
    integer                                           :: i
    real(fp)       , dimension(:,:)     , allocatable :: rbuff2        ! work array
    real(fp)       , dimension(:,:)     , allocatable :: rbuff2gl      ! work array
    ! body
    !
    ! filter only the requested layers
    !
    allocate(rbuff2(ub1, 1:kmaxout))
    do i = 1,kmaxout
       rbuff2(:,i) = var(:,shlay(i))
    enddo
    !
    if (inode == master) then
       allocate( rbuff2gl(ub1global, 1:kmaxout) )
    else
       allocate( rbuff2gl(1, 1) )
    endif
    if (parll) then
       call dfgather_filter(lundia, ub1, ub1sum, ub1global, 1, kmaxout, order, rbuff2, rbuff2gl, gdp, filter_op=operation)
    else
       do i = 1,ub1
          rbuff2gl(order(i),:) = rbuff2(i,:)
       enddo
    endif
    if (inode == master) then
       call wrtvar(fds, filename, filetype, grpnam, itime, &
                 & gdp, ierr, lundia, rbuff2gl, varnam)
    endif
    deallocate(rbuff2gl)
    deallocate(rbuff2)
end subroutine wrtarray_nk_fp


subroutine wrtarray_nl_int(fds, filename, filetype, grpnam, &
                    & itime, ub1, ub1sum, ub1global, order, gdp, &
                    & ub2, &
                    & ierr, lundia, var, varnam, operation, mergedim)
    use precision
    use dfparall, only: inode, master, parll
    use dffunctionals, only: dfgather_filter
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                         :: fds        ! file handle
    integer                                         :: itime      ! output (time) index
    character(*)                                    :: filename   ! file name
    integer                                         :: filetype   ! NEFIS or NetCDF
    integer                                         :: operation  ! FILTER_SUM (cross sec) / FILTER_LAST (station)
    integer                                         :: ub1        ! upperbound dim1 (local)
    integer                                         :: ub1sum     ! upperbound dim1 (sum of locals)
    integer                                         :: ub1global  ! upperbound dim1 (global)
    integer                                         :: ub2        ! upperbound dim2
    integer      , dimension(ub1)                   :: order      ! order dim1 (local)
    integer                                         :: ierr       ! error flag
    integer      , dimension(:,:)                   :: var        ! actual data
    character(*)                                    :: grpnam     ! name of data group
    character(*)                                    :: varnam     ! name of variable
    integer                                         :: lundia     ! Description and declaration in inout.igs
    integer                              , optional :: mergedim
    ! local
    integer                                         :: i
    integer      , dimension(:,:)     , allocatable :: ibuff2gl      ! work array
    integer                                         :: dim1
    ! body
    if (inode /= master) allocate( ibuff2gl(1, 1) )
    if (present(mergedim)) then
       dim1 = mergedim
    else
       dim1 = 1
    endif
    if (dim1==1) then ! var(ub1,ub2)
       if (inode == master) allocate( ibuff2gl(ub1global, ub2) )
       if (parll) then
          call dfgather_filter(lundia, ub1, ub1sum, ub1global, 1, ub2, order, var, ibuff2gl, gdp, filter_op=operation, dim=dim1)
       else
          do i = 1,ub1
             ibuff2gl(order(i),:) = var(i,:)
          enddo
       endif
    else ! var(ub2,ub1)
       if (inode == master) allocate( ibuff2gl(ub2, ub1global) )
       if (parll) then
          call dfgather_filter(lundia, ub1, ub1sum, ub1global, 1, ub2, order, var, ibuff2gl, gdp, filter_op=operation, dim=dim1)
       else
          do i = 1,ub1
             ibuff2gl(:,order(i)) = var(:,i)
          enddo
       endif
    endif
    if (inode == master) then
       call wrtvar(fds, filename, filetype, grpnam, itime, &
                 & gdp, ierr, lundia, ibuff2gl, varnam)
    endif
    deallocate(ibuff2gl)
end subroutine wrtarray_nl_int


subroutine wrtarray_nl_fp(fds, filename, filetype, grpnam, &
                    & itime, ub1, ub1sum, ub1global, order, gdp, &
                    & ub2, &
                    & ierr, lundia, var, varnam, operation, mergedim)
    use precision
    use dfparall, only: inode, master, parll
    use dffunctionals, only: dfgather_filter
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                         :: fds        ! file handle
    integer                                         :: itime      ! output (time) index
    character(*)                                    :: filename   ! file name
    integer                                         :: filetype   ! NEFIS or NetCDF
    integer                                         :: operation  ! FILTER_SUM (cross sec) / FILTER_LAST (station)
    integer                                         :: ub1        ! upperbound dim1 (local)
    integer                                         :: ub1sum     ! upperbound dim1 (sum of locals)
    integer                                         :: ub1global  ! upperbound dim1 (global)
    integer                                         :: ub2        ! upperbound dim2
    integer      , dimension(ub1)                   :: order      ! order dim1 (local)
    integer                                         :: ierr       ! error flag
    real(fp)     , dimension(:,:)                   :: var        ! actual data
    character(*)                                    :: grpnam     ! name of data group
    character(*)                                    :: varnam     ! name of variable
    integer                                         :: lundia     ! Description and declaration in inout.igs
    integer                              , optional :: mergedim
    ! local
    integer                                         :: i
    real(fp)     , dimension(:,:)     , allocatable :: rbuff2gl      ! work array
    integer                                         :: dim1
    ! body
    if (inode /= master) allocate( rbuff2gl(1, 1) )
    if (present(mergedim)) then
       dim1 = mergedim
    else
       dim1 = 1
    endif
    if (dim1==1) then ! var(ub1,ub2)
       if (inode == master) allocate( rbuff2gl(ub1global, ub2) )
       if (parll) then
          call dfgather_filter(lundia, ub1, ub1sum, ub1global, 1, ub2, order, var, rbuff2gl, gdp, filter_op=operation, dim=dim1)
       else
          do i = 1,ub1
             rbuff2gl(order(i),:) = var(i,:)
          enddo
       endif  
    else
       if (inode == master) allocate( rbuff2gl(ub2, ub1global) )
       if (parll) then
          call dfgather_filter(lundia, ub1, ub1sum, ub1global, 1, ub2, order, var, rbuff2gl, gdp, filter_op=operation, dim=dim1)
       else
          do i = 1,ub1
             rbuff2gl(:,order(i)) = var(:,i)
          enddo
       endif  
    endif
    if (inode == master) then
       call wrtvar(fds, filename, filetype, grpnam, itime, &
                 & gdp, ierr, lundia, rbuff2gl, varnam)
    endif
    deallocate(rbuff2gl)
end subroutine wrtarray_nl_fp

    
subroutine wrtarray_nkl_fp(fds, filename, filetype, grpnam, &
                    & itime, ub1, ub1sum, ub1global, order, gdp, &
                    & shlay, kmaxout, lb2, ub2, ub3, &
                    & ierr, lundia, var, varnam, operation)
    use precision
    use dfparall, only: inode, master, parll
    use dffunctionals, only: dfgather_filter
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                         :: fds        ! file handle
    integer                                         :: itime      ! output (time) index
    character(*)                                    :: filename   ! file name
    integer                                         :: filetype   ! NEFIS or NetCDF
    integer                                         :: operation  ! FILTER_SUM (cross sec) / FILTER_LAST (station)
    integer                                         :: ub1        ! upperbound dim1 (local)
    integer                                         :: ub1sum     ! upperbound dim1 (sum of locals)
    integer                                         :: ub1global  ! upperbound dim1 (global)
    integer                                         :: lb2        ! lowerbound dim2 (layer numbers)
    integer                                         :: ub2        ! upperbound dim2 (layer numbers)
    integer                                         :: ub3        ! upperbound dim3 (lstsci or ltur)
    integer      , dimension(ub1)                   :: order      ! order dim1 (local)
    integer                                         :: ierr       ! error flag
    real(fp)     , dimension(ub1, lb2:ub2, 1:ub3)   :: var        ! actual data
    character(*)                                    :: grpnam     ! name of data group
    character(*)                                    :: varnam     ! name of variable
    integer                                         :: lundia     ! Description and declaration in inout.igs
    integer                                         :: kmaxout    ! number of output layers
    integer       , dimension(kmaxout)              :: shlay      ! indices of output layers
    ! local
    integer                                           :: i
    real(fp)       , dimension(:,:,:)   , allocatable :: rbuff3        ! work array
    real(fp)       , dimension(:,:,:)   , allocatable :: rbuff3gl      ! work array
    ! body
    !
    ! filter only the requested layers
    !
    allocate(rbuff3(ub1, 1:kmaxout, 1:ub3))
    do i = 1,kmaxout
       rbuff3(:,i,:) = var(:,shlay(i),:)
    enddo
    !
    if (inode == master) then
       allocate( rbuff3gl(ub1global, 1:kmaxout, 1:ub3) )
    else
       allocate( rbuff3gl(1, 1, 1) )
    endif
    if (parll) then
       call dfgather_filter(lundia, ub1, ub1sum, ub1global, 1, kmaxout, 1, ub3, order, rbuff3, rbuff3gl, gdp, filter_op=operation)
    else
       do i = 1,ub1
          rbuff3gl(order(i),:,:) = rbuff3(i,:,:)
       enddo
    endif
    if (inode == master) then
       call wrtvar(fds, filename, filetype, grpnam, itime, &
                 & gdp, ierr, lundia, rbuff3gl, varnam)
    endif
    deallocate(rbuff3gl)
    deallocate(rbuff3)
end subroutine wrtarray_nkl_fp


subroutine wrtarray_nmkl_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, smlay, &
                     & kmaxout, lk, uk, ul, ierr, lundia, varptr, varnam, kfmin, kfmax)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr4, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer                                                                      , intent(in)  :: kmaxout       ! length of smlay
    integer                                                                      , intent(in)  :: ul            ! upperbound dim4
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    integer      , dimension(:)                                                  , intent(in)  :: smlay
    integer      , dimension(:,:)                                                , intent(in)  :: kfmin
    integer      , dimension(:,:)                                                , intent(in)  :: kfmax
    real(fp)     , dimension(:,:,:)                                              , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    ! body
    if (associated(varptr)) then
       call wrtarray_nmkl(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, smlay, &
                     & kmaxout, lk, uk, ul, ierr, lundia, varptr, varnam, kfmin, kfmax)
    else
       !
       ! TODO: It would be more efficient to just fill glbarr4 with -999.0_fp values, but I'm not sure
       !       whether we can guarantee that it has been allocated with the appropriate size. Should
       !       check this and optimize.
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout,ul ), stat = istat )
       rbuff4(:,:,:,:) = -999.0_fp
       if (parll) then
          call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff4, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
       endif   
       deallocate(rbuff4)
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
                ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr4)
                if (ierr /= 0) then
                   ierr = neferr(0, errmsg)
                   call prterr(lundia, 'P004', errmsg)
                endif
             case (FTYPE_NETCDF)
                ierr = nf90_inq_varid(fds, varnam, idvar)
                if (ierr == nf90_noerr) then
                    ierr = nf90_put_var  (fds, idvar, glbarr4, start=(/ 1, 1, 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, kmaxout, ul, 1 /))
                endif
                call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
          endselect
       else
          ierr = 0
       endif
    endif
end subroutine wrtarray_nmkl_ptr

subroutine wrtarray_nmkl(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, smlay, &
                     & kmaxout, lk, uk, ul,ierr, lundia, var, varnam, kfmin, kfmax)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr4, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer                                                                      , intent(in)  :: kmaxout       ! length of smlay
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    integer      , dimension(1:kmaxout)                                          , intent(in)  :: smlay
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kfmin
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kfmax
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lk:uk, ul), intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: k
    integer                                       :: l
    integer                                       :: m
    integer                                       :: n
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:kmaxout, ul ), stat = istat )
    rbuff4(:,:,:,:) = -999.0_fp
    do l = 1, ul
       do k = 1, kmaxout
          do m = 1, gdp%d%mmax
             do n = 1, gdp%d%nmaxus
                if (gdp%gdprocs%zmodel) then
                   if (smlay(k)<(kfmin(n,m)-1+lk) .or. smlay(k)>kfmax(n, m)) then
                      cycle
                   endif
                endif
                rbuff4(n,m,k,l) = var(n,m,smlay(k),l)
             enddo
          enddo
       enddo
    enddo
    if (parll) then
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    else 
       call dfgather_seq(rbuff4, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif
    deallocate(rbuff4)
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr4)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbarr4, start=(/ 1, 1, 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, kmaxout, ul, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_nmkl


subroutine wrtarray_nmll(fds, filename, filetype, grpnam, &
                    & itime, nf, nl, mf, ml, iarrc, gdp, &
                    & u3, u4, ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr4, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, u3, u4), intent(in)  :: var
    character(*)                                                              , intent(in)  :: varnam
    character(*)                                                              , intent(in)  :: grpnam
    character(*)                                                              , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: m
    integer                                       :: n
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    if (parll) then
       call dfgather(var,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif   
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr4)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbarr4, start=(/ 1, 1, 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, u3, u4, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_nmll


subroutine wrtarray_nmk_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, smlay, &
                     & kmaxout, lk, uk, ierr, lundia, varptr, varnam, kfmin, kfmax)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr3, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer                                                                      , intent(in)  :: kmaxout       ! length of smlay
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                          , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc         ! array containing collected grid indices 
    integer      , dimension(:)                                                  , intent(in)  :: smlay
    integer      , dimension(:,:)                                                , intent(in)  :: kfmin
    integer      , dimension(:,:)                                                , intent(in)  :: kfmax
    real(fp)     , dimension(:,:,:)                                              , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    ! body
    if (associated(varptr)) then
       call wrtarray_nmk(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, smlay, &
                     & kmaxout, lk, uk, ierr, lundia, varptr, varnam, kfmin, kfmax)
    else
       !
       ! TODO: It would be more efficient to just fill glbarr3 with -999.0_fp values, but I'm not sure
       !       whether we can guarantee that it has been allocated with the appropriate size. Should
       !       check this and optimize.
       !
       allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout ), stat = istat )
       rbuff3(:,:,:) = -999.0_fp
       if (parll) then
          call dfgather(rbuff3,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff3, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
       endif   
       deallocate(rbuff3)
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
                ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr3)
                if (ierr /= 0) then
                   ierr = neferr(0, errmsg)
                   call prterr(lundia, 'P004', errmsg)
                endif
             case (FTYPE_NETCDF)
                ierr = nf90_inq_varid(fds, varnam, idvar)
                if (ierr == nf90_noerr) then
                    ierr = nf90_put_var  (fds, idvar, glbarr3, start=(/ 1, 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, kmaxout, 1 /))
                endif
                call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
          endselect
       else
          ierr = 0
       endif
    endif
end subroutine wrtarray_nmk_ptr

subroutine wrtarray_nmk(fds, filename, filetype, grpnam, &
                    & itime, nf, nl, mf, ml, iarrc, gdp, smlay, &
                    & kmaxout, lk, uk, ierr, lundia, var, varnam, kfmin, kfmax)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr3, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer                                                                  , intent(in)  :: lk            ! lowerbound dim3(0 or 1)
    integer                                                                  , intent(in)  :: uk            ! upperbound dim3(kmax or kmax+1)
    integer                                                                  , intent(in)  :: kmaxout       ! length of smlay
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: mf            ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: ml            ! last index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: nf            ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)                                      , intent(in)  :: nl            ! last index w.r.t. global grid in y-direction
    integer      , dimension(4,0:nproc-1)                                    , intent(in)  :: iarrc         ! array containing collected grid indices 
    integer      , dimension(1:kmaxout)                                      , intent(in)  :: smlay
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfmin
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfmax
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lk:uk), intent(in)  :: var
    character(*)                                                             , intent(in)  :: varnam
    character(*)                                                             , intent(in)  :: grpnam
    character(*)                                                             , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: k
    integer                                       :: m
    integer                                       :: n
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:kmaxout), stat = istat )
    do k=1,kmaxout
       rbuff3(:,:,k) = var(:,:,smlay(k))
    enddo
    if (gdp%gdprocs%zmodel) then
       do m = 1, gdp%d%mmax
          do n = 1, gdp%d%nmaxus
             do k = 1, kmaxout
                if (smlay(k)<(kfmin(n,m)-1+lk) .or. smlay(k)>kfmax(n, m))  rbuff3(n, m, k) = -999.0_fp
             enddo
          enddo
       enddo
    endif
    if (parll) then
       call dfgather(rbuff3,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(rbuff3, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif   
    deallocate(rbuff3)
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr3)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbarr3, start=(/ 1, 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, kmaxout, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_nmk


subroutine wrtarray_nml_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam)
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
    real(fp)     , dimension(:,:)                                                , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    real(fp)   , dimension(:,:,:)  , pointer  :: dummy
    ! body
    if (associated(varptr)) then
       call wrtarray_nml(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam)
    else
       call wrtarray_nml_3d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, dummy, varnam)
    endif
end subroutine wrtarray_nml_2d_ptr

subroutine wrtarray_nml_3d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr3, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(fp)     , dimension(:,:,:)                                              , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    ! body
    if (associated(varptr)) then
       call wrtarray_nml(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ul, ierr, lundia, varptr, varnam)
    else
       !
       ! TODO: It would be more efficient to just fill glbarr3 with -999.0_fp values, but I'm not sure
       !       whether we can guarantee that it has been allocated with the appropriate size. Should
       !       check this and optimize.
       !
       allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, ul ), stat = istat )
       rbuff3(:,:,:) = -999.0_fp
       if (parll) then
          call dfgather(rbuff3,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff3, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
       endif   
       deallocate(rbuff3)
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
                ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr3)
                if (ierr /= 0) then
                   ierr = neferr(0, errmsg)
                   call prterr(lundia, 'P004', errmsg)
                endif
             case (FTYPE_NETCDF)
                ierr = nf90_inq_varid(fds, varnam, idvar)
                if (ierr == nf90_noerr) then
                    ierr = nf90_put_var  (fds, idvar, glbarr3, start=(/ 1, 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, ul, 1 /))
                endif
                call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
          endselect
       else
          ierr = 0
       endif
    endif
end subroutine wrtarray_nml_3d_ptr

subroutine wrtarray_nml(fds, filename, filetype, grpnam, &
                    & itime, nf, nl, mf, ml, iarrc, gdp, &
                    & ul, ierr, lundia, var, varnam)
use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr3, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, ul)   , intent(in)  :: var
    character(*)                                                             , intent(in)  :: varnam
    character(*)                                                             , intent(in)  :: grpnam
    character(*)                                                             , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: m
    integer                                       :: n
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    if (parll) then
       call dfgather(var,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif   
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr3)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbarr3, start=(/ 1, 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, ul, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_nml

subroutine wrtarray_lnm(fds, filename, filetype, grpnam, &
                    & itime, nf, nl, mf, ml, iarrc, gdp, &
                    & ul, ierr, lundia, var, varnam)
use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr3, dfgather_lnm, dfgather_lnm_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(fp)     , dimension(ul, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: var
    character(*)                                                             , intent(in)  :: varnam
    character(*)                                                             , intent(in)  :: grpnam
    character(*)                                                             , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: m
    integer                                       :: n
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    if (parll) then
       call dfgather_lnm(var,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_lnm_seq(var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif   
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr3)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbarr3, start=(/ 1, 1, 1, itime /), count = (/ul, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_lnm

subroutine wrtarray_nm_sp_1d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
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
    real(sp)     , dimension(:)                                                  , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    real(sp)   , dimension(:,:)    , pointer  :: dummy
    ! body
    if (associated(varptr)) then
       call wrtarray_nm_sp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
    else
       call wrtarray_nm_sp_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, dummy, varnam)
    endif
end subroutine wrtarray_nm_sp_1d_ptr

subroutine wrtarray_nm_hp_1d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
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
    real(hp)     , dimension(:)                                                  , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    real(hp)   , dimension(:,:)    , pointer  :: dummy
    ! body
    if (associated(varptr)) then
       call wrtarray_nm_hp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
    else
       call wrtarray_nm_hp_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, dummy, varnam)
    endif
end subroutine wrtarray_nm_hp_1d_ptr

subroutine wrtarray_nm_sp_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr2, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(sp)     , dimension(:,:)                                                , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    real(sp)   , dimension(:,:)    , allocatable  :: rbuff2
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    ! body
    if (associated(varptr)) then
       call wrtarray_nm_sp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
    else
       !
       ! TODO: It would be more efficient to just fill glbarr2 with -999.0_fp values, but I'm not sure
       !       whether we can guarantee that it has been allocated with the appropriate size. Should
       !       check this and optimize.
       !
       allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), stat = istat )
       rbuff2(:,:) = -999.0_sp
       if (parll) then
          call dfgather(rbuff2,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff2, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
       endif   
       deallocate(rbuff2)
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
                ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr2)
                if (ierr /= 0) then
                   ierr = neferr(0, errmsg)
                   call prterr(lundia, 'P004', errmsg)
                endif
             case (FTYPE_NETCDF)
                ierr = nf90_inq_varid(fds, varnam, idvar)
                if (ierr == nf90_noerr) then
                    ierr = nf90_put_var  (fds, idvar, glbarr2, start=(/ 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, 1 /))
                endif
                call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
          endselect
       else
          ierr = 0
       endif
    endif
end subroutine wrtarray_nm_sp_2d_ptr

subroutine wrtarray_nm_hp_2d_ptr(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr2, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(hp)     , dimension(:,:)                                                , pointer     :: varptr
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    ! local
    integer                                       :: idvar
    integer                                       :: istat
    integer                                       :: namlen
    integer    , dimension(3,5)                   :: uindex
    real(hp)   , dimension(:,:)    , allocatable  :: rbuff2
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    ! body
    if (associated(varptr)) then
       call wrtarray_nm_hp(fds, filename, filetype, grpnam, &
                     & itime, nf, nl, mf, ml, iarrc, gdp, &
                     & ierr, lundia, varptr, varnam)
    else
       !
       ! TODO: It would be more efficient to just fill glbarr2 with -999.0_fp values, but I'm not sure
       !       whether we can guarantee that it has been allocated with the appropriate size. Should
       !       check this and optimize.
       !
       allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), stat = istat )
       rbuff2(:,:) = -999.0_hp
       if (parll) then
          call dfgather(rbuff2,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff2, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
       endif   
       deallocate(rbuff2)
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
                ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr2)
                if (ierr /= 0) then
                   ierr = neferr(0, errmsg)
                   call prterr(lundia, 'P004', errmsg)
                endif
             case (FTYPE_NETCDF)
                ierr = nf90_inq_varid(fds, varnam, idvar)
                if (ierr == nf90_noerr) then
                    ierr = nf90_put_var  (fds, idvar, glbarr2, start=(/ 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, 1 /))
                endif
                call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
          endselect
       else
          ierr = 0
       endif
    endif
end subroutine wrtarray_nm_hp_2d_ptr

subroutine wrtarray_nm_2d(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr2, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    !
    ! body
    call wrtarray_nm(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam)
end subroutine wrtarray_nm_2d

subroutine wrtarray_nm_sp(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr2, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(sp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    if (parll) then
       call dfgather(var,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif       
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr2)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbarr2, start=(/ 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_nm_sp

subroutine wrtarray_nm_hp(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbarr2, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    real(hp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    if (parll) then
       call dfgather(var,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif       
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbarr2)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbarr2, start=(/ 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_nm_hp
                   
subroutine wrtarray_nm_int(fds, filename, filetype, grpnam, &
                   & itime, nf, nl, mf, ml, iarrc, gdp, &
                   & ierr, lundia, var, varnam)
    use precision
    use dfparall, only: inode, master, nproc, parll
    use dffunctionals, only: glbari2, dfgather, dfgather_seq
    use netcdf, only: nf90_inq_varid, nf90_noerr, nf90_put_var
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
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: var
    character(*)                                                                 , intent(in)  :: varnam
    character(*)                                                                 , intent(in)  :: grpnam
    character(*)                                                                 , intent(in)  :: filename
    !
    ! local
    integer                                       :: idvar
    integer                                       :: namlen
    integer      , dimension(3,5)                 :: uindex
    character(16)                                 :: varnam_nfs
    character(16)                                 :: grpnam_nfs
    character(256)                                :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
    integer                        , external     :: neferr
    integer                        , external     :: putelt
    !
    ! body
    !
    if (parll) then
       call dfgather(var,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(var, 1-gdp%d%nlb, 1-gdp%d%mlb, gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl)
    endif       
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
             ierr = putelt(fds, grpnam_nfs, varnam_nfs, uindex, 1, glbari2)
             if (ierr /= 0) then
                ierr = neferr(0, errmsg)
                call prterr(lundia, 'P004', errmsg)
             endif
          case (FTYPE_NETCDF)
             ierr = nf90_inq_varid(fds, varnam, idvar)
             if (ierr == nf90_noerr) then
                 ierr = nf90_put_var  (fds, idvar, glbari2, start=(/ 1, 1, itime /), count = (/gdp%gdparall%nmaxgl, gdp%gdparall%mmaxgl, 1 /))
             endif
             call nc_check_err(lundia, ierr, 'writing '//varnam, filename)
       endselect
    else
       ierr = 0
    endif
end subroutine wrtarray_nm_int

end module wrtarray
