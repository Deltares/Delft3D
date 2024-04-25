#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
module dfscatter_module
!
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
!    Function: regroups functionalities for operations spanning all partitions
!              and requiring communications
!    Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   !
   implicit none
   !
   interface dfscatter
      module procedure dfscatter_I2e
      module procedure dfscatter_I3e
      module procedure dfscatter_R2e_sp2sp
      module procedure dfscatter_R2e_hp2sp
      module procedure dfscatter_R2e_hp2hp
      module procedure dfscatter_R3e_sp2sp
      module procedure dfscatter_R3e_hp2sp
      module procedure dfscatter_R3e_hp2hp
      module procedure dfscatter_R4e_sp2sp
      module procedure dfscatter_R4e_hp2sp
      module procedure dfscatter_R4e_hp2hp
   end interface dfscatter
   !
   interface dfscatter_seq
      module procedure dfscatter_I2_seq
      module procedure dfscatter_I3_seq
      module procedure dfscatter_R2e_seq_sp2sp
      module procedure dfscatter_R2e_seq_hp2sp
      module procedure dfscatter_R2e_seq_hp2hp
      module procedure dfscatter_R3e_seq_sp2sp
      module procedure dfscatter_R3e_seq_hp2sp
      module procedure dfscatter_R3e_seq_hp2hp
      module procedure dfscatter_R4e_seq_sp2sp
      module procedure dfscatter_R4e_seq_hp2sp
      module procedure dfscatter_R4e_seq_hp2hp
   end interface dfscatter_seq
   !
contains
!
!
!
!===============================================================================
subroutine dfscatter_I2e(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
integer                                      , intent(in)    :: fillval !< value used for initialization of array
integer, dimension(:,:)         , allocatable, intent(in)    :: inparr ! only allocated on master thread
integer, dimension(:,:)                      , intent(out)   :: ouparr
integer, dimension(4,0:nproc-1)              , intent(in)    :: iarrc
integer, dimension(0:nproc-1)                , intent(in)    :: nf
integer, dimension(0:nproc-1)                , intent(in)    :: nl
integer, dimension(0:nproc-1)                , intent(in)    :: mf
integer, dimension(0:nproc-1)                , intent(in)    :: ml
!
! Local variables
!
integer                         , pointer :: nfg
integer                         , pointer :: nlg
integer                         , pointer :: mfg
integer                         , pointer :: mlg
integer                                   :: iif
integer                                   :: iil
integer                                   :: jjf
integer                                   :: jjl
integer                                   :: ip
integer                                   :: ierr
integer                                   :: n
integer                                   :: m
integer                                   :: nm
integer                                   :: msiz
integer                                   :: nsiz
integer                                   :: lenlo
integer                                   :: lengl
integer                                   :: is
integer, dimension(:), allocatable        :: tmp
integer, dimension(:,:), allocatable      :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
             do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                tmp(nm) = inparr(n, m)
             enddo
          enddo
          is = is + msiz*nsiz
       enddo
    else
       allocate(tmp(1))
    endif
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfint, gdp )
    ouparr(iif:iil,jjf:jjl) = ouparr_slice(iif:iil,jjf:jjl)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_I2e
!
!
!
!===============================================================================
subroutine dfscatter_I3e(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
integer                                      , intent(in)    :: fillval !< value used for initialization of array
integer, dimension(:,:,:)       , allocatable, intent(in)    :: inparr ! only allocated on master thread
integer, dimension(:,:,:)                    , intent(out)   :: ouparr
integer, dimension(4,0:nproc-1)              , intent(in)    :: iarrc
integer, dimension(0:nproc-1)                , intent(in)    :: nf
integer, dimension(0:nproc-1)                , intent(in)    :: nl
integer, dimension(0:nproc-1)                , intent(in)    :: mf
integer, dimension(0:nproc-1)                , intent(in)    :: ml
!
! Local variables
!
integer                         , pointer :: nfg
integer                         , pointer :: nlg
integer                         , pointer :: mfg
integer                         , pointer :: mlg
integer                                   :: iif
integer                                   :: iil
integer                                   :: jjf
integer                                   :: jjl
integer                                   :: kf
integer                                   :: kl
integer                                   :: ip
integer                                   :: ierr
integer                                   :: k
integer                                   :: n
integer                                   :: m
integer                                   :: nm
integer                                   :: msiz
integer                                   :: nsiz
integer                                   :: lenlo
integer                                   :: lengl
integer                                   :: is
integer, dimension(:), allocatable        :: tmp
integer, dimension(:,:,:), allocatable    :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(ouparr,3)
    kl = ubound(ouparr,3)
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)
       allocate(tmp(lengl))
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do k = kf, kl
             do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
                do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                   nm = is + (k - kf)*nsiz*msiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   tmp(nm) = inparr(n, m, k)
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)
       enddo
    else
       allocate(tmp(1))
    endif
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl,kf:kl))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfint, gdp )
    ouparr(iif:iil,jjf:jjl,kf:kl) = ouparr_slice(iif:iil,jjf:jjl,kf:kl)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_I3e
!
!
!===============================================================================
subroutine dfscatter_R2e_sp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(sp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(sp), dimension(:,:)        , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(sp), dimension(:,:)                     , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                      , pointer :: nfg
integer                      , pointer :: nlg
integer                      , pointer :: mfg
integer                      , pointer :: mlg
integer                                :: iif
integer                                :: iil
integer                                :: jjf
integer                                :: jjl
integer                                :: ip
integer                                :: ierr
integer                                :: n
integer                                :: m
integer                                :: nm
integer                                :: msiz
integer                                :: nsiz
integer                                :: lenlo
integer                                :: lengl
integer                                :: is
real(sp), dimension(:), allocatable    :: tmp
real(sp), dimension(:,:), allocatable  :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
             do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                tmp(nm) = inparr(n, m)
             enddo
          enddo
          !
          is = is + msiz*nsiz
          !
       enddo
    else
       allocate(tmp(1))
    endif
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfreal, gdp )
    ouparr(iif:iil,jjf:jjl) = ouparr_slice(iif:iil,jjf:jjl)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_R2e_sp2sp
!
!
!
!===============================================================================
subroutine dfscatter_R2e_hp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(sp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(hp), dimension(:,:)        , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(sp), dimension(:,:)                     , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
real(sp), dimension(:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2)))
tmp = real(inparr,sp)
call dfscatter_R2e_sp2sp(tmp,ouparr,nf,nl,mf,ml,iarrc,fillval,gdp)
deallocate(tmp)
end subroutine dfscatter_R2e_hp2sp
!
!
!===============================================================================
subroutine dfscatter_R2e_hp2hp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(hp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(hp), dimension(:,:)        , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(hp), dimension(:,:)                     , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                      , pointer :: nfg
integer                      , pointer :: nlg
integer                      , pointer :: mfg
integer                      , pointer :: mlg
integer                                :: iif
integer                                :: iil
integer                                :: jjf
integer                                :: jjl
integer                                :: ip
integer                                :: ierr
integer                                :: n
integer                                :: m
integer                                :: nm
integer                                :: msiz
integer                                :: nsiz
integer                                :: lenlo
integer                                :: lengl
integer                                :: is
real(hp), dimension(:), allocatable    :: tmp
real(hp), dimension(:,:), allocatable  :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
             do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                tmp(nm) = inparr(n, m)
             enddo
          enddo
          !
          is = is + msiz*nsiz
          !
       enddo
    else
       allocate(tmp(1))
    endif
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfdble, gdp )
    ouparr(iif:iil,jjf:jjl) = ouparr_slice(iif:iil,jjf:jjl)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_R2e_hp2hp
!
!
!===============================================================================
subroutine dfscatter_R3e_sp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(sp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(sp), dimension(:,:,:)      , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(sp), dimension(:,:,:)                   , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                       , pointer :: nfg
integer                       , pointer :: nlg
integer                       , pointer :: mfg
integer                       , pointer :: mlg
integer                                 :: iif
integer                                 :: iil
integer                                 :: jjf
integer                                 :: jjl
integer                                 :: kf
integer                                 :: kl
integer                                 :: kf_in
integer                                 :: k_in
integer                                 :: ip
integer                                 :: ierr
integer                                 :: k
integer                                 :: n
integer                                 :: m
integer                                 :: nm
integer                                 :: msiz
integer                                 :: nsiz
integer                                 :: lenlo
integer                                 :: lengl
integer                                 :: is
real(sp), dimension(:)    , allocatable :: tmp
real(sp), dimension(:,:,:), allocatable :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(ouparr,3)
    kl = ubound(ouparr,3)
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       ! inparr must be passed through with the "allocatable" attribute, 
       ! because it is only allocated on the master thread.
       ! inparr does not loose the dimension offset: kf_in can be zero.
       ! ouparr can not be passed through with the "allocatable" attribute,
       ! because it doesn't have to be allocated.
       ! ouparr DOES loose the dimension offset: kf is always 1.
       ! Solution:
       ! - Define kf_in as the lbound of inparr (inside "inode==master": it's only defined on the master thread!)
       ! - Map (the output) k to (the input) k_in: k_in = k - kf + kf_in
       ! Assumption:
       ! - "kl_in - kf_in" is equal to "kl - kf"
       !
       kf_in = lbound(inparr,3)
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)
       allocate(tmp(lengl))
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do k = kf, kl
             do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
                do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                   nm      = is + (k - kf)*nsiz*msiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   k_in    = k - kf + kf_in
                   tmp(nm) = inparr(n, m, k_in)
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)
       enddo
    else
       allocate(tmp(1))
    endif
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl,kf:kl))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfreal, gdp )
    ouparr(iif:iil,jjf:jjl,kf:kl) = ouparr_slice(iif:iil,jjf:jjl,kf:kl)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_R3e_sp2sp
!
!
!===============================================================================
subroutine dfscatter_R3e_hp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(sp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(hp), dimension(:,:,:)      , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(sp), dimension(:,:,:)                   , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
real(sp), dimension(:,:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2),size(inparr,3)))
tmp = real(inparr,sp)
call dfscatter_R3e_sp2sp(tmp,ouparr,nf,nl,mf,ml,iarrc,fillval,gdp)
deallocate(tmp)
end subroutine dfscatter_R3e_hp2sp
!
!
!===============================================================================
subroutine dfscatter_R3e_hp2hp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(hp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(hp), dimension(:,:,:)      , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(hp), dimension(:,:,:)                   , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                       , pointer :: nfg
integer                       , pointer :: nlg
integer                       , pointer :: mfg
integer                       , pointer :: mlg
integer                                 :: iif
integer                                 :: iil
integer                                 :: jjf
integer                                 :: jjl
integer                                 :: kf
integer                                 :: kl
integer                                 :: kf_in
integer                                 :: k_in
integer                                 :: ip
integer                                 :: ierr
integer                                 :: k
integer                                 :: n
integer                                 :: m
integer                                 :: nm
integer                                 :: msiz
integer                                 :: nsiz
integer                                 :: lenlo
integer                                 :: lengl
integer                                 :: is
real(hp), dimension(:)    , allocatable :: tmp
real(hp), dimension(:,:,:), allocatable :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(ouparr,3)
    kl = ubound(ouparr,3)
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)
    iif   = 1-gdp%d%nlb+1
    iil   = gdp%d%nmax-gdp%d%nlb+1
    jjf   = -1-gdp%d%mlb+1
    jjl   = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       ! inparr must be passed through with the "allocatable" attribute, 
       ! because it is only allocated on the master thread.
       ! inparr does not loose the dimension offset: kf_in can be zero.
       ! ouparr can not be passed through with the "allocatable" attribute,
       ! because it doesn't have to be allocated.
       ! ouparr DOES loose the dimension offset: kf is always 1.
       ! Solution:
       ! - Define kf_in as the lbound of inparr (inside "inode==master": it's only defined on the master thread!)
       ! - Map (the output) k to (the input) k_in: k_in = k - kf + kf_in
       ! Assumption:
       ! - "kl_in - kf_in" is equal to "kl - kf"
       !
       kf_in = lbound(inparr,3)
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)
       allocate(tmp(lengl))
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do k = kf, kl
             do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
                do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                   nm = is + (k - kf)*nsiz*msiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   k_in    = k - kf + kf_in
                   tmp(nm) = inparr(n, m, k_in)
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)
       enddo
    else
       allocate(tmp(1))
    endif
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl,kf:kl))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfdble, gdp )
    ouparr(iif:iil,jjf:jjl,kf:kl) = ouparr_slice(iif:iil,jjf:jjl,kf:kl)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_R3e_hp2hp
!
!
!===============================================================================
subroutine dfscatter_R4e_sp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(sp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(sp), dimension(:,:,:,:)    , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(sp), dimension(:,:,:,:)                 , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                          , pointer :: nfg
integer                          , pointer :: nlg
integer                          , pointer :: mfg
integer                          , pointer :: mlg
integer                                    :: iif
integer                                    :: iil
integer                                    :: jjf
integer                                    :: jjl
integer                                    :: kf
integer                                    :: kl
integer                                    :: kf_in
integer                                    :: k_in
integer                                    :: lf
integer                                    :: ll
integer                                    :: ip
integer                                    :: istat
integer                                    :: ierr
integer                                    :: k
integer                                    :: l
integer                                    :: n
integer                                    :: m
integer                                    :: nm
integer                                    :: msiz
integer                                    :: nsiz
integer                                    :: lenlo
integer                                    :: lengl
integer                                    :: is
real(sp), dimension(:)      , allocatable  :: tmp
real(sp), dimension(:,:,:,:), allocatable  :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(ouparr,3)
    kl = ubound(ouparr,3)
    lf = lbound(ouparr,4)
    ll = ubound(ouparr,4)
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)*(ll-lf+1)
    iif = 1-gdp%d%nlb+1
    iil = gdp%d%nmax-gdp%d%nlb+1
    jjf = -1-gdp%d%mlb+1
    jjl = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       ! inparr must be passed through with the "allocatable" attribute, 
       ! because it is only allocated on the master thread.
       ! inparr does not loose the dimension offset: kf_in can be zero.
       ! ouparr can not be passed through with the "allocatable" attribute,
       ! because it doesn't have to be allocated.
       ! ouparr DOES loose the dimension offset: kf is always 1.
       ! Solution:
       ! - Define kf_in as the lbound of inparr (inside "inode==master": it's only defined on the master thread!)
       ! - Map (the output) k to (the input) k_in: k_in = k - kf + kf_in
       ! Assumption:
       ! - "kl_in - kf_in" is equal to "kl - kf"
       !
       kf_in = lbound(inparr,3)
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)*(ll-lf+1)
       allocate(tmp(lengl), stat=istat)
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do l = lf, ll
          do k = kf, kl
             do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
                do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                   nm      = is + (l - lf)*msiz*nsiz*(kl-kf+1) + (k - kf)*msiz*nsiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   k_in    = k - kf + kf_in
                   tmp(nm) = inparr(n, m, k_in, l)
                enddo
             enddo
          enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)*(ll-lf+1)
       enddo
    else
       allocate(tmp(1), stat=istat)
    endif
    if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-scatter_R4e allocation problem for tmp array'
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfreal, gdp )
    ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll) = ouparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_R4e_sp2sp
!
!
!
!===============================================================================
subroutine dfscatter_R4e_hp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(sp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(hp), dimension(:,:,:,:)    , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(sp), dimension(:,:,:,:)                 , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
real(sp), dimension(:,:,:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2),size(inparr,3),size(inparr,4)))
tmp = real(inparr,sp)
call dfscatter_R4e_sp2sp(tmp,ouparr,nf,nl,mf,ml,iarrc,fillval,gdp)
deallocate(tmp)
end subroutine dfscatter_R4e_hp2sp
!
!
!===============================================================================
subroutine dfscatter_R4e_hp2hp(inparr, ouparr, nf, nl, mf, ml, iarrc, fillval, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Scatter from inparr (on master) to distributed arrays ouparr (on all threads)
!    Method used: dfscatter + shift indices of input array (otherwise assumed
!                 array bounds from 1 to ...
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
#ifdef HAVE_MPI
    use mpi
#endif
    use precision
    use dfparall
    use globaldata
!
! Global variables
!
type(globdat), target                                        :: gdp
real(hp)                                     , intent(in)    :: fillval !< value used for initialization of array
real(hp), dimension(:,:,:,:)    , allocatable, intent(in)    :: inparr ! only allocated on master thread
real(hp), dimension(:,:,:,:)                 , intent(out)   :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                          , pointer :: nfg
integer                          , pointer :: nlg
integer                          , pointer :: mfg
integer                          , pointer :: mlg
integer                                    :: iif
integer                                    :: iil
integer                                    :: jjf
integer                                    :: jjl
integer                                    :: kf
integer                                    :: kl
integer                                    :: kf_in
integer                                    :: k_in
integer                                    :: lf
integer                                    :: ll
integer                                    :: ip
integer                                    :: istat
integer                                    :: ierr
integer                                    :: k
integer                                    :: l
integer                                    :: n
integer                                    :: m
integer                                    :: nm
integer                                    :: msiz
integer                                    :: nsiz
integer                                    :: lenlo
integer                                    :: lengl
integer                                    :: is
real(hp), dimension(:)      , allocatable  :: tmp
real(hp), dimension(:,:,:,:), allocatable  :: ouparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(ouparr,3)
    kl = ubound(ouparr,3)
    lf = lbound(ouparr,4)
    ll = ubound(ouparr,4)
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)*(ll-lf+1)
    iif = 1-gdp%d%nlb+1
    iil = gdp%d%nmax-gdp%d%nlb+1
    jjf = -1-gdp%d%mlb+1
    jjl = gdp%d%mmax+2-gdp%d%mlb+1
    !
    if (inode == master) then
       ! inparr must be passed through with the "allocatable" attribute, 
       ! because it is only allocated on the master thread.
       ! inparr does not loose the dimension offset: kf_in can be zero.
       ! ouparr can not be passed through with the "allocatable" attribute,
       ! because it doesn't have to be allocated.
       ! ouparr DOES loose the dimension offset: kf is always 1.
       ! Solution:
       ! - Define kf_in as the lbound of inparr (inside "inode==master": it's only defined on the master thread!)
       ! - Map (the output) k to (the input) k_in: k_in = k - kf + kf_in
       ! Assumption:
       ! - "kl_in - kf_in" is equal to "kl - kf"
       !
       kf_in = lbound(inparr,3)
       !
       ! determine total length of the data for all nodes and allocate the tmp array
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)*(ll-lf+1)
       allocate(tmp(lengl), stat=istat)
       tmp = fillval
       !
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do l = lf, ll
          do k = kf, kl
             do n = max(1,iarrc(3,ip)), min(iarrc(4,ip),size(inparr,1))
                do m = max(1,iarrc(1,ip)), min(iarrc(2,ip),size(inparr,2))
                   nm      = is + (l - lf)*msiz*nsiz*(kl-kf+1) + (k - kf)*msiz*nsiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   k_in    = k - kf + kf_in
                   tmp(nm) = inparr(n, m, k_in, l)
                enddo
             enddo
          enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)*(ll-lf+1)
       enddo
    else
       allocate(tmp(1), stat=istat)
    endif
    if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-scatter_R4e allocation problem for tmp array'
    !
    ! When calling dfscatter_lowlevel with 3rd argument ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, recieve the data in the local array ouparr_slice (yes, again a copy action)
    !
    allocate(ouparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll))
    call dfscatter_lowlevel ( tmp, lengl, ouparr_slice, lenlo, dfdble, gdp )
    ouparr(iif:iil,jjf:jjl,kf:kl,lf:ll) = ouparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll)
    deallocate(ouparr_slice)
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfscatter_R4e_hp2hp
!
!
!===============================================================================
subroutine dfscatter_I2_seq(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!    use precision
!    use globaldata
!
! Global variables
!
integer, dimension(:,:)                      , intent(in)    :: inparr
integer, dimension(:,:)                      , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
!
!! executable statements -------------------------------------------------------
!
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n+noff,m+moff) = inparr(n,m)
   enddo
enddo
end subroutine dfscatter_I2_seq
!
!
!===============================================================================
subroutine dfscatter_I3_seq(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!    use precision
!    use globaldata
!
! Global variables
!
integer, dimension(:,:,:)                    , intent(in)    :: inparr
integer, dimension(:,:,:)                    , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(ouparr,3)
kl = ubound(ouparr,3)
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n+noff,m+moff,k) = inparr(n,m,k)
      enddo
   enddo
enddo
end subroutine dfscatter_I3_seq
!
!===============================================================================
subroutine dfscatter_R2e_seq_sp2sp(inparr, ouparr, noff, moff, nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(sp), dimension(:,:)                     , intent(in)    :: inparr
real(sp), dimension(:,:)                     , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
!
!! executable statements -------------------------------------------------------
!
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n+noff,m+moff) = inparr(n,m)
   enddo
enddo
end subroutine dfscatter_R2e_seq_sp2sp
!
!
!===============================================================================
subroutine dfscatter_R2e_seq_hp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(hp), dimension(:,:)                     , intent(in)    :: inparr
real(sp), dimension(:,:)                     , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
!
!! executable statements -------------------------------------------------------
!
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n+noff,m+moff) = real(inparr(n,m),sp)
   enddo
enddo
end subroutine dfscatter_R2e_seq_hp2sp
!
!
!===============================================================================
subroutine dfscatter_R2e_seq_hp2hp(inparr, ouparr, noff, moff, nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(hp), dimension(:,:)                     , intent(in)    :: inparr
real(hp), dimension(:,:)                     , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
!
!! executable statements -------------------------------------------------------
!
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n+noff,m+moff) = inparr(n,m)
   enddo
enddo
end subroutine dfscatter_R2e_seq_hp2hp
!
!
!===============================================================================
subroutine dfscatter_R3e_seq_sp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(sp), dimension(:,:,:)                   , intent(in)    :: inparr
real(sp), dimension(:,:,:)                   , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(ouparr,3)
kl = ubound(ouparr,3)
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n+noff,m+moff,k) = inparr(n,m,k)
      enddo
   enddo
enddo
end subroutine dfscatter_R3e_seq_sp2sp
!
!
!===============================================================================
subroutine dfscatter_R3e_seq_hp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(hp), dimension(:,:,:)                   , intent(in)    :: inparr
real(sp), dimension(:,:,:)                   , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(ouparr,3)
kl = ubound(ouparr,3)
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n+noff,m+moff,k) = real(inparr(n,m,k),sp)
      enddo
   enddo
enddo
end subroutine dfscatter_R3e_seq_hp2sp
!
!
!===============================================================================
subroutine dfscatter_R3e_seq_hp2hp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(hp), dimension(:,:,:)                   , intent(in)    :: inparr
real(hp), dimension(:,:,:)                   , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(ouparr,3)
kl = ubound(ouparr,3)
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n+noff,m+moff,k) = inparr(n,m,k)
      enddo
   enddo
enddo
end subroutine dfscatter_R3e_seq_hp2hp
!
!
!===============================================================================
subroutine dfscatter_R4e_seq_sp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(sp), dimension(:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:)                 , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
integer                                      :: l
integer                                      :: lf
integer                                      :: ll
!
!! executable statements -------------------------------------------------------
!
kf = lbound(ouparr,3)
kl = ubound(ouparr,3)
lf = lbound(ouparr,4)
ll = ubound(ouparr,4)

do l = lf, ll
   do k = kf, kl
      do n = 1, nmaxgl
         do m = 1, mmaxgl
            ouparr(n+noff,m+moff,k,l) = inparr(n,m,k,l)
         enddo
      enddo
   enddo
enddo
end subroutine dfscatter_R4e_seq_sp2sp
!
!
!===============================================================================
subroutine dfscatter_R4e_seq_hp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(hp), dimension(:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:)                 , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
integer                                      :: l
integer                                      :: lf
integer                                      :: ll
!
!! executable statements -------------------------------------------------------
!
kf = lbound(ouparr,3)
kl = ubound(ouparr,3)
lf = lbound(ouparr,4)
ll = ubound(ouparr,4)

do l = lf, ll
   do k = kf, kl
      do n = 1, nmaxgl
         do m = 1, mmaxgl
            ouparr(n+noff,m+moff,k,l) = real(inparr(n,m,k,l),sp)
         enddo
      enddo
   enddo
enddo
end subroutine dfscatter_R4e_seq_hp2sp
!
!
!===============================================================================
subroutine dfscatter_R4e_seq_hp2hp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    scatter array to master (sequential mode)
!    Method used: shift indices of input array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
!    use globaldata
!
! Global variables
!
real(hp), dimension(:,:,:,:)                 , intent(in)    :: inparr
real(hp), dimension(:,:,:,:)                 , intent(out)   :: ouparr
integer                                      , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                      , intent(in)    :: nmaxgl
integer                                      , intent(in)    :: mmaxgl
!
! Local variables
!
integer                                      :: m
integer                                      :: n
integer                                      :: k
integer                                      :: kf
integer                                      :: kl
integer                                      :: l
integer                                      :: lf
integer                                      :: ll
!
!! executable statements -------------------------------------------------------
!
kf = lbound(ouparr,3)
kl = ubound(ouparr,3)
lf = lbound(ouparr,4)
ll = ubound(ouparr,4)

do l = lf, ll
   do k = kf, kl
      do n = 1, nmaxgl
         do m = 1, mmaxgl
            ouparr(n+noff,m+moff,k,l) = inparr(n,m,k,l)
         enddo
      enddo
   enddo
enddo
end subroutine dfscatter_R4e_seq_hp2hp

end module dfscatter_module
