#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
module dfgather_module
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
   integer , parameter :: FILTER_LAST     =  0
   integer , parameter :: FILTER_MIN      =  1
   integer , parameter :: FILTER_MAX      =  2
   integer , parameter :: FILTER_SUM      =  3
   integer , parameter :: MIN_INT_KIND_4  = -2147483647
   integer , parameter :: MAX_INT_KIND_4  =  2147483646
   real(sp), parameter :: MIN_REAL_KIND_4 = -3.4028e+38_sp
   real(sp), parameter :: MAX_REAL_KIND_4 =  3.4028e+38_sp
   real(hp), parameter :: MIN_REAL_KIND_8 = -1.7976e+308_hp
   real(hp), parameter :: MAX_REAL_KIND_8 =  1.7976e+308_hp
   !
   interface dfgather_filter
      module procedure dfgather_filter_C
      module procedure dfgather_filter_I1D
      module procedure dfgather_filter_I2D_flipper
      module procedure dfgather_filter_R1D_sp2sp
      module procedure dfgather_filter_R1D_hp2sp
      module procedure dfgather_filter_R1D_hp2hp
      module procedure dfgather_filter_R2D_sp2sp_flipper
      module procedure dfgather_filter_R2D_hp2sp_flipper
      module procedure dfgather_filter_R2D_hp2hp_flipper
      module procedure dfgather_filter_R3D_sp2sp
      module procedure dfgather_filter_R3D_hp2sp
      module procedure dfgather_filter_R3D_hp2hp
   end interface dfgather_filter
   !
   interface dfgather
      module procedure dfgather_I2e
      module procedure dfgather_I3e
      module procedure dfgather_R2e_sp2sp
      module procedure dfgather_R2e_hp2sp
      module procedure dfgather_R2e_hp2hp
      module procedure dfgather_R3e_sp2sp
      module procedure dfgather_R3e_hp2sp
      module procedure dfgather_R3e_hp2hp
      module procedure dfgather_R4e_sp2sp
      module procedure dfgather_R4e_hp2sp
      module procedure dfgather_R4e_hp2hp
      module procedure dfgather_R5e_sp2sp
      module procedure dfgather_R5e_hp2sp
      module procedure dfgather_R5e_hp2hp
   end interface dfgather
   !
   interface dfgather_seq
      module procedure dfgather_I2_seq
      module procedure dfgather_I3_seq
      module procedure dfgather_R2e_seq_sp2sp
      module procedure dfgather_R2e_seq_hp2sp
      module procedure dfgather_R2e_seq_hp2hp
      module procedure dfgather_R3e_seq_sp2sp
      module procedure dfgather_R3e_seq_hp2sp
      module procedure dfgather_R3e_seq_hp2hp
      module procedure dfgather_R4e_seq_sp2sp
      module procedure dfgather_R4e_seq_hp2sp
      module procedure dfgather_R4e_seq_hp2hp
      module procedure dfgather_R5e_seq_sp2sp
      module procedure dfgather_R5e_seq_hp2sp
      module procedure dfgather_R5e_seq_hp2hp
   end interface dfgather_seq
   !
contains
!
!
!
!===============================================================================
subroutine dfgather_filter_C(lundia, nblocal, nbtotal, nbglobal, order, &
                           & inbuff, oubuff, gdp )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer      , dimension(nblocal)     , intent(in)            :: order
character(*) , dimension(1:nblocal)   , intent(in)            :: inbuff
character(*) , dimension(1:nbglobal)                          :: oubuff
!
! Local variables
!
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer, dimension(:), allocatable        :: ibuff
character(len(inbuff)), dimension(:), allocatable  :: rbuff
!
!! executable statements -------------------------------------------------------
!
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(nbtotal) )
       allocate( ibuff(1:nbtotal) )
    else
       allocate( rbuff(1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, len(inbuff)*nbtotal, inbuff, len(inbuff)*nblocal, dfchar, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    ! condense the data by applying the operator
    if (inode == master) then
       ! only FILTER_LAST available
       do n = 1, nbtotal
          if (ibuff(n) /= 0) oubuff(ibuff(n)) = rbuff(n)
       enddo
    endif
    deallocate( ibuff )
    deallocate( rbuff )
    call dfsync(gdp)
end subroutine dfgather_filter_C
!
!
!
!===============================================================================
subroutine dfgather_filter_I1D(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer, dimension(nblocal)           , intent(in)            :: order
integer, dimension(1:nblocal)         , intent(in)            :: inbuff
integer, dimension(1:nbglobal)                                :: oubuff
integer                               , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: n
integer                                   :: ngl
integer, dimension(:), allocatable        :: rbuff
integer, dimension(:), allocatable        :: ibuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
       rbuff = 0
    else
       allocate( rbuff(1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff   , nbtotal, inbuff, nblocal, dfint, gdp )
    call dfgather_lowlevel ( ibuff   , nbtotal, order , nblocal, dfint, gdp )
    ! condense the data by applying the operator
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = oubuff(ngl) + rbuff(n)
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_INT_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = max(oubuff(ngl), rbuff(n))
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_INT_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = min(oubuff(ngl), rbuff(n))
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0.0_sp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = rbuff(n)
             endif
          enddo
       end select
    endif
    deallocate( ibuff )
    deallocate( rbuff )
    call dfsync(gdp)
end subroutine dfgather_filter_I1D
!
!
!
!===============================================================================
subroutine dfgather_filter_I2D(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer, dimension(nblocal)           , intent(in)            :: order
integer, dimension(jf:jl, 1:nblocal)  , intent(in)            :: inbuff
integer, dimension(jf:jl, 1:nbglobal)                         :: oubuff
integer                               , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer                                   :: ngl
integer, dimension(:)  , allocatable      :: ibuff
integer, dimension(:,:), allocatable      :: rbuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(jf:jl, 1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
    else
       allocate( rbuff(1, 1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*nbtotal, inbuff, (jl-jf+1)*nblocal, dfint, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    ! condense the data by applying the operator
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = oubuff(m, ngl) + rbuff(m, n)
                enddo
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_INT_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = max(oubuff(m, ngl), rbuff(m, n))
                enddo
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_INT_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = min(oubuff(m, ngl), rbuff(m, n))
                enddo
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = rbuff(m, n)
                enddo
             endif
          enddo
       end select
    endif
    deallocate( ibuff )
    deallocate( rbuff )
    call dfsync(gdp)
end subroutine dfgather_filter_I2D
!
!
!
!===============================================================================
subroutine dfgather_filter_I2D_flipper(lundia, nblocal, nbtotal, nbglobal, &
                & jf, jl, order, inbuff, oubuff, gdp, filter_op, dim )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer, dimension(nblocal)           , intent(in)            :: order
integer, dimension(:,:)               , intent(in)            :: inbuff
integer, dimension(:,:)                                       :: oubuff
integer                               , intent(in) , optional :: filter_op
integer                               , intent(in) , optional :: dim
!
! Local variables
!
integer                                   :: m
integer                                   :: n
integer, dimension(:,:), allocatable      :: tbuff_in
integer, dimension(:,:), allocatable      :: tbuff_ou
integer                                   :: operation
integer                                   :: gather_dim
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    if (present(dim)) then
        gather_dim = dim
    else
        gather_dim = 2
    endif
    !
    if (gather_dim == 1) then
       ! inbuff(1:nblocal, jf:jl)  ...or... inbuff(1:nblocal, 1:jl-jf+1)
       ! oubuff(1:nbglobal, jf:jl) ...or... oubuff(1:nbglobal, 1:jl-jf+1)
       ! reorder the incoming data because the gather dimension should be the last (slowest) dimension
       allocate( tbuff_in(1:jl-jf+1, 1:nblocal) )
       do m = 1, jl-jf+1
          do n = 1, nblocal
             tbuff_in(m, n) = inbuff(n, m)
          enddo
       enddo
       if (inode==master) then
          allocate( tbuff_ou(1:jl-jf+1, 1:nbglobal) )
       else
          allocate( tbuff_ou(1, 1) )
       endif
       ! do the gather filter operation
       call dfgather_filter_I2D(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                              & tbuff_in, tbuff_ou, gdp, operation )
       ! reorder the outgoing data
       if (inode==master) then
          do m = 1, jl-jf+1
             do n = 1, nbglobal
                oubuff(n, m) = tbuff_ou(m, n)
             enddo
          enddo
       endif
       deallocate( tbuff_ou )
       deallocate( tbuff_in )
       call dfsync(gdp)
    elseif (gather_dim == 2) then
       ! inbuff(jf:jl, 1:nblocal)  ...or... inbuff(1:jl-jf+1, 1:nblocal)
       ! oubuff(jf:jl, 1:nbglobal) ...or... oubuff(1:jl-jf+1, 1:nbglobal)
       call dfgather_filter_I2D(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                              & inbuff, oubuff, gdp, operation )
    else
       write(lundia,'(A,I5)') 'Trying to apply gather_filter along invalid dimension: ',gather_dim
    endif
end subroutine dfgather_filter_I2D_flipper
!
!
!
!===============================================================================
subroutine dfgather_filter_R1D_sp2sp(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer , dimension(nblocal)          , intent(in)            :: order
real(sp), dimension(1:nblocal)        , intent(in)            :: inbuff
real(sp), dimension(1:nbglobal)                               :: oubuff
integer                               , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: n
integer                                   :: ngl
real(sp), dimension(:), allocatable       :: rbuff
integer , dimension(:), allocatable       :: ibuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
       rbuff = 0.0_sp
    else
       allocate( rbuff(1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, nbtotal, inbuff, nblocal, dfreal, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    ! condense the data by applying the operator
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0.0_sp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = oubuff(ngl) + rbuff(n)
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_REAL_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = max(oubuff(ngl), rbuff(n))
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_REAL_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = min(oubuff(ngl), rbuff(n))
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0.0_sp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = rbuff(n)
             endif
          enddo
       end select
    endif
    deallocate( ibuff )
    deallocate( rbuff )
    call dfsync(gdp)
end subroutine dfgather_filter_R1D_sp2sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R1D_hp2sp(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!              the input of this function is in high precision, and output is in single precision.
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer , dimension(nblocal)          , intent(in)            :: order
real(hp), dimension(1:nblocal)        , intent(in)            :: inbuff
real(sp), dimension(1:nbglobal)                               :: oubuff
integer                               , intent(in) , optional :: filter_op
!
! Local variables
!
real(sp), dimension(:), allocatable       :: rbuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    allocate(rbuff(nblocal))
    rbuff = real(inbuff,sp)
    if (present(filter_op)) then
       operation = filter_op
    else
       operation = FILTER_LAST
    endif
    call dfgather_filter_R1D_sp2sp(lundia, nblocal, nbtotal, nbglobal, order, &
                                 & rbuff, oubuff, gdp, operation )
    deallocate(rbuff)
end subroutine dfgather_filter_R1D_hp2sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R1D_hp2hp(lundia, nblocal, nbtotal, nbglobal, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!              the input of this function is in high precision, and output is in high precision.
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer , dimension(nblocal)          , intent(in)            :: order
real(hp), dimension(1:nblocal)        , intent(in)            :: inbuff
real(hp), dimension(1:nbglobal)                               :: oubuff
integer                               , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: n
integer                                   :: ngl
real(hp), dimension(:), allocatable       :: rbuff
integer , dimension(:), allocatable       :: ibuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
       rbuff = 0.0_hp
    else
       allocate( rbuff(1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, nbtotal, inbuff, nblocal, dfdble, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    ! condense the data by applying the operator
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0.0_hp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = oubuff(ngl) + rbuff(n)
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_REAL_KIND_8
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = max(oubuff(ngl), rbuff(n))
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_REAL_KIND_8
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = min(oubuff(ngl), rbuff(n))
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0.0_hp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                oubuff(ngl) = rbuff(n)
             endif
          enddo
       end select
    endif
    deallocate( ibuff )
    deallocate( rbuff )
    call dfsync(gdp)
end subroutine dfgather_filter_R1D_hp2hp
!
!
!
!===============================================================================
subroutine dfgather_filter_R2D_sp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer , dimension(nblocal)          , intent(in)            :: order
real(sp), dimension(jf:jl, 1:nblocal) , intent(in)            :: inbuff
real(sp), dimension(jf:jl, 1:nbglobal)                        :: oubuff
integer                               , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer                                   :: ngl
integer , dimension(:)  , allocatable     :: ibuff
real(sp), dimension(:,:), allocatable     :: rbuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(jf:jl, 1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
    else
       allocate( rbuff(1, 1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*nbtotal, inbuff, (jl-jf+1)*nblocal, dfreal, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    ! condense the data by applying the operator
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0.0_sp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = oubuff(m, ngl) + rbuff(m, n)
                enddo
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_REAL_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = max(oubuff(m, ngl), rbuff(m, n))
                enddo
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_REAL_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = min(oubuff(m, ngl), rbuff(m, n))
                enddo
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0.0_sp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = rbuff(m, n)
                enddo
             endif
          enddo
       end select
    endif
    deallocate( ibuff )
    deallocate( rbuff )
    call dfsync(gdp)
end subroutine dfgather_filter_R2D_sp2sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R2D_sp2sp_flipper(lundia, nblocal, nbtotal, nbglobal, &
                & jf, jl, order, inbuff, oubuff, gdp, filter_op, dim )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer,  dimension(nblocal)          , intent(in)            :: order
real(sp), dimension(:,:)              , intent(in)            :: inbuff
real(sp), dimension(:,:)                                      :: oubuff
integer                               , intent(in) , optional :: filter_op
integer                               , intent(in) , optional :: dim
!
! Local variables
!
integer                                   :: m
integer                                   :: n
real(sp), dimension(:,:), allocatable     :: tbuff_in
real(sp), dimension(:,:), allocatable     :: tbuff_ou
integer                                   :: operation
integer                                   :: gather_dim
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    if (present(dim)) then
        gather_dim = dim
    else
        gather_dim = 1
    endif
    !
    if (gather_dim == 1) then
       ! inbuff(1:nblocal, jf:jl)  ...or... inbuff(1:nblocal, 1:jl-jf+1)
       ! oubuff(1:nbglobal, jf:jl) ...or... oubuff(1:nbglobal, 1:jl-jf+1)
       ! reorder the incoming data because the gather dimension should be the last (slowest) dimension
       allocate( tbuff_in(1:jl-jf+1, 1:nblocal) )
       do m = 1, jl-jf+1
          do n = 1, nblocal
             tbuff_in(m, n) = inbuff(n, m)
          enddo
       enddo
       if (inode==master) then
          allocate( tbuff_ou(1:jl-jf+1, 1:nbglobal) )
       else
          allocate( tbuff_ou(1, 1) )
       endif
       ! do the gather filter operation
       call dfgather_filter_R2D_sp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                                    & tbuff_in, tbuff_ou, gdp, operation )
       ! reorder the outgoing data
       if (inode==master) then
          do m = 1, jl-jf+1
             do n = 1, nbglobal
                oubuff(n, m) = tbuff_ou(m, n)
             enddo
          enddo
       endif
       deallocate( tbuff_ou )
       deallocate( tbuff_in )
       call dfsync(gdp)
    elseif (gather_dim == 2) then
       ! inbuff(jf:jl, 1:nblocal)  ...or... inbuff(1:jl-jf+1, 1:nblocal)
       ! oubuff(jf:jl, 1:nbglobal) ...or... oubuff(1:jl-jf+1, 1:nbglobal)
       call dfgather_filter_R2D_sp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                                    & inbuff, oubuff, gdp, operation )
    else
       write(lundia,'(A,I5)') 'Trying to apply gather_filter along invalid dimension: ',gather_dim
    endif
end subroutine dfgather_filter_R2D_sp2sp_flipper
!
!
!
!===============================================================================
subroutine dfgather_filter_R2D_hp2sp_flipper(lundia, nblocal, nbtotal, nbglobal, &
                & jf, jl, order, inbuff, oubuff, gdp, filter_op, dim )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer,  dimension(nblocal)          , intent(in)            :: order
real(hp), dimension(:,:)              , intent(in)            :: inbuff
real(sp), dimension(:,:)                                      :: oubuff
integer                               , intent(in) , optional :: filter_op
integer                               , intent(in) , optional :: dim
!
! Local variables
!
integer                                   :: m
integer                                   :: n
real(sp), dimension(:,:), allocatable     :: tbuff_in
real(sp), dimension(:,:), allocatable     :: tbuff_ou
integer                                   :: operation
integer                                   :: gather_dim
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    if (present(dim)) then
        gather_dim = dim
    else
        gather_dim = 1
    endif
    !
    if (gather_dim == 1) then
       ! inbuff(1:nblocal, jf:jl)  ...or... inbuff(1:nblocal, 1:jl-jf+1)
       ! oubuff(1:nbglobal, jf:jl) ...or... oubuff(1:nbglobal, 1:jl-jf+1)
       ! reorder the incoming data because the gather dimension should be the last (slowest) dimension
       allocate( tbuff_in(1:jl-jf+1, 1:nblocal) )
       do m = 1, jl-jf+1
          do n = 1, nblocal
             tbuff_in(m, n) = inbuff(n, m)
          enddo
       enddo
       if (inode==master) then
          allocate( tbuff_ou(1:jl-jf+1, 1:nbglobal) )
       else
          allocate( tbuff_ou(1, 1) )
       endif
       ! do the gather filter operation
       call dfgather_filter_R2D_sp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                                    & tbuff_in, tbuff_ou, gdp, operation )
       ! reorder the outgoing data
       if (inode==master) then
          do m = 1, jl-jf+1
             do n = 1, nbglobal
                oubuff(n, m) = tbuff_ou(m, n)
             enddo
          enddo
       endif
       deallocate( tbuff_ou )
       deallocate( tbuff_in )
       call dfsync(gdp)
    elseif (gather_dim == 2) then
       ! inbuff(jf:jl, 1:nblocal)  ...or... inbuff(1:jl-jf+1, 1:nblocal)
       ! oubuff(jf:jl, 1:nbglobal) ...or... oubuff(1:jl-jf+1, 1:nbglobal)
       ! no flipping of dimensions needed, only convert to single precision
       allocate(tbuff_in(1:jl-jf+1,1:nblocal))
       tbuff_in = real(inbuff,sp)
       call dfgather_filter_R2D_sp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                                    & tbuff_in, oubuff, gdp, operation )
       deallocate( tbuff_in )
    else
       write(lundia,'(A,I5)') 'Trying to apply gather_filter along invalid dimension: ',gather_dim
    endif
end subroutine dfgather_filter_R2D_hp2sp_flipper
!
!
!
!===============================================================================
subroutine dfgather_filter_R2D_hp2hp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer , dimension(nblocal)          , intent(in)            :: order
real(hp), dimension(jf:jl, 1:nblocal) , intent(in)            :: inbuff
real(hp), dimension(jf:jl, 1:nbglobal)                        :: oubuff
integer                               , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer                                   :: ngl
integer , dimension(:)  , allocatable     :: ibuff
real(hp), dimension(:,:), allocatable     :: rbuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(jf:jl, 1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
    else
       allocate( rbuff(1, 1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*nbtotal, inbuff, (jl-jf+1)*nblocal, dfdble, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    ! condense the data by applying the operator
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0.0_hp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = oubuff(m, ngl) + rbuff(m, n)
                enddo
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_REAL_KIND_8
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = max(oubuff(m, ngl), rbuff(m, n))
                enddo
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_REAL_KIND_8
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = min(oubuff(m, ngl), rbuff(m, n))
                enddo
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0.0_hp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do m = jf, jl
                   oubuff(m, ngl) = rbuff(m, n)
                enddo
             endif
          enddo
       end select
    endif
    deallocate( ibuff )
    deallocate( rbuff )
    call dfsync(gdp)
end subroutine dfgather_filter_R2D_hp2hp
!
!
!
!===============================================================================
subroutine dfgather_filter_R2D_hp2hp_flipper(lundia, nblocal, nbtotal, nbglobal, &
                & jf, jl, order, inbuff, oubuff, gdp, filter_op, dim )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                         :: gdp
integer                                                       :: lundia !  Description and declaration in inout.igs
integer                                                       :: nblocal
integer                                                       :: nbtotal
integer                                                       :: nbglobal
integer                                                       :: jf
integer                                                       :: jl
integer,  dimension(nblocal)          , intent(in)            :: order
real(hp), dimension(:,:)              , intent(in)            :: inbuff
real(hp), dimension(:,:)                                      :: oubuff
integer                               , intent(in) , optional :: filter_op
integer                               , intent(in) , optional :: dim
!
! Local variables
!
integer                                   :: m
integer                                   :: n
real(hp), dimension(:,:), allocatable     :: tbuff_in
real(hp), dimension(:,:), allocatable     :: tbuff_ou
integer                                   :: operation
integer                                   :: gather_dim
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    if (present(dim)) then
        gather_dim = dim
    else
        gather_dim = 1
    endif
    !
    if (gather_dim == 1) then
       ! inbuff(1:nblocal, jf:jl)  ...or... inbuff(1:nblocal, 1:jl-jf+1)
       ! oubuff(1:nbglobal, jf:jl) ...or... oubuff(1:nbglobal, 1:jl-jf+1)
       ! reorder the incoming data because the gather dimension should be the last (slowest) dimension
       allocate( tbuff_in(1:jl-jf+1, 1:nblocal) )
       do m = 1, jl-jf+1
          do n = 1, nblocal
             tbuff_in(m, n) = inbuff(n, m)
          enddo
       enddo
       if (inode==master) then
          allocate( tbuff_ou(1:jl-jf+1, 1:nbglobal) )
       else
          allocate( tbuff_ou(1, 1) )
       endif
       ! do the gather filter operation
       call dfgather_filter_R2D_hp2hp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                                    & tbuff_in, tbuff_ou, gdp, operation )
       ! reorder the outgoing data
       if (inode==master) then
          do m = 1, jl-jf+1
             do n = 1, nbglobal
                oubuff(n, m) = tbuff_ou(m, n)
             enddo
          enddo
       endif
       deallocate( tbuff_ou )
       deallocate( tbuff_in )
       call dfsync(gdp)
    elseif (gather_dim == 2) then
       ! inbuff(jf:jl, 1:nblocal)  ...or... inbuff(1:jl-jf+1, 1:nblocal)
       ! oubuff(jf:jl, 1:nbglobal) ...or... oubuff(1:jl-jf+1, 1:nbglobal)
       call dfgather_filter_R2D_hp2hp(lundia, nblocal, nbtotal, nbglobal, jf, jl, order, &
                                    & inbuff, oubuff, gdp, operation )
    else
       write(lundia,'(A,I5)') 'Trying to apply gather_filter along invalid dimension: ',gather_dim
    endif
end subroutine dfgather_filter_R2D_hp2hp_flipper
!
!
!
!===============================================================================
subroutine dfgather_filter_R3D_sp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, kf, kl, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                                :: gdp
integer                                                              :: lundia !  Description and declaration in inout.igs
integer                                                              :: nblocal
integer                                                              :: nbtotal
integer                                                              :: nbglobal
integer                                                              :: jf
integer                                                              :: jl
integer                                                              :: kf
integer                                                              :: kl
integer , dimension(nblocal)                 , intent(in)            :: order
real(sp), dimension(1:nblocal, jf:jl, kf:kl) , intent(in)            :: inbuff
real(sp), dimension(1:nbglobal, jf:jl, kf:kl)                        :: oubuff
integer                                      , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer                                   :: ngl
integer , dimension(:)    , allocatable   :: ibuff
real(sp), dimension(:,:,:), allocatable   :: rbuff
real(sp), dimension(:,:,:), allocatable   :: tbuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! reorder the incoming data because the gather dimension should be the last (slowest) dimension
    allocate( tbuff(jf:jl, kf:kl, 1:nblocal) )
    do k = kf, kl
       do m = jf, jl
          do n = 1, nblocal
             tbuff(m, k, n) = inbuff(n, m, k)
          enddo
       enddo
    enddo
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(jf:jl, kf:kl, 1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
    else
       allocate( rbuff(1, 1, 1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*(kl-kf+1)*nbtotal, tbuff, (jl-jf+1)*(kl-kf+1)*nblocal, dfreal, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    deallocate( tbuff )
    ! condense the data by applying the operator - in the meantime change the dimensions back to original order
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0.0_sp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = oubuff(ngl, m, k) + rbuff(m, k, n)
                   enddo
                enddo
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_REAL_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = max(oubuff(ngl, m, k), rbuff(m, k, n))
                   enddo
                enddo
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_REAL_KIND_4
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = min(oubuff(ngl, m, k), rbuff(m, k, n))
                   enddo
                enddo
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0.0_sp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = rbuff(m, k, n)
                   enddo
                enddo
             endif
          enddo
       end select
    endif
    deallocate( rbuff )
    deallocate( ibuff )
    call dfsync(gdp)
end subroutine dfgather_filter_R3D_sp2sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R3D_hp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, kf, kl, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                                :: gdp
integer                                                              :: lundia !  Description and declaration in inout.igs
integer                                                              :: nblocal
integer                                                              :: nbtotal
integer                                                              :: nbglobal
integer                                                              :: jf
integer                                                              :: jl
integer                                                              :: kf
integer                                                              :: kl
integer , dimension(nblocal)                 , intent(in)            :: order
real(hp), dimension(1:nblocal, jf:jl, kf:kl) , intent(in)            :: inbuff
real(sp), dimension(1:nbglobal, jf:jl, kf:kl)                        :: oubuff
integer                                      , intent(in) , optional :: filter_op
!
! Local variables
!
real(sp), dimension(:,:,:), allocatable   :: rbuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    allocate(rbuff(1:nblocal, jf:jl, kf:kl))
    rbuff = real(inbuff,sp)
    call dfgather_filter_R3D_sp2sp(lundia, nblocal, nbtotal, nbglobal, jf, jl, kf, kl, order, &
                             & rbuff, oubuff, gdp, operation )
    deallocate(rbuff)
end subroutine dfgather_filter_R3D_hp2sp
!
!
!
!===============================================================================
subroutine dfgather_filter_R3D_hp2hp(lundia, nblocal, nbtotal, nbglobal, jf, jl, kf, kl, order, &
                             & inbuff, oubuff, gdp, filter_op )
!!--description-----------------------------------------------------------------
!
!    Function: gather point-wise quantities, excluding duplicates over partitions
!    Method used:
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
type(globdat), target                                                :: gdp
integer                                                              :: lundia !  Description and declaration in inout.igs
integer                                                              :: nblocal
integer                                                              :: nbtotal
integer                                                              :: nbglobal
integer                                                              :: jf
integer                                                              :: jl
integer                                                              :: kf
integer                                                              :: kl
integer , dimension(nblocal)                 , intent(in)            :: order
real(hp), dimension(1:nblocal, jf:jl, kf:kl) , intent(in)            :: inbuff
real(hp), dimension(1:nbglobal, jf:jl, kf:kl)                        :: oubuff
integer                                      , intent(in) , optional :: filter_op
!
! Local variables
!
integer                                   :: k
integer                                   :: m
integer                                   :: n
integer                                   :: ngl
integer , dimension(:)    , allocatable   :: ibuff
real(hp), dimension(:,:,:), allocatable   :: rbuff
real(hp), dimension(:,:,:), allocatable   :: tbuff
integer                                   :: operation
!
!! executable statements -------------------------------------------------------
!
    if (present(filter_op)) then
        operation = filter_op
    else
        operation = FILTER_LAST
    endif
    ! reorder the incoming data because the gather dimension should be the last (slowest) dimension
    allocate( tbuff(jf:jl, kf:kl, 1:nblocal) )
    do m = jf, jl
       do k = kf, kl
          do n = 1, nblocal
             tbuff(m, k, n) = inbuff(n, m, k)
          enddo
       enddo
    enddo
    ! gather the data from all nodes to the master
    if (inode == master) then
       allocate( rbuff(jf:jl, kf:kl, 1:nbtotal) )
       allocate( ibuff(1:nbtotal) )
    else
       allocate( rbuff(1, 1, 1) )
       allocate( ibuff(1) )
    endif
    call dfgather_lowlevel ( rbuff, (jl-jf+1)*(kl-kf+1)*nbtotal, tbuff, (jl-jf+1)*(kl-kf+1)*nblocal, dfdble, gdp )
    call dfgather_lowlevel ( ibuff, nbtotal, order, nblocal, dfint, gdp )
    deallocate( tbuff )
    ! condense the data by applying the operator - in the meantime change the dimensions back to original order
    if (inode == master) then
       select case (operation)
       case (FILTER_SUM)
          oubuff = 0.0_hp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = oubuff(ngl, m, k) + rbuff(m, k, n)
                   enddo
                enddo
             endif
          enddo
       case (FILTER_MAX)
          oubuff = MIN_REAL_KIND_8
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = max(oubuff(ngl, m, k), rbuff(m, k, n))
                   enddo
                enddo
             endif
          enddo
       case (FILTER_MIN)
          oubuff = MAX_REAL_KIND_8
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = min(oubuff(ngl, m, k), rbuff(m, k, n))
                   enddo
                enddo
             endif
          enddo
       case (FILTER_LAST)
          oubuff = 0.0_hp
          do n = 1, nbtotal
             ngl = ibuff(n)
             if (ngl /= 0 .and. ngl <= nbglobal) then
                do k = kf, kl
                   do m = jf, jl
                      oubuff(ngl, m, k) = rbuff(m, k, n)
                   enddo
                enddo
             endif
          enddo
       end select
    endif
    deallocate( rbuff )
    deallocate( ibuff )
    call dfsync(gdp)
end subroutine dfgather_filter_R3D_hp2hp
!
!
!
!===============================================================================
subroutine dfgather_I2e(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
integer, dimension(:,:)                      , intent(in)    :: inparr
integer, dimension(:,:)         , allocatable, intent(inout) :: ouparr
integer, dimension(4,0:nproc-1)              , intent(in)    :: iarrc
integer, dimension(0:nproc-1)                , intent(in)    :: nf
integer, dimension(0:nproc-1)                , intent(in)    :: nl
integer, dimension(0:nproc-1)                , intent(in)    :: mf
integer, dimension(0:nproc-1)                , intent(in)    :: ml
!
! Local variables
!
integer                         , pointer :: nmaxgl
integer                         , pointer :: mmaxgl
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
integer, dimension(:,:), allocatable      :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
    else
       allocate(tmp(1))
    endif
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
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl))
    inparr_slice(iif:iil,jjf:jjl) = inparr(iif:iil,jjf:jjl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfint, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl) )
       ouparr = -999
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                ouparr(n, m) = tmp(nm)
             enddo
          enddo
          is = is + msiz*nsiz
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_I2e
!
!
!
!===============================================================================
subroutine dfgather_I3e(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
integer, dimension(:,:,:)                    , intent(in)    :: inparr
integer, dimension(:,:,:)       , allocatable, intent(inout) :: ouparr
integer, dimension(4,0:nproc-1)              , intent(in)    :: iarrc
integer, dimension(0:nproc-1)                , intent(in)    :: nf
integer, dimension(0:nproc-1)                , intent(in)    :: nl
integer, dimension(0:nproc-1)                , intent(in)    :: mf
integer, dimension(0:nproc-1)                , intent(in)    :: ml
!
! Local variables
!
integer                         , pointer :: nmaxgl
integer                         , pointer :: mmaxgl
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
integer, dimension(:,:,:), allocatable    :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
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
    else
       allocate(tmp(1))
    endif
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
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl))
    inparr_slice(iif:iil,jjf:jjl,kf:kl) = inparr(iif:iil,jjf:jjl,kf:kl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfint, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl, kf:kl) )
       ouparr = -999
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do k = kf, kl
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   nm = is + (k - kf)*nsiz*msiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   ouparr(n, m, k) = tmp(nm)
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_I3e
!
!
!===============================================================================
subroutine dfgather_R2e_sp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(sp), dimension(:,:)                     , intent(in)    :: inparr
real(sp), dimension(:,:)        , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                      , pointer :: nmaxgl
integer                      , pointer :: mmaxgl
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
real(sp), dimension(:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
    else
       allocate(tmp(1))
    endif
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
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl))
    inparr_slice(iif:iil,jjf:jjl) = inparr(iif:iil,jjf:jjl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfreal, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl) )
       ouparr = -999.0_sp
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                ouparr(n, m) = tmp(nm)
             enddo
          enddo
          !
          is = is + msiz*nsiz
          !
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R2e_sp2sp
!
!
!
!===============================================================================
subroutine dfgather_R2e_hp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(hp), dimension(:,:)                     , intent(in)    :: inparr
real(sp), dimension(:,:)        , allocatable, intent(inout) :: ouparr
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
call dfgather_R2e_sp2sp(tmp,ouparr,nf,nl,mf,ml,iarrc,gdp)
deallocate(tmp)
end subroutine dfgather_R2e_hp2sp
!
!
!===============================================================================
subroutine dfgather_R2e_hp2hp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(hp), dimension(:,:)                     , intent(in)    :: inparr
real(hp), dimension(:,:)        , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                      , pointer :: nmaxgl
integer                      , pointer :: mmaxgl
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
real(hp), dimension(:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       allocate(tmp(lengl))
    else
       allocate(tmp(1))
    endif
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
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl))
    inparr_slice(iif:iil,jjf:jjl) = inparr(iif:iil,jjf:jjl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfdble, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl) )
       ouparr = -999.0_hp
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                nm = is + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                ouparr(n, m) = tmp(nm)
             enddo
          enddo
          !
          is = is + msiz*nsiz
          !
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R2e_hp2hp
!
!
!===============================================================================
subroutine dfgather_R3e_sp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(sp), dimension(:,:,:)                   , intent(in)    :: inparr
real(sp), dimension(:,:,:)      , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                       , pointer :: nmaxgl
integer                       , pointer :: mmaxgl
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
real(sp), dimension(:,:,:), allocatable :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
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
    else
       allocate(tmp(1))
    endif
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
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl,kf:kl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl))
    inparr_slice(iif:iil,jjf:jjl,kf:kl) = inparr(iif:iil,jjf:jjl,kf:kl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfreal, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl, kf:kl) )
       ouparr = -999.0_sp
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do k = kf, kl
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   nm = is + (k - kf)*nsiz*msiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   ouparr(n, m, k) = tmp(nm)
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R3e_sp2sp
!
!
!===============================================================================
subroutine dfgather_R3e_hp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(hp), dimension(:,:,:)                   , intent(in)    :: inparr
real(sp), dimension(:,:,:)      , allocatable, intent(inout) :: ouparr
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
call dfgather_R3e_sp2sp(tmp,ouparr,nf,nl,mf,ml,iarrc,gdp)
deallocate(tmp)
end subroutine dfgather_R3e_hp2sp
!
!
!===============================================================================
subroutine dfgather_R3e_hp2hp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(hp), dimension(:,:,:)                   , intent(in)    :: inparr
real(hp), dimension(:,:,:)      , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                       , pointer :: nmaxgl
integer                       , pointer :: mmaxgl
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
real(hp), dimension(:,:,:), allocatable :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
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
    else
       allocate(tmp(1))
    endif
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
    ! When calling dfgather_lowlevel with 3rd argument inparr(iif:iil,jjf:jjl,kf:kl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl))
    inparr_slice(iif:iil,jjf:jjl,kf:kl) = inparr(iif:iil,jjf:jjl,kf:kl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfdble, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl, kf:kl) )
       ouparr = -999.0_hp
       is = 0
       do ip = 0, nproc-1
          !
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          !
          do k = kf, kl
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   nm = is + (k - kf)*nsiz*msiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   ouparr(n, m, k) = tmp(nm)
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R3e_hp2hp
!
!
!===============================================================================
subroutine dfgather_R4e_sp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(sp), dimension(:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                          , pointer :: nmaxgl
integer                          , pointer :: mmaxgl
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
real(sp), dimension(:,:,:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    lf = lbound(inparr,4)
    ll = ubound(inparr,4)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
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
    else
       allocate(tmp(1), stat=istat)
    endif
    if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R4e allocation problem for tmp array'
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
    ! When calling dfgather_lowlevel with four-argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll))
    inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll) = inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfreal, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl, kf:kl, lf:ll) , stat=istat)
       ouparr = -999.0_sp
       if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R4e allocation problem for ouparr array'
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do l = lf, ll
          do k = kf, kl
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   nm = is + (l - lf)*msiz*nsiz*(kl-kf+1) + (k - kf)*msiz*nsiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   ouparr(n, m, k, l) = tmp(nm)
                enddo
             enddo
          enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)*(ll-lf+1)
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R4e_sp2sp
!
!
!
!===============================================================================
subroutine dfgather_R4e_hp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(hp), dimension(:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:)    , allocatable, intent(inout) :: ouparr
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
call dfgather_R4e_sp2sp(tmp,ouparr,nf,nl,mf,ml,iarrc,gdp)
deallocate(tmp)
end subroutine dfgather_R4e_hp2sp
!
!
!===============================================================================
subroutine dfgather_R4e_hp2hp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
real(hp), dimension(:,:,:,:)                 , intent(in)    :: inparr
real(hp), dimension(:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)             , intent(in)    :: iarrc
integer , dimension(0:nproc-1)               , intent(in)    :: nf
integer , dimension(0:nproc-1)               , intent(in)    :: nl
integer , dimension(0:nproc-1)               , intent(in)    :: mf
integer , dimension(0:nproc-1)               , intent(in)    :: ml
!
! Local variables
!
integer                          , pointer :: nmaxgl
integer                          , pointer :: mmaxgl
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
real(hp), dimension(:,:,:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    lf = lbound(inparr,4)
    ll = ubound(inparr,4)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
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
    else
       allocate(tmp(1), stat=istat)
    endif
    if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R4e allocation problem for tmp array'
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
    ! When calling dfgather_lowlevel with four-argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll))
    inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll) = inparr(iif:iil,jjf:jjl,kf:kl,lf:ll)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfdble, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl, kf:kl, lf:ll) , stat=istat)
       ouparr = -999.0_hp
       if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R4e allocation problem for ouparr array'
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do l = lf, ll
          do k = kf, kl
             do n = nf(ip), nl(ip)
                do m = mf(ip), ml(ip)
                   nm = is + (l - lf)*msiz*nsiz*(kl-kf+1) + (k - kf)*msiz*nsiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                   ouparr(n, m, k, l) = tmp(nm)
                enddo
             enddo
          enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)*(ll-lf+1)
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R4e_hp2hp
!
!
!===============================================================================
subroutine dfgather_R5e_sp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
type(globdat), target                                          :: gdp
real(sp), dimension(:,:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)               , intent(in)    :: iarrc
integer , dimension(0:nproc-1)                 , intent(in)    :: nf
integer , dimension(0:nproc-1)                 , intent(in)    :: nl
integer , dimension(0:nproc-1)                 , intent(in)    :: mf
integer , dimension(0:nproc-1)                 , intent(in)    :: ml
!
! Local variables
!
integer                            , pointer :: nmaxgl
integer                            , pointer :: mmaxgl
integer                            , pointer :: nfg
integer                            , pointer :: nlg
integer                            , pointer :: mfg
integer                            , pointer :: mlg
integer                                      :: iif
integer                                      :: iil
integer                                      :: jjf
integer                                      :: jjl
integer                                      :: kf
integer                                      :: kl
integer                                      :: lf
integer                                      :: ll
integer                                      :: pf
integer                                      :: pl
integer                                      :: ip
integer                                      :: istat
integer                                      :: ierr
integer                                      :: k
integer                                      :: l
integer                                      :: n
integer                                      :: m
integer                                      :: p
integer                                      :: nm
integer                                      :: msiz
integer                                      :: nsiz
integer                                      :: lenlo
integer                                      :: lengl
integer                                      :: is
real(sp), dimension(:)        , allocatable  :: tmp
real(sp), dimension(:,:,:,:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    lf = lbound(inparr,4)
    ll = ubound(inparr,4)
    pf = lbound(inparr,5)
    pl = ubound(inparr,5)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)*(ll-lf+1)*(pl-pf+1)
       allocate(tmp(lengl), stat=istat)
    else
       allocate(tmp(1), stat=istat)
    endif
    if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R5e allocation problem for tmp array'
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)*(ll-lf+1)*(pl-pf+1)
    iif = 1-gdp%d%nlb+1
    iil = gdp%d%nmax-gdp%d%nlb+1
    jjf = -1-gdp%d%mlb+1
    jjl = gdp%d%mmax+2-gdp%d%mlb+1
    !
    ! When calling dfgather_lowlevel with five-argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl))
    inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl) = inparr(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfreal, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl, kf:kl, lf:ll, pf:pl) , stat=istat)
       ouparr = -999.0_sp
       if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R4e allocation problem for ouparr array'
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do p = pf, pl
             do l = lf, ll
                do k = kf, kl
                   do n = nf(ip), nl(ip)
                      do m = mf(ip), ml(ip)
                         nm = is + (p - pf)*msiz*nsiz*(kl-kf+1)*(ll-lf+1) + (l - lf)*msiz*nsiz*(kl-kf+1) + (k - kf)*msiz*nsiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                         ouparr(n, m, k, l, p) = tmp(nm)
                      enddo
                   enddo
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)*(ll-lf+1)*(pl-pf+1)
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R5e_sp2sp
!
!
!
!===============================================================================
subroutine dfgather_R5e_hp2sp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
type(globdat), target                                          :: gdp
real(hp), dimension(:,:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)               , intent(in)    :: iarrc
integer , dimension(0:nproc-1)                 , intent(in)    :: nf
integer , dimension(0:nproc-1)                 , intent(in)    :: nl
integer , dimension(0:nproc-1)                 , intent(in)    :: mf
integer , dimension(0:nproc-1)                 , intent(in)    :: ml
!
! Local variables
!
real(sp), dimension(:,:,:,:,:), allocatable    :: tmp
!
!! executable statements -------------------------------------------------------
!
allocate(tmp(size(inparr,1),size(inparr,2),size(inparr,3),size(inparr,4),size(inparr,5)))
tmp = real(inparr,sp)
call dfgather_R5e_sp2sp(tmp,ouparr,nf,nl,mf,ml,iarrc,gdp)
deallocate(tmp)
end subroutine dfgather_R5e_hp2sp
!
!
!===============================================================================
subroutine dfgather_R5e_hp2hp(inparr, ouparr, nf, nl, mf, ml, iarrc, gdp)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather distributed arrays to ouparr (on master)
!    Method used: dfgather + shift indices of input array (otherwise assumed
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
type(globdat), target                                          :: gdp
real(hp), dimension(:,:,:,:,:)                 , intent(in)    :: inparr
real(hp), dimension(:,:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer , dimension(4,0:nproc-1)               , intent(in)    :: iarrc
integer , dimension(0:nproc-1)                 , intent(in)    :: nf
integer , dimension(0:nproc-1)                 , intent(in)    :: nl
integer , dimension(0:nproc-1)                 , intent(in)    :: mf
integer , dimension(0:nproc-1)                 , intent(in)    :: ml
!
! Local variables
!
integer                            , pointer :: nmaxgl
integer                            , pointer :: mmaxgl
integer                            , pointer :: nfg
integer                            , pointer :: nlg
integer                            , pointer :: mfg
integer                            , pointer :: mlg
integer                                      :: iif
integer                                      :: iil
integer                                      :: jjf
integer                                      :: jjl
integer                                      :: kf
integer                                      :: kl
integer                                      :: lf
integer                                      :: ll
integer                                      :: pf
integer                                      :: pl
integer                                      :: ip
integer                                      :: istat
integer                                      :: ierr
integer                                      :: k
integer                                      :: l
integer                                      :: n
integer                                      :: m
integer                                      :: p
integer                                      :: nm
integer                                      :: msiz
integer                                      :: nsiz
integer                                      :: lenlo
integer                                      :: lengl
integer                                      :: is
real(hp), dimension(:)        , allocatable  :: tmp
real(hp), dimension(:,:,:,:,:), allocatable  :: inparr_slice
!
!! executable statements -------------------------------------------------------
!
    kf = lbound(inparr,3)
    kl = ubound(inparr,3)
    lf = lbound(inparr,4)
    ll = ubound(inparr,4)
    pf = lbound(inparr,5)
    pl = ubound(inparr,5)
    if (inode == master) then
       !
       ! determine total length for collecting data of all nodes and allocate arrays
       !
       lengl = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          lengl = lengl + msiz*nsiz
       enddo
       lengl = lengl*(kl-kf+1)*(ll-lf+1)*(pl-pf+1)
       allocate(tmp(lengl), stat=istat)
    else
       allocate(tmp(1), stat=istat)
    endif
    if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R5e allocation problem for tmp array'
    nfg => gdp%gdparall%nfg
    nlg => gdp%gdparall%nlg
    mfg => gdp%gdparall%mfg
    mlg => gdp%gdparall%mlg
    msiz = (mlg + 2) - (mfg - 2) + 1
    nsiz = nlg - nfg +1
    if (mod(nsiz,2)==0) nsiz = nsiz + 1
    lenlo = msiz*nsiz*(kl-kf+1)*(ll-lf+1)*(pl-pf+1)
    iif = 1-gdp%d%nlb+1
    iil = gdp%d%nmax-gdp%d%nlb+1
    jjf = -1-gdp%d%mlb+1
    jjl = gdp%d%mmax+2-gdp%d%mlb+1
    !
    ! When calling dfgather_lowlevel with five-argument inparr(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl)
    ! this (possibly big) array is placed on the stack
    ! To avoid this, copy it to the local array inparr_slice (yes, again a copy action)
    !
    allocate(inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl))
    inparr_slice(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl) = inparr(iif:iil,jjf:jjl,kf:kl,lf:ll,pf:pl)
    call dfgather_lowlevel ( tmp, lengl, inparr_slice, lenlo, dfdble, gdp )
    deallocate(inparr_slice)
    if (inode == master) then
       nmaxgl => gdp%gdparall%nmaxgl
       mmaxgl => gdp%gdparall%mmaxgl
       if (allocated(ouparr)) deallocate(ouparr)
       allocate( ouparr(nmaxgl, mmaxgl, kf:kl, lf:ll, pf:pl) , stat=istat)
       ouparr = -999.0_hp
       if (istat /= 0) write(gdp%gdinout%lundia,*)'dffunctionals.f90-gather_R5e allocation problem for ouparr array'
       is = 0
       do ip = 0, nproc-1
          msiz = iarrc(2,ip)-iarrc(1,ip)+1
          nsiz = iarrc(4,ip)-iarrc(3,ip)+1
          if (mod(nsiz,2)==0) nsiz = nsiz + 1
          do p = pf, pl
             do l = lf, ll
                do k = kf, kl
                   do n = nf(ip), nl(ip)
                      do m = mf(ip), ml(ip)
                         nm = is + (p - pf)*msiz*nsiz*(kl-kf+1)*(ll-lf+1) + (l - lf)*msiz*nsiz*(kl-kf+1) + (k - kf)*msiz*nsiz + (m - iarrc(1,ip))*nsiz + (n - iarrc(3,ip)) + 1
                         ouparr(n, m, k, l, p) = tmp(nm)
                      enddo
                   enddo
                enddo
             enddo
          enddo
          is = is + msiz*nsiz*(kl-kf+1)*(ll-lf+1)*(pl-pf+1)
       enddo
    endif
    deallocate(tmp)
#ifdef HAVE_MPI
call mpi_barrier(engine_comm_world, ierr)
#endif
end subroutine dfgather_R5e_hp2hp
!
!
!===============================================================================
subroutine dfgather_I2_seq(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
integer, dimension(:,:)         , allocatable, intent(inout) :: ouparr
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
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl))
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n,m) = inparr(n+noff,m+moff)
   enddo
enddo
end subroutine dfgather_I2_seq
!
!
!===============================================================================
subroutine dfgather_I3_seq(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
integer, dimension(:,:,:)       , allocatable, intent(inout) :: ouparr
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
kf = lbound(inparr,3)
kl = ubound(inparr,3)
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl))
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n,m,k) = inparr(n+noff,m+moff,k)
      enddo
   enddo
enddo
end subroutine dfgather_I3_seq
!
!===============================================================================
subroutine dfgather_R2e_seq_sp2sp(inparr, ouparr, noff, moff, nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(sp), dimension(:,:)        , allocatable, intent(inout) :: ouparr
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
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl))
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n,m) = inparr(n+noff,m+moff)
   enddo
enddo
end subroutine dfgather_R2e_seq_sp2sp
!
!
!===============================================================================
subroutine dfgather_R2e_seq_hp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(sp), dimension(:,:)        , allocatable, intent(inout) :: ouparr
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
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl))
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n,m) = real(inparr(n+noff,m+moff),sp)
   enddo
enddo
end subroutine dfgather_R2e_seq_hp2sp
!
!
!===============================================================================
subroutine dfgather_R2e_seq_hp2hp(inparr, ouparr, noff, moff, nmaxgl,mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(hp), dimension(:,:)        , allocatable, intent(inout) :: ouparr
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
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl))
do n = 1, nmaxgl
   do m = 1, mmaxgl
      ouparr(n,m) = inparr(n+noff,m+moff)
   enddo
enddo
end subroutine dfgather_R2e_seq_hp2hp
!
!
!===============================================================================
subroutine dfgather_R3e_seq_sp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(sp), dimension(:,:,:)      , allocatable, intent(inout) :: ouparr
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
kf = lbound(inparr,3)
kl = ubound(inparr,3)
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl))
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n,m,k) = inparr(n+noff,m+moff,k)
      enddo
   enddo
enddo
end subroutine dfgather_R3e_seq_sp2sp
!
!
!===============================================================================
subroutine dfgather_R3e_seq_hp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(sp), dimension(:,:,:)      , allocatable, intent(inout) :: ouparr
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
kf = lbound(inparr,3)
kl = ubound(inparr,3)
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl))
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n,m,k) = real(inparr(n+noff,m+moff,k),sp)
      enddo
   enddo
enddo
end subroutine dfgather_R3e_seq_hp2sp
!
!
!===============================================================================
subroutine dfgather_R3e_seq_hp2hp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(hp), dimension(:,:,:)      , allocatable, intent(inout) :: ouparr
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
kf = lbound(inparr,3)
kl = ubound(inparr,3)
if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl))
do k = kf, kl
   do n = 1, nmaxgl
      do m = 1, mmaxgl
         ouparr(n,m,k) = inparr(n+noff,m+moff,k)
      enddo
   enddo
enddo
end subroutine dfgather_R3e_seq_hp2hp
!
!
!===============================================================================
subroutine dfgather_R4e_seq_sp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(sp), dimension(:,:,:,:)    , allocatable, intent(inout) :: ouparr
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
kf = lbound(inparr,3)
kl = ubound(inparr,3)
lf = lbound(inparr,4)
ll = ubound(inparr,4)

if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl,lf:ll))
do l = lf, ll
   do k = kf, kl
      do n = 1, nmaxgl
         do m = 1, mmaxgl
            ouparr(n,m,k,l) = inparr(n+noff,m+moff,k,l)
         enddo
      enddo
   enddo
enddo
end subroutine dfgather_R4e_seq_sp2sp
!
!
!===============================================================================
subroutine dfgather_R4e_seq_hp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(sp), dimension(:,:,:,:)    , allocatable, intent(inout) :: ouparr
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
kf = lbound(inparr,3)
kl = ubound(inparr,3)
lf = lbound(inparr,4)
ll = ubound(inparr,4)

if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl,lf:ll))
do l = lf, ll
   do k = kf, kl
      do n = 1, nmaxgl
         do m = 1, mmaxgl
            ouparr(n,m,k,l) = real(inparr(n+noff,m+moff,k,l),sp)
         enddo
      enddo
   enddo
enddo
end subroutine dfgather_R4e_seq_hp2sp
!
!
!===============================================================================
subroutine dfgather_R4e_seq_hp2hp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(hp), dimension(:,:,:,:)    , allocatable, intent(inout) :: ouparr
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
kf = lbound(inparr,3)
kl = ubound(inparr,3)
lf = lbound(inparr,4)
ll = ubound(inparr,4)

if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl,lf:ll))
do l = lf, ll
   do k = kf, kl
      do n = 1, nmaxgl
         do m = 1, mmaxgl
            ouparr(n,m,k,l) = inparr(n+noff,m+moff,k,l)
         enddo
      enddo
   enddo
enddo
end subroutine dfgather_R4e_seq_hp2hp
!
!
!===============================================================================
subroutine dfgather_R5e_seq_sp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(sp), dimension(:,:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer                                        , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                        , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                        , intent(in)    :: nmaxgl
integer                                        , intent(in)    :: mmaxgl
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
integer                                      :: p
integer                                      :: pf
integer                                      :: pl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(inparr,3)
kl = ubound(inparr,3)
lf = lbound(inparr,4)
ll = ubound(inparr,4)
pf = lbound(inparr,5)
pl = ubound(inparr,5) 

if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl,lf:ll,pf:pl))
do p = pf, pl
   do l = lf, ll
      do k = kf, kl
         do n = 1, nmaxgl
            do m = 1, mmaxgl
               ouparr(n,m,k,l,p) = inparr(n+noff,m+moff,k,l,p)
            enddo
         enddo
      enddo
   enddo
enddo
end subroutine dfgather_R5e_seq_sp2sp
!
!
!===============================================================================
subroutine dfgather_R5e_seq_hp2sp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(hp), dimension(:,:,:,:,:)                 , intent(in)    :: inparr
real(sp), dimension(:,:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer                                        , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                        , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                        , intent(in)    :: nmaxgl
integer                                        , intent(in)    :: mmaxgl
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
integer                                      :: p
integer                                      :: pf
integer                                      :: pl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(inparr,3)
kl = ubound(inparr,3)
lf = lbound(inparr,4)
ll = ubound(inparr,4)
pf = lbound(inparr,5)
pl = ubound(inparr,5)

if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl,lf:ll,pf:pl))
do p = pf, pl
   do l = lf, ll
      do k = kf, kl
         do n = 1, nmaxgl
            do m = 1, mmaxgl
               ouparr(n,m,k,l,p) = real(inparr(n+noff,m+moff,k,l,p),sp)
            enddo
         enddo
      enddo
   enddo
enddo
end subroutine dfgather_R5e_seq_hp2sp
!
!
!===============================================================================
subroutine dfgather_R5e_seq_hp2hp(inparr, ouparr, noff, moff, nmaxgl, mmaxgl)
!!--description-----------------------------------------------------------------
!
!    Function:    Gather array to master (sequential mode)
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
real(hp), dimension(:,:,:,:,:)                 , intent(in)    :: inparr
real(hp), dimension(:,:,:,:,:)    , allocatable, intent(inout) :: ouparr
integer                                        , intent(in)    :: noff       ! desired offset w.r.t. ouparr
integer                                        , intent(in)    :: moff       ! desired offset w.r.t. ouparr
integer                                        , intent(in)    :: nmaxgl
integer                                        , intent(in)    :: mmaxgl
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
integer                                      :: p
integer                                      :: pf
integer                                      :: pl
!
!! executable statements -------------------------------------------------------
!
kf = lbound(inparr,3)
kl = ubound(inparr,3)
lf = lbound(inparr,4)
ll = ubound(inparr,4)
pf = lbound(inparr,5)
pl = ubound(inparr,5)

if (allocated(ouparr)) deallocate(ouparr)
allocate(ouparr(1:nmaxgl,1:mmaxgl,kf:kl,lf:ll,pf:pl))
do p = pf, pl
   do l = lf, ll
      do k = kf, kl
         do n = 1, nmaxgl
            do m = 1, mmaxgl
               ouparr(n,m,k,l,p) = inparr(n+noff,m+moff,k,l,p)
            enddo
         enddo
      enddo
   enddo
enddo
end subroutine dfgather_R5e_seq_hp2hp

end module dfgather_module
