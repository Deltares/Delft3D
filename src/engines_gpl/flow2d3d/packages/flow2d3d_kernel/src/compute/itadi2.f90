subroutine itadi2(lundia    ,s1        ,kcs       ,mmax      ,nmax      , &
                & j         ,nmmaxj    ,nmmax     ,a         ,b         , &
                & c         ,d         ,e         ,f         ,iter      , &
                & itmax     ,smax      ,wrka1     ,wrka2     ,wrka3     , &
                & gdp       )
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
!    Function: ITADI2 solves the linearized system of equations
!              by an adi-type iteration process
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                    :: iter
    integer                      , intent(in)  :: itmax
    integer                                    :: j      !!  Begin pointer for arrays which have
                                                         !!  been transformed into 1D arrays.
                                                         !!  Due to the shift in the 2nd (M-)
                                                         !!  index, J = -2*NMAX + 1
    integer                                    :: lundia !  Description and declaration in inout.igs
    integer                                    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                    :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                    :: nmmax  !  Description and declaration in dimens.igs
    integer                                    :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                   :: smax   !  Description and declaration in f0isf1.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: a
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: b
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: d
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: e
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: f
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: wrka1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: wrka2  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: wrka3  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: icx
    integer  :: icy
    integer  :: itinn
    integer  :: nm
    real(fp) :: dif
!
!! executable statements -------------------------------------------------------
!
    ! start iteration process
    !
    itinn = 0
  200 continue
    itinn = itinn + 1
    iter  = iter + 1
    !
    !     wrka1 contains water elevation at old time level
    !
    do nm = 1, nmmax
       wrka1(nm) = s1(nm)
    enddo
    !
    ! computations for x-direction
    !
    icx = nmax + 2*gdp%d%ddbound
    icy = 1
    call trid2(lundia    ,s1        ,kcs       ,mmax      ,nmax      , &
             & nmmax     ,icx       ,icy       ,j         ,nmmaxj    , &
             & a         ,b         ,c         ,d         ,e         , &
             & f         ,wrka2     ,wrka3     ,gdp       )
    !
    ! computations for y-direction
    !
    icx = 1
    icy = nmax + 2*gdp%d%ddbound
    call trid2(lundia    ,s1        ,kcs       ,nmax      ,mmax      , &
             & nmmax     ,icx       ,icy       ,j         ,nmmaxj    , &
             & e         ,b         ,f         ,d         ,a         , &
             & c         ,wrka2     ,wrka3     ,gdp       )
    !
    ! compute residual
    smax = 0.0
    do nm = 1, nmmax
       dif = abs(wrka1(nm) - s1(nm))
       if (dif>smax .and. kcs(nm)==1) then
          smax = dif
       endif
    enddo
    !
    ! convergence criterium
    !
    if (smax>1e-5 .and. itinn<itmax) goto 200
end subroutine itadi2
