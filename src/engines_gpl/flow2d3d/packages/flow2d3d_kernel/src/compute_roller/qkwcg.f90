subroutine qkwcg(tp        ,rlabda    ,teta      ,qxkw      ,qykw      , &
               & qxkr      ,qykr      ,dps       ,s         ,kfs       , &
               & guu       ,gvv       ,cgc       ,c         ,irocol    , &
               & norow     ,nocol     ,kfu       ,kfv       ,nmax      , &
               & mmax      ,gdp       )
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
! calculation of transport velocities for transport
! of wave energy in U- and V-points
! See "Extension of SURFBEAT model to two dimensions" by H. Petit
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'flow_steps_f.inc'
    real(fp)               , pointer :: dryflc
    real(fp)               , pointer :: ag
    integer                , pointer :: iro
!
! Global variables
!
    integer                                                     , intent(in)    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                                                     :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, *)                                                    :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)    :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out)  :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: cgc    !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)   :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)   :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: qxkr   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: qxkw   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: qykr   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               :: qykw   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)   :: rlabda !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)   :: s
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)   :: teta   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)   :: tp     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: icx
    integer :: icy
    integer :: m
    integer :: n
    integer :: nhystp
    integer :: nmaxddb
    integer :: iopt
    real(fp):: cf
    real(fp):: cg     ! Group velocity of the waves
    real(fp):: h      ! Total water depth in zeta point
    real(fp):: q
    real(fp):: sigma
!
!! executable statements -------------------------------------------------------
!
    dryflc    => gdp%gdnumeco%dryflc
    ag        => gdp%gdphysco%ag
    iro       => gdp%gdphysco%iro
    !
    do n = 1, nmax
       do m = 1, mmax
          !
          qxkw(n, m) = 0.0
          qykw(n, m) = 0.0
          qxkr(n, m) = 0.0
          qykr(n, m) = 0.0
          cgc(n, m)  = 0.0
          c(n, m)    = 0.0
          !
          if (kfs(n, m)>0) then
             !
             h = max(real(dps(n, m),fp) + s(n, m), dryflc)
             !
             ! Compute cf anf cg
             !
             iopt=1
             !
             if (tp(n, m)>0.01_fp) then
                 !
                 if (iopt==2) then
                   !
                   q = 2.0_fp*pi*h/max(5.0_fp,rlabda(n, m)) ! k*h
                   !
                   if (q<pi/10.0_fp) then ! Shallow water
                      cf = sqrt(ag*h)
                      cg = cf
                   elseif (q>pi/10.0_fp .and. q<pi) then ! Transitional water
                      cf = ((ag*tp(n, m))/(2.0_fp*pi))*tanh(q)
                      cg = ((cf/2.0_fp)*(1.0 + ((2.0_fp*q)/sinh(2.0_fp*q))))
                   else ! Deep water
                      cf = (( ag * tp(n, m) ) / ( 2.0_fp * pi ) )
                      cg = cf/2.0_fp
                   endif             
                else
                   !
                   if (tp(n, m)>0.1_fp) then
                      cf = rlabda(n, m)/tp(n, m)
                   else
                      cf = rlabda(n, m)
                   endif
                   !             
                   q          = 2.0_fp*pi*h/max(0.1_fp,rlabda(n, m))
                   sigma      = tanh(q)
                   cg         = ag*tp(n, m)*(sigma + (1.0_fp - sigma*sigma)*q)/4.0_fp/pi
                   cg         = min(cg, cf)
                   !
                endif
             else
                cf = sqrt(ag*h)
                cg = cf
             endif
             !
             c(n, m)    = cf
             qxkr(n, m) = cf*cos(teta(n, m)*degrad)
             qykr(n, m) = cf*sin(teta(n, m)*degrad)
             qxkw(n, m) = cg*cos(teta(n, m)*degrad)
             qykw(n, m) = cg*sin(teta(n, m)*degrad)
             cgc(n, m)  = cg/max(0.01_fp,cf)
             !
          endif
       enddo
    enddo
    !
    nhystp = nxtstp(d3dflow_roller_UV, gdp)
    !
    do n = 1, nmax
       do m = 1, mmax - 1
          if (kfu(n, m)>0) then
             ! Normal active u point
             qxkw(n, m) = 0.5*(qxkw(n, m) + qxkw(n, m + 1))*guu(n, m)
             qxkr(n, m) = 0.5*(qxkr(n, m) + qxkr(n, m + 1))*guu(n, m)
          elseif (kfs(n, m) > 0 .and. kfs(n, m + 1) > 0) then
             ! Inactive velocity point (thin dam)
             if (qxkw(n, m) > 0.0 .and. qxkw(n, m + 1) > 0.0) then
                ! Use upwind approach in case of thin dams
                qxkw(n, m) = qxkw(n, m)*guu(n, m)
                qxkr(n, m) = qxkr(n, m)*guu(n, m)
             elseif (qxkw(n, m) < 0.0 .and. qxkw(n, m + 1) < 0.0) then
                ! Use upwind approach in case of thin dams
                qxkw(n, m) = qxkw(n, m + 1)*guu(n, m) 
                qxkr(n, m) = qxkr(n, m + 1)*guu(n, m)
             else
                qxkw(n, m) = 0.0 
                qxkr(n, m) = 0.0
             endif
          elseif (kfs(n, m) > 0.0 .and. kfv(n, m + 1) == 0) then
             ! Next cell to the right is dry 
             qxkw(n, m) = qxkw(n, m)*guu(n, m)
             qxkr(n, m) = qxkr(n, m)*guu(n, m)
          elseif (kfs(n, m) == 0 .and. kfs(n, m + 1) > 0) then
             ! This cell is dry
             qxkw(n, m) = qxkw(n, m + 1)*guu(n, m)
             qxkr(n, m) = qxkr(n, m + 1)*guu(n, m)
          else
              ! This should never happen
             qxkw(n, m) = 0.0 
             qxkr(n, m) = 0.0
          endif
       enddo
    enddo
    do n = 1, nmax - 1
       do m = 1, mmax
          if (kfv(n, m)>0) then
             ! Normal active v point
             qykw(n, m) = 0.5*(qykw(n, m) + qykw(n + 1, m))*gvv(n, m)
             qykr(n, m) = 0.5*(qykr(n, m) + qykr(n + 1, m))*gvv(n, m)
          elseif (kfs(n, m) > 0 .and. kfs(n + 1, m) > 0) then
             ! Inactive velocity point (thin dam)
             if (qykw(n, m) > 0.0 .and. qykw(n + 1, m) > 0.0) then
                ! Use upwind approach in case of thin dams
                qykw(n, m) = qykw(n, m)*gvv(n, m)
                qykr(n, m) = qykr(n, m)*gvv(n, m)
             elseif (qykw(n, m) < 0.0 .and. qykw(n + 1, m) < 0.0) then
                ! Use upwind approach in case of thin dams
                qykw(n, m) = qykw(n + 1, m)*gvv(n, m) 
                qykr(n, m) = qykr(n + 1, m)*gvv(n, m)
             else
                qykw(n, m) = 0.0 
                qykr(n, m) = 0.0
             endif
          elseif (kfs(n, m) > 0.0 .and. kfv(n + 1, m) == 0) then
             ! Next cell up is dry 
             qykw(n, m) = qykw(n, m)*gvv(n, m)
             qykr(n, m) = qykr(n, m)*gvv(n, m)
          elseif (kfs(n, m) == 0 .and. kfs(n + 1, m) > 0) then
             ! This cell is dry
             qykw(n, m) = qykw(n + 1, m)*gvv(n, m)
             qykr(n, m) = qykr(n + 1, m)*gvv(n, m)
          else
              ! This should never happen
              qykw(n, m) = 0.0 
              qykr(n, m) = 0.0
          endif
       enddo
    enddo


    nmaxddb = nmax + 2*gdp%d%ddbound
    icx     = nmaxddb
    icy     = 1
    call qkwbn(qxkw      ,cgc       ,kfu       ,irocol(1, 1)         ,norow     , &
             & icx       ,icy       ,gdp       )
    icx = 1
    icy = nmaxddb
    call qkwbn(qykw      ,cgc       ,kfv       ,irocol(1, norow + 1) ,nocol     , &
             & icx       ,icy       ,gdp       )
    icx = nmaxddb
    icy = 1
    call qkwbn(qxkr      ,cgc       ,kfu       ,irocol(1, 1)         ,norow     , &
             & icx       ,icy       ,gdp       )
    icx = 1
    icy = nmaxddb
    call qkwbn(qykr      ,cgc       ,kfv       ,irocol(1, norow + 1) ,nocol     , &
             & icx       ,icy       ,gdp       )
end subroutine qkwcg
