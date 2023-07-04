!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------
! 
! 
module m_advection

public :: advec

contains
    
!> calculate_advection, based on u0, q0 24
subroutine calculate_advection()
   use m_flowtimes
   use m_flowgeom
   use m_flow
   use m_fixedweirs
   use m_sferic
   use unstruc_channel_flow, only: network

   implicit none
   
   integer, parameter  :: SEMI_SUBGRID      = 21
   integer, parameter  :: ADVECTION_CORRECTION = 2
   integer, parameter  :: NO_RHO_EFFECTS_IN_MOMENTUM = 0

   integer                           :: link, k1, k2      ! link, nd1, nd2
   double precision                  :: v12t ! time derivative of control volume (m3/s)
   double precision                  :: advel          ! local adve

   double precision                  :: qu1            ! Flux times advection velocity node 1 (m4/s2)
   double precision                  :: qu2            ! idem                          node 2
   double precision                  :: cs, sn

   double precision, external        :: QucWen         ! Sum over links of Flux times upwind cell centre velocity (m4/s2), do not include own link
   double precision, external        :: QucPer         ! idem, include own link
 double precision, external        :: QucPerpure1D   ! idem, include own link
 double precision, external        :: QucPeri        ! idem, inly incoming         nb: QucPeripiaczek is a subroutine

 double precision, external        :: QucPerq1       ! ..
 double precision, external        :: QucPercu       ! testing center differences
 double precision, external        :: QufPer         ! testing adv of face velocities instead of centre upwind velocities

 integer                           :: isg, iadvL
 integer                           :: iad, n, kk, kb
 double precision                  :: ucxku, ucyku, ai, ae, abh, volu, volui, huvL, baik1, baik2
 double precision                  :: vol_k1        !< representative volume for node k1
 double precision                  :: vol_k2        !< representative volume for node k2
 double precision                  :: ucin, fdx, ql, ac1, ac2, uqn, qn, dzss, qnn
 integer                           :: LL, Lb, Lt, i

 integer                           :: ku, kd, k, nfw, kt
 integer                           :: n12

 double precision                  :: quk1(3,kmxx), quk2(3,kmxx), volukk(kmxx)   ! 3D for 1=u, 2=turkin, 3=tureps

 integer                           :: kt1, kt2, n1, n2, kb1, kb2, Ltx0, ktx01, ktx02 , ktx1 , ktx2, Ltx, L1, ksb, kst
 double precision                  :: hs1, hs2, vo1, vo2

 double precision                  :: quuk1(0:kmxx), quuk2(0:kmxx), volk1(0:kmxx), volk2(0:kmxx), sqak1(0:kmxx), sqak2(0:kmxx)
 double precision                  :: quuL1(0:kmxx), quuL2(0:kmxx), volL1(0:kmxx), volL2(0:kmxx), sqaL1(0:kmxx), sqaL2(0:kmxx)
 double precision                  :: sigk1(0:kmxx), sigk2(0:kmxx), siguL(0:kmxx)

 double precision,        external :: lin2nodx, lin2nody
 double precision,        external :: nod2linx, nod2liny
 double precision,        external :: dslim

 if (ifixedweirscheme >= 3 .and. ifixedweirscheme <= 5) then
    call set_ucx_ucy_for_weirs_at_semi_subgrid()
 end if

 if (jabarrieradvection == ADVECTION_CORRECTION) then
     call set_ucx_ucy_for_gates_signals()
     call set_ucx_ucy_for_gates_in_structures()
 end if

 call sethigherorderadvectionvelocities()

 uqcx(:) = 0d0
 uqcy(:) = 0d0
 sqa (:) = 0d0

 if (kmx == 0) then

   if (jasfer3d == 1) then

      do link = Lnx,1,-1
          k1 = ln(1,link)
          k2 = ln(2,link)
          qL = qa(link)
          uqcx(k1) = uqcx(k1) + qL*lin2nodx(link,1,ucxu(link),ucyu(link))
          uqcx(k2) = uqcx(k2) - qL*lin2nodx(link,2,ucxu(link),ucyu(link))
          uqcy(k1) = uqcy(k1) + qL*lin2nody(link,1,ucxu(link),ucyu(link))
          uqcy(k2) = uqcy(k2) - qL*lin2nody(link,2,ucxu(link),ucyu(link))
          sqa (k1) = sqa (k1) + ql
          sqa (k2) = sqa (k2) - ql
       end do

   else

       do link = Lnx,1,-1
          k1 = ln(1,link)
          k2 = ln(2,link)
          qL = qa(link)
          uqcx(k1) = uqcx(k1) + qL*ucxu(link)
          uqcx(k2) = uqcx(k2) - qL*ucxu(link)
          uqcy(k1) = uqcy(k1) + qL*ucyu(link)
          uqcy(k2) = uqcy(k2) - qL*ucyu(link)
          sqa (k1) = sqa (k1) + ql
          sqa (k2) = sqa (k2) - ql
       end do

    end if

 else

    do LL = Lnx,1,-1
       Lb = lbot(LL) ; Lt = ltop(LL)
       do link = Lb, Lt
          k1 = ln(1,link) 
          k2 = ln(2,link) 
          qL = qa(link)
          if (jasfer3d == 1) then
             uqcx(k1) = uqcx(k1) + qL*lin2nodx(LL,1,ucxu(link),ucyu(link))
             uqcx(k2) = uqcx(k2) - qL*lin2nodx(LL,2,ucxu(link),ucyu(link))
             uqcy(k1) = uqcy(k1) + qL*lin2nody(LL,1,ucxu(link),ucyu(link))
             uqcy(k2) = uqcy(k2) - qL*lin2nody(LL,2,ucxu(link),ucyu(link))
          else
             uqcx(k1) = uqcx(k1) + qL*ucxu(link)
             uqcx(k2) = uqcx(k2) - qL*ucxu(link)
             uqcy(k1) = uqcy(k1) + qL*ucyu(link)
             uqcy(k2) = uqcy(k2) - qL*ucyu(link)
          end if
          sqa (k1) = sqa (k1) + ql
          sqa (k2) = sqa (k2) - ql
       end do
    end do

 end if

 if (javau >= 6) then ! 3D checkerboard pepare explicit node based vertical advection
    if (jarhoxu == NO_RHO_EFFECTS_IN_MOMENTUM) then
       call set_uqcx_uqcy_sqa_without_rho_effects()
    else
       call set_uqcx_uqcy_sqa_with_rho_effects()
    end if
 end if

 if (jarhoxu > 0) then
    sqa(:) = sqa(:) * rho(:)
 end if

 call set_uqcx_uqcy_sqa_for_sources()
 
 nfw = 0

 if (kmx == 0) then
    call calculate_advection_for_2D()     
 else                                                      ! Plus vertical
    call calculate_advection_for_3D() 
 end if

 if (kmx == 0 .and. lnx1D > 0) then
    call setucxy1D()
 end if

contains

!> set_ucx_ucy_for_weirs_at_semi_subgrid
subroutine set_ucx_ucy_for_weirs_at_semi_subgrid()

    do link  = 1, lnxi
       if (iadv(link) == SEMI_SUBGRID) then
          call getucxucyweironly ( ln(2,link), ucx(ln(2,link)), ucy(ln(2,link)), ifixedweirscheme )
          call getucxucyweironly ( ln(1,link), ucx(ln(1,link)), ucy(ln(1,link)), ifixedweirscheme )
       end if
    end do
    
end subroutine set_ucx_ucy_for_weirs_at_semi_subgrid

!> set_ucx_ucy_for_gates_signals
subroutine set_ucx_ucy_for_gates_signals()
   
   integer             :: gate

    do gate = 1, ngatesg
        call set_ucx_ucy_for_barrier(gate, L1gatesg, L2gatesg, kgate)
    end do
    
end subroutine set_ucx_ucy_for_gates_signals

!> set_ucx_ucy_for_barrier
subroutine set_ucx_ucy_for_barrier(barrier, first_points, second_points, links)

    integer, intent(in) :: barrier
    integer, intent(in) :: first_points(:)
    integer, intent(in) :: second_points(:)
    integer, intent(in) :: links(:,:)

    integer             :: point

    do point = first_points(barrier), second_points(barrier)
        link = abs(links(3, point))
        call getucxucybarrierzero ( link, ln(1,link), ucx(ln(1,link)), ucy(ln(1,link)) )
        call getucxucybarrierzero ( link, ln(2,link), ucx(ln(2,link)), ucy(ln(2,link)) )
   end do

end subroutine set_ucx_ucy_for_barrier

!> set_ucx_ucy_for_gates_in_structures
subroutine set_ucx_ucy_for_gates_in_structures()

   integer             :: gate
   integer             :: general_structure

    do gate = 1, ngategen
       general_structure = gate2cgen(gate)
       call set_ucx_ucy_for_barrier(general_structure, L1cgensg, L2cgensg, kcgen)
    end do
    
end subroutine set_ucx_ucy_for_gates_in_structures

!> set_uqcx_uqcy_sqa_without_rho_effects
subroutine set_uqcx_uqcy_sqa_without_rho_effects()

   double precision, parameter :: TOLERANCE = 1d-4 
   double precision            :: sl, dzu, dzk, du1, du2, dux, duy

   do kk = 1, ndxi
      call getkbotktop(kk,kb,kt)
      do k = kb, kt-1
         if ( qw(k) > 0d0) then
             uqcx(k+1) = uqcx(k+1) - qw(k) * ucx(k)
             uqcx(k  ) = uqcx(k  ) + qw(k) * ucx(k)
             uqcy(k+1) = uqcy(k+1) - qw(k) * ucy(k)
             uqcy(k  ) = uqcy(k  ) + qw(k) * ucy(k)
             if (javau == 7 .and. k > kb ) then
                dzu =  zws(k) - zws(k-2)      ! 2*dz of upwind face
                if ( dzu > TOLERANCE) then
                   dzk =  zws(k+1) - zws(k-1) ! 2*dz of this face
                   sl  =  dzk/dzu
                   du2 = (ucx(k+1) - ucx(k)   )
                   du1 = (ucx(k )  - ucx(k-1) ) * sl
                   dux =  0.5d0 * dslim(du1,du2,4)
                   du2 = (ucy(k+1) - ucy(k)   )
                   du1 = (ucy(k )  - ucy(k-1) ) * sl
                   duy =  0.5d0 * dslim(du1,du2,4)
                   uqcx(k+1) = uqcx(k+1) - qw(k) * dux
                   uqcx(k  ) = uqcx(k  ) + qw(k) * dux
                   uqcy(k+1) = uqcy(k+1) - qw(k) * duy
                   uqcy(k  ) = uqcy(k  ) + qw(k) * duy
                end if
             end if
         else if ( qw(k) < 0d0) then
             uqcx(k+1) = uqcx(k+1) - qw(k) * ucx(k+1)
             uqcx(k  ) = uqcx(k  ) + qw(k) * ucx(k+1)
             uqcy(k+1) = uqcy(k+1) - qw(k) * ucy(k+1)
             uqcy(k  ) = uqcy(k  ) + qw(k) * ucy(k+1)
             if (javau == 7 .and. k < kt-1 ) then
                dzu =  zws(k+2) - zws(k)      ! 2*dz of upwind face
                if ( dzu > TOLERANCE) then
                   dzk =  zws(k+1) - zws(k-1) ! 2*dz of this face
                   sl  =  dzk/dzu
                   du2 = (ucx(k)   - ucx(k+1) )
                   du1 = (ucx(k+1) - ucx(k+2) ) * sl
                   dux =  0.5d0*dslim(du1,du2,4)
                   du2 = (ucy(k)   - ucy(k+1) )
                   du1 = (ucy(k+1) - ucy(k+2) ) * sl
                   duy =  0.5d0*dslim(du1,du2,4)
                   uqcx(k+1) = uqcx(k+1) - qw(k) * dux
                   uqcx(k  ) = uqcx(k  ) + qw(k) * dux
                   uqcy(k+1) = uqcy(k+1) - qw(k) * duy
                   uqcy(k  ) = uqcy(k  ) + qw(k) * duy
                end if
             end if

         end if
         sqa(k+1) = sqa(k+1) - qw(k)
         sqa(k  ) = sqa(k  ) + qw(k)
      end do
   end do

end subroutine set_uqcx_uqcy_sqa_without_rho_effects

!> set_uqcx_uqcy_sqa_with_rho_effects
subroutine set_uqcx_uqcy_sqa_with_rho_effects()

   do kk = 1, ndxi
      do k = kbot(kk), ktop(kk)-1
         if ( qw(k) > 0d0) then
             uqcx(k+1) = uqcx(k+1) - qw(k) * ucx(k) * rho(k)
             uqcx(k  ) = uqcx(k  ) + qw(k) * ucx(k) * rho(k)
             uqcy(k+1) = uqcy(k+1) - qw(k) * ucy(k) * rho(k)
             uqcy(k  ) = uqcy(k  ) + qw(k) * ucy(k) * rho(k)
         else if ( qw(k) < 0d0) then
             uqcx(k+1) = uqcx(k+1) - qw(k) * ucx(k+1) * rho(k+1)
             uqcx(k  ) = uqcx(k  ) + qw(k) * ucx(k+1) * rho(k+1)
             uqcy(k+1) = uqcy(k+1) - qw(k) * ucy(k+1) * rho(k+1)
             uqcy(k  ) = uqcy(k  ) + qw(k) * ucy(k+1) * rho(k+1)
        end if
        sqa(k+1) = sqa(k+1) - qw(k)
        sqa(k  ) = sqa(k  ) + qw(k)
      end do
   end do

end subroutine set_uqcx_uqcy_sqa_with_rho_effects

!> set_uqcx_uqcy_sqa_for_sources
subroutine set_uqcx_uqcy_sqa_for_sources()

   do n  = 1, numsrc                             ! momentum
      if (arsrc(n) > 0) then                    ! if momentum desired
         call set_kk_ksb_kst()
         if (kk > 0 .and. ksb > 0) then

            qnn = qsrc(n)
            do k  = ksb,kst
               qn = qnn
               if (kmx > 0) then
                  dzss  = zws(kst) - zws(ksb-1)
                  if (dzss > epshs) then
                     qn = qnn*( zws(k) - zws(k-1) ) / dzss
                  else
                     qn = qnn / (kst - ksb + 1)
                  end if
               end if
               uqn = qn*qnn / arsrc(n)

               if (jarhoxu > 0) then
                  qn  = qn  * rhomean
                  uqn = uqn * rhomean
               end if

               if (qsrc(n) > 0) then               ! from 1 to 2
                  uqcx(k) = uqcx(k) - uqn*cssrc(2,n)
                  uqcy(k) = uqcy(k) - uqn*snsrc(2,n)
                  sqa(k)  = sqa(k)  - qn           ! sqa : out - in
               else                                ! from 2 to 1
                  uqcx(k) = uqcx(k) + uqn*cssrc(1,n)
                  uqcy(k) = uqcy(k) + uqn*snsrc(1,n)
                  sqa(k)  = sqa(k)  + qn           ! sqa : out - in
               end if

            end do
         end if
      end if
  end do

end subroutine set_uqcx_uqcy_sqa_for_sources

!> calculate_advection_for_2D
subroutine calculate_advection_for_2D()

 !$OMP PARALLEL DO                                                                   &
 !$OMP PRIVATE(link, advel,k1,k2,iadvL,qu1,qu2,volu,ai,ae,iad,volui,abh,v12t,ku,kd,isg,n12, ucxku, ucyku, ucin, fdx, vol_k1, vol_k2)

    do link  = 1,lnx

        advel = 0                                          !  advi (1/s), adve (m/s2)

        if ( hu(link) <= 0 ) cycle

        k1    = ln(1,link)
        k2    = ln(2,link)
        iadvL = iadv(link)

        if (link > lnxi) then
           if (iadvL == 77) then
              if (u0(link) < 0) iadvL = 0
           else if (u0(link) > 0) then
              iadvL = 0                                 ! switch off advection for inflowing waterlevel bnd's, if not normalvelocitybnds
           end if
        end if

        select case(iadvL)
        case(33)
            call calculate_advection_using_scheme_33()
        case(44)
            call calculate_advection_using_scheme_44()
        case(3)
            call calculate_advection_using_scheme_3()
        case(103)
            call calculate_advection_using_scheme_103()
        case(333)
            call calculate_advection_using_scheme_333()
        case(30)
            call calculate_advection_using_scheme_30()
        case(31)
            call calculate_advection_using_scheme_31()
        case(40)
            call calculate_advection_using_scheme_40()
        case(1)
            call calculate_advection_using_scheme_1()
        case(2)
            call calculate_advection_using_scheme_2()
        case(4)
            call calculate_advection_using_scheme_4()
        case(5 : 6) 
            call calculate_advection_using_schemes_5_6()
        case(7 : 12) 
            call calculate_advection_using_schemes_7_till_12()
        case(21)
            call calculate_advection_using_scheme_21()
        case(77)
            call calculate_advection_using_scheme_77()
        case(38) 
            call calculate_advection_using_scheme_38()
        case(34) 
            call calculate_advection_using_scheme_34()
        case(35)
            call calculate_advection_using_scheme_35()
        case(36) 
            call calculate_advection_using_scheme_36()
        case(37) 
           call calculate_advection_using_scheme_37()
        end select

        adve(link) = adve(link) + advel

    end do

 !$OMP END PARALLEL DO

end subroutine calculate_advection_for_2D


!> set_kk_ksb_kst
subroutine set_kk_ksb_kst()

    if (qsrc(n) > 0) then
       kk  = ksrc(4,n)                     ! 2D pressure cell nr TO
       ksb = ksrc(5,n)                     ! cell nr
       kst = ksrc(6,n)                     ! cell nr
    else
       kk  = ksrc(1,n)                     ! 2D pressure cell nr FROM
       ksb = ksrc(2,n)                     ! cell nr
       kst = ksrc(3,n)                     ! cell nr
    end if

end subroutine set_kk_ksb_kst

!> calculate_advection_using_scheme_33
subroutine calculate_advection_using_scheme_33()

    if (jasfer3d == 1) then
       qu1   = csu(link)*nod2linx(link,1,uqcx(k1),uqcy(k1)) + snu(link)*nod2liny(link,1,uqcx(k1),uqcy(k1)) - u1(link)*sqa(k1)
       qu2   = csu(link)*nod2linx(link,2,uqcx(k2),uqcy(k2)) + snu(link)*nod2liny(link,2,uqcx(k2),uqcy(k2)) - u1(link)*sqa(k2)
    else
       qu1   = csu(link)*uqcx(k1) + snu(link)*uqcy(k1) - u1(link)*sqa(k1)
       qu2   = csu(link)*uqcx(k2) + snu(link)*uqcy(k2) - u1(link)*sqa(k2)
    end if

    if (jarhoxu == NO_RHO_EFFECTS_IN_MOMENTUM) then
       if (kcu(link) == 1) then
          volu  = acl(link)*vol1_f(k1) + (1d0-acl(link))*vol1_f(k2)
       else
          volu  = acl(link)*vol1(k1) + (1d0-acl(link))*vol1(k2)
       end if
    else
       if (kcu(link) == 1) then
          volu  = acl(link)*vol1_f(k1)*rho(k1) + (1d0-acl(link))*vol1_f(k2)*rho(k2)
       else
          volu  = acl(link)*vol1(k1)*rho(k1) + (1d0-acl(link))*vol1(k2)*rho(k2)
       end if
    end if

    if (volu > 0) then
       advel = (acl(link)*qu1 + (1d0-acl(link))*qu2) / volu
    end if

end subroutine calculate_advection_using_scheme_33
    
!> calculate_advection_using_scheme_44
subroutine calculate_advection_using_scheme_44()

    if (vol1(k1) > 0) then
       if (jasfer3D == 1) then
          qu1   = csu(link)*nod2linx(link,1,uqcx(k1),uqcy(k1)) + snu(link)*nod2liny(link,1,uqcx(k1),uqcy(k1)) - u1(link)*sqa(k1)
       else
          qu1   = csu(link)*uqcx(k1) + snu(link)*uqcy(k1) - u1(link)*sqa(k1)
       end if
       advel = advel + acl(link)*qu1/vol1(k1)
    end if
    if (vol1(k2) > 0) then
       if (jasfer3D == 1) then
          qu2   = csu(link)*nod2linx(link,2,uqcx(k2),uqcy(k2)) + snu(link)*nod2liny(link,2,uqcx(k2),uqcy(k2)) - u1(link)*sqa(k2)
       else
          qu2   = csu(link)*uqcx(k2) + snu(link)*uqcy(k2) - u1(link)*sqa(k2)
       end if
       advel = advel +  (1d0-acl(link))*qu2 / vol1(k2)
    end if
       
end subroutine calculate_advection_using_scheme_44

!> calculate_advection_using_scheme_3
!! explicit first order mom conservative based upon cell center excess advection velocity and Perot control volume
subroutine calculate_advection_using_scheme_3()
    qu1 = 0 
    if (vol1(k1) > 0) then
       qu1 = QucPer(1,link)                          ! excess momentum in/out u(link) dir. from k1
       qu1 = qu1*acl(link)                           ! Perot weigthing
    end if
    qu2 = 0
    if (vol1(k2) > 0) then
       qu2 = QucPer(2,link)                          ! excess momentum in/out u(link) dir. from k2
       qu2 = qu2*(1d0-acl(link))                     ! Perot weigthing
    end if
    volu  = acl(link)*vol1(k1) + (1d0-acl(link))*vol1(k2)
    if (volu > 0) then
       advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
    end if
       
end subroutine calculate_advection_using_scheme_3

!> calculate_advection_using_scheme_103
!! explicit first order mom conservative based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_103()

    qu1 = 0                                       ! and Perot control volume
    qu2 = 0
    if (jaPure1D == 1) then
       vol_k1 = vol1_f(k1)
       vol_k2 = vol1_f(k2)
    else
       vol_k1 = vol1(k1)
       vol_k2 = vol1(k2)
    end if
       
    if (vol_k1 > 0) then
       qu1 = QucPerPure1D(1,link)                    ! excess momentum in/out u(link) dir. from k1
       qu1 = qu1*acl(link)                           ! Perot weigthing
    end if
    if (vol_k2 > 0) then
       qu2 = QucPerPure1D(2,link)                    ! excess momentum in/out u(link) dir. from k2
       qu2 = qu2*(1d0-acl(link))                     ! Perot weigthing
    end if
    volu = acl(link)*vol_k1 + (1d0-acl(link))*vol_k2
    if (volu > 0) then
       advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
    end if

end subroutine calculate_advection_using_scheme_103

!> calculate_advection_using_scheme_333
!! explicit first order mom conservative based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_333()

    qu1 = 0                                       ! and Perot control volume
    if (volau(k1) > 0) then
       qu1 = QucPer(1,link)                          ! excess momentum in/out u(link) dir. from k1
       qu1 = qu1*acl(link)/ volau(k1)                ! Perot weigthing
    end if
    qu2 = 0
    if (volau(k2) > 0) then
       qu2 = QucPer(2,link)                          ! excess momentum in/out u(link) dir. from k2
       qu2 = qu2*(1d0-acl(link))/ volau(k2)                     ! Perot weigthing
    end if
    advel = qu1 + qu2                             ! dimension: ((m4/s2) / m3) =   (m/s2)

end subroutine calculate_advection_using_scheme_333

!> calculate_advection_using_scheme_30
!! Same as 3, now with alfa = 0.5 in volumes and advection based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_30()

    qu1 = 0
    if (vol1(k1) > 0) then
       qu1 = QucPer(1,link)                          ! excess momentum in/out u(link) dir. from k1
    end if
    qu2 = 0
    if (vol1(k2) > 0) then
       qu2 = QucPer(2,link)                          ! excess momentum in/out u(link) dir. from k2
    end if
    volu  = vol1(k1) + vol1(k2)
    if (volu > 0) then
       advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
    end if

end subroutine calculate_advection_using_scheme_30

!> calculate_advection_using_scheme_31
!! Thesis Olga 4.8 based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_31()

    if (jasfer3D == 1) then
       qu1   = csu(link)*nod2linx(link,1,uqcx(k1),uqcy(k1)) + snu(link)*nod2liny(link,1,uqcx(k1),uqcy(k1))
       qu2   = csu(link)*nod2linx(link,2,uqcx(k2),uqcy(k2)) + snu(link)*nod2liny(link,2,uqcx(k2),uqcy(k2))
    else
       qu1   = csu(link)*uqcx(k1) + snu(link)*uqcy(k1)
       qu2   = csu(link)*uqcx(k2) + snu(link)*uqcy(k2)
    end if
    advel = acl(link)*qu1 + (1d0-acl(link))*qu2
       
end subroutine calculate_advection_using_scheme_31

!> calculate_advection_using_scheme_40
subroutine calculate_advection_using_scheme_40()

    if (jasfer3D == 1) then
       qu1   = csu(link)*nod2linx(link,1,uqcx(k1),uqcy(k1)) + snu(link)*nod2liny(link,1,uqcx(k1),uqcy(k1)) - u1(link)*sqa(k1)
       qu2   = csu(link)*nod2linx(link,2,uqcx(k2),uqcy(k2)) + snu(link)*nod2liny(link,2,uqcx(k2),uqcy(k2)) - u1(link)*sqa(k2)
    else
       qu1   = csu(link)*uqcx(k1) + snu(link)*uqcy(k1) - u1(link)*sqa(k1)
       qu2   = csu(link)*uqcx(k2) + snu(link)*uqcy(k2) - u1(link)*sqa(k2)
    end if
    volu  = acl(link)*voldhu(k1) + (1d0-acl(link))*voldhu(k2)

    if (volu > 0) then
       advel = (acl(link)*qu1 + (1d0-acl(link))*qu2) / volu
    end if
       
end subroutine calculate_advection_using_scheme_40

!> calculate_advection_using_scheme_1
!! explicit first order mom conservative based upon cell center advection velocity and Wenneker control volume
!! now with uqcx and uqcy arrays instead of function call, (much faster than excess form)
subroutine calculate_advection_using_scheme_1()

     volu      = vol1(k1) + vol1(k2)              ! Wennekers control volume
                                                  ! qu1     = ( uqcx(k1)*cs + uqcy(k1)*sn )
                                                  ! qu2     = ( uqcx(k2)*cs + uqcy(k2)*sn )
    if (volu   > 0) then
       if (jasfer3D == 1) then
          qu1 = csu(link)*(nod2linx(link,1,uqcx(k1),uqcy(k1)) + nod2linx(link,2,uqcx(k2),uqcy(k2)))
          qu2 = snu(link)*(nod2liny(link,1,uqcx(k1),uqcy(k1)) + nod2liny(link,2,uqcx(k2),uqcy(k2)))
       else
          qu1 = csu(link)*( uqcx(k1) + uqcx(k2) )
          qu2 = snu(link)*( uqcy(k1) + uqcy(k2) )
       end if
       v12t    = sq(k1) + sq(k2)                  ! time der. of v12
       advel   = (qu1 + qu2 + u1(link)*v12t) / volu  ! dimension: ((m4/s2) / m3) =   (m/s2)
    end if
    
end subroutine calculate_advection_using_scheme_1

!> calculate_advection_using_scheme_2
!! explicit first order mom conservative based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_2()

    volu     = vol1(k1) + vol1(k2)                ! Wennekers control volume
    if (volu > 0) then
       qu1   = QucWen(1,link)                        ! excess momentum in u(link) dir. out of k1
       qu2   = QucWen(2,link)                        ! out of k2
       advel = (qu1 + qu2) / volu                 ! dimension: ((m4/s2) / m3) =   (m/s2)
    end if
       
end subroutine calculate_advection_using_scheme_2    

!> calculate_advection_using_scheme_4
!! explicit first order mom conservative
subroutine calculate_advection_using_scheme_4()

    qu1 = 0                                       ! and Perot control volume
    if (vol1(k1) > 0) then
       qu1 = QucPeri(1,link)                         ! excess momentum in u(link) dir. from of k1
       qu1 = qu1*acl(link)                           ! Perot weigthing
    end if
    qu2 = 0
    if (vol1(k2) > 0) then
       qu2 = QucPeri(2,link)                         ! excess momentum in u(link) dir. from of k2
       qu2 = qu2*(1d0-acl(link))                     ! Perot weigthing
    end if

    volu  = acl(link)*vol1(k1) + (1d0-acl(link))*vol1(k2)
    if (volu > 0) then
       advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
    end if

end subroutine calculate_advection_using_scheme_4

!> calculate_advection_using_schemes_5_6
!! 5,6 = advection like 3,4, now Piaczek teta
subroutine calculate_advection_using_schemes_5_6()

    if (kcu(link) ==1) then
       volu = acl(link)*vol1_f(k1) + (1d0-acl(link))*vol1_f(k2)
    else
       volu = acl(link)*vol1(k1) + (1d0-acl(link))*vol1(k2)
    end if

    if (volu > 0) then
       volui = 1d0/volu
       if (vol1(k1) > 0) then
          call QucPeripiaczekteta(1,link,ai,ae,volu,iadvL-2)   ! excess momentum in u(link) dir. out of k1, include own
          abh     = acl(link)*volui
          adveL   = adveL   + abh*ae
          advi(link) = advi(link) + abh*ai
       end if
       if (vol1(k2) > 0) then
          call QucPeripiaczekteta(2,link,ai,ae,volu,iadvL-2)   ! excess momentum in u(link) dir. out of k2
          abh = (1d0-acl(link))*volui
          adveL   = adveL   + abh*ae
          advi(link) = advi(link) + abh*ai
       end if

    end if

end subroutine calculate_advection_using_schemes_5_6

!> calculate_advection_using_schemes_7_till_12
!! Piaczek fully implicit
subroutine calculate_advection_using_schemes_7_till_12()

    iad  = 3
    if (iadvL == 8 .or. iadvL == 10 .or. iadvL == 12) then
       iad = 4
    end if

    if (kcu(link) == 1) then
       volu = acl(link)*vol1_f(k1) + (1d0-acl(link))*vol1_f(k2)
    else if (kcu(link) == 3 .and. iadveccorr1D2D == 1) then
       volu = au(link)*dx(link) ! Use volume weighting based on approximated "lateral volume", to avoid large 1D river volumes.
    else
       volu = acl(link)*vol1(k1) + (1d0-acl(link))*vol1(k2)
    end if

    if (volu > 0) then
       volui = 1d0/volu
       if (hs(k1) > 0) then
          call QucPeripiaczek(1,link,ai,ae,iad)   ! excess momentum in u(link) dir. out of k1, include own
          abh     = acl(link)*volui
          adveL   = adveL   + abh*ae
          advi(link) = advi(link) + abh*ai
       end if
       if (hs(k2) > 0) then
          call QucPeripiaczek(2,link,ai,ae,iad)   ! excess momentum in u(link) dir. out of k2
          abh = (1d0-acl(link))*volui
          adveL   = adveL   + abh*ae
          advi(link) = advi(link) + abh*ai
       end if

    end if
       
end subroutine calculate_advection_using_schemes_7_till_12

!> calculate_advection_using_scheme_21
!! subgrid weir small stencil, ifixedweirscheme = 3, upwind center velocity does not feel crest link
subroutine calculate_advection_using_scheme_21()

    if (u0(link)  > 0d0) then
       ku = k1 ; kd = k2 ; isg =  1 ; n12 = 1
    else
       ku = k2 ; kd = k1 ; isg = -1 ; n12 = 2
    end if

    call getucxucynoweirs(ku, ucxku, ucyku, ifixedweirscheme )
    if (jasfer3D == 1) then
       ucin = nod2linx(link,n12,ucxku,ucyku)*csu(link) + nod2liny(link,n12,ucxku,ucyku)*snu(link)
    else
       ucin = ucxku*csu(link) + ucyku*snu(link)
    end if

    fdx     = 0.5d0*dxi(link)*isg

    advi(link) = advi(link) + fdx*u0(link)
    advel   = advel   - fdx*ucin*ucin

end subroutine calculate_advection_using_scheme_21

!> calculate_advection_using_scheme_77
!! supercritical inflow boundary
subroutine calculate_advection_using_scheme_77()

    abh     = bai(k1)*huvli(link)*acl(link)
    if (jasfer3D == 1) then
       adveL = adveL   - abh*q1(link)*(nod2linx(link,1,ucx(k1),ucy(k1))*csu(link) + nod2liny(link,1,ucx(k1),ucy(k1))*snu(link))
    else
       adveL = adveL   - abh*q1(link)*(ucx(k1)*csu(link)+ucy(k1)*snu(link))
    end if
    advi(link) = advi(link) + abh*q1(link)
       
end subroutine calculate_advection_using_scheme_77

!> calculate_advection_using_scheme_38
!! explicit first order mom conservative olga (17), based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_38()

    qu1 = 0                                       ! and Perot control volume
    if (vol1(k1) > 0) then
       qu1 = QucPercu(1,link)                        ! excess momentum in/out uc(k1) dir. from k1
       qu1 = qu1*acl(link)/volau(k1)                 ! Perot weigthing
    end if
    qu2 = 0
    if (vol1(k2) > 0) then
       qu2 = QucPercu(2,link)                        ! excess momentum in/out uc(k2) dir. from k2
       qu2 = qu2*(1d0-acl(link))/volau(k2)           ! Perot weigthing
    end if
    advel = qu1 + qu2                             ! dimension: ((m4/s2) / m3) =   (m/s2)

end subroutine calculate_advection_using_scheme_38

!> calculate_advection_using_scheme_34
!! explicit first order mom conservative (stelling kramer), based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_34()

    qu1 = 0                                       ! and Perot control volume
    if (vol1(k1) > 0) then
       qu1 = QucPer(1,link)                          ! excess momentum in/out u(link) dir. from k1
       qu1 = qu1*acl(link)*bai(k1)                   ! Perot weigthing
    end if
    qu2 = 0
    if (vol1(k2) > 0) then
       qu2 = QucPer(2,link)                          ! excess momentum in/out u(link) dir. from k2
       qu2 = qu2*(1d0-acl(link))*bai(k2)             ! Perot weigthing
    end if
    advel = (qu1 + qu2)*huvli(link)                  ! dimension: ((m4/s2) / m3) =   (m/s2)
       
end subroutine calculate_advection_using_scheme_34      

!> calculate_advection_using_scheme_35
!! explicit first order mom conservative (stelling kramer), based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_35()

    qu1 = 0                                       ! and Perot control volume
    if (vol1(k1) > 0) then
       qu1 = QufPer(1,link)                          ! excess momentum in/out u(link) dir. from k1
       qu1 = qu1*acl(link)                           ! Perot weigthing
    end if
    qu2 = 0
    if (vol1(k2) > 0) then
       qu2 = QufPer(2,link)                          ! excess momentum in/out u(link) dir. from k2
       qu2 = qu2*(1d0-acl(link))                     ! Perot weigthing
    end if
    volu  = acl(link)*vol1(k1) + (1d0-acl(link))*vol1(k2)
    if (volu > 0) then
       advel = (qu1 + qu2)/volu                   ! dimension: ((m4/s2) / m3) =   (m/s2)
    end if
       
end subroutine calculate_advection_using_scheme_35

!> calculate_advection_using_scheme_36
!! explicit first order mom conservative, based upon cell center excess advection velocity
subroutine calculate_advection_using_scheme_36()

    qu1 = 0                                       ! and Perot control volume
    if (vol1(k1) > 0) then
       qu1 = QucPerq1(1,link)                        ! excess momentum in/out uc(k1) dir. from k1
       qu1 = qu1*acl(link)/vol1(k1)                  ! Perot weigthing
    end if
    qu2 = 0
    if (vol1(k2) > 0) then
       qu2 = QucPerq1(2,link)                        ! excess momentum in/out uc(k2) dir. from k2
       qu2 = qu2*(1d0-acl(link))/vol1(k2)            ! Perot weigthing
    end if
    advel = qu1 + qu2                             ! dimension: ((m4/s2) / m3) =   (m/s2)

end subroutine calculate_advection_using_scheme_36

!> calculate_advection_using_scheme_37
!! Kramer Stelling
subroutine calculate_advection_using_scheme_37()

    qu1 = 0d0
    if (vol1(k1) > 0) then
       qu1 = acl(link)*QucPerq1(1,link)/ba(k1)          ! excess momentum in/out u(link) dir. from k1
    end if
    qu2 = 0d0
    if (vol1(k2) > 0) then
       qu2 = (1d0-acl(link))*QucPerq1(2,link)/ba(k2)    ! excess momentum in/out u(link) dir. from k1
    end if
    advel = huvli(link)*(qu1 + qu2)
       
end subroutine calculate_advection_using_scheme_37

!> calculate_advection_for_3d
subroutine calculate_advection_for_3d()

 do LL  = 1, lnx

    if ( hu(LL) > 0 ) then

       iadvL = iadv(LL)
       if (LL > lnxi) then
          if (iadvL == 77) then
             if (u0(LL) < 0) cycle
          else if (u0(LL) > 0) then
             cycle                                            ! switch off advection for inflowing waterlevel bnd's, if not normalvelocitybnds
          end if
       end if
       cs  = csu(LL)  ; sn  = snu(LL)
       Lb  = Lbot(LL) ; Lt  = Ltop(LL)
       ac1 = acl(LL)  ; ac2 = 1d0 - ac1

       if (iadv(LL) == 3) then
          call QucPer3Dsigma(1,LL,Lb,Lt,cs,sn,quk1)           ! sum of (Q*uc cell centre upwind normal) at side 1 of basis link LL
          call QucPer3Dsigma(2,LL,Lb,Lt,cs,sn,quk2)           ! sum of (Q*uc cell centre upwind normal) at side 2 of basis link LL

          do link = Lb, Lt
             advel = 0d0                                      ! advi (1/s), adve (m/s2)
             k1    = ln(1,link) ; k2 = ln(2,link)
             qu1   = 0d0
             if (vol1(k1) > 0) then
                qu1 = quk1(1,link-Lb+1)*ac1                      ! Perot weigthing
             end if
             qu2    = 0d0
             if (vol1(k2) > 0) then
                qu2 = quk2(1,link-Lb+1)*ac2                      ! Perot weigthing
             end if
             if (jarhoxu == NO_RHO_EFFECTS_IN_MOMENTUM) then
                volu  = ac1*vol1(k1)         + ac2*vol1(k2)
             else
                volu  = ac1*vol1(k1)*rho(k1) + ac2*vol1(k2)*rho(k2)
             end if
             if (volu > 0) then
                advel = (qu1 + qu2)/volu                      ! dimension: ((m4/s2) / m3) =   (m/s2)
             end if
             adve(link) = adve(link) + advel

          end do

       else if ( iadv(LL) == 33 .or. iadv(LL) == 40 .or. iadv(LL) == 6 ) then                       !

          if (layertype == 1) then

             if (iadv(LL) == -6 ) then  ! .and. newzbndadv == 1 ) then

                do link = Lb, Lt
                   if (u1(link) > 0) then
                      k = ln(1,link)
                      n12 = 1
                   else
                      k = ln(2,link)
                      n12 = 2
                   end if
                   if (jasfer3D == 1) then
                      advel   = 2d0*( u1(link) - (cs*nod2linx(LL,n12,ucx(k),ucy(k)) + sn*nod2liny(LL,n12,ucx(k),ucy(k)) ) )*dxi(LL)
                   else
                      advel   = 2d0*( u1(link) - (cs*ucx(k) + sn*ucy(k) ) )*dxi(LL)
                   end if
                   if ( advel > 0d0 ) then
                      advi(link) = advi(link) + advel
                   else
                      adve(link) = adve(link) - cs*u1(link)*advel
                   end if
                end do

             else

             do link = Lb, Lt
                k1    = ln(1,link) ; k2 = ln(2,link)
                if (jasfer3D == 1) then
                   qu1   =  cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) +  sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(link)*sqa(k1)
                   qu2   =  cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) +  sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(link)*sqa(k2)
                else
                   qu1   =  cs*uqcx(k1) + sn*uqcy(k1) - u1(link)*sqa(k1)
                   qu2   =  cs*uqcx(k2) + sn*uqcy(k2) - u1(link)*sqa(k2)
                end if

                if (jarhoxu > 0) then
                      volu  = ac1*vol1(k1)*rho(k1) + ac2*vol1(k2)*rho(k2)
                else
                      volu  = ac1*vol1(k1)         + ac2*vol1(k2)
                end if

                if (volu > 0) then

                   adve(link) = adve(link) + (ac1*qu1 + ac2*qu2) / volu

                end if
             end do

             end if

          else if (layertype == 2 .and. jahazlayer == 0) then ! default fixed layers

             Ltx = Lt-Lb+1
             volukk(1:Ltx) = 0d0
             do link = Lb, Lt
                k1    = ln(1,link) ; k2 = ln(2,link)
                if (jarhoxu > 0) then
                   volukk(link-Lb+1)  = volukk(link-Lb+1) + ac1*vol1(k1)*rho(k1) + ac2*vol1(k2)*rho(k2)
                else
                   volukk(link-Lb+1)  = volukk(link-Lb+1) + ac1*vol1(k1) + ac2*vol1(k2)
                end if
             end do
             do k = k1+1, ktop(ln(1,LL) )
                if (jarhoxu > 0) then
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac1*vol1(k)*rho(k)
                else
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac1*vol1(k)
                end if
             end do
             do k = k2+1, ktop(ln(2,LL) )
                if (jarhoxu > 0) then
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac2*vol1(k)*rho(k)
                else
                   volukk(Lt-Lb+1) = volukk(Lt-Lb+1) + ac2*vol1(k)
                end if
             end do

             do link = Lb, Lt
                k1    = ln(1,link) ; k2 = ln(2,link)
                if (jasfer3D == 1) then
                   qu1    = cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) + sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(link)*sqa(k1)
                   qu2    = cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) + sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(link)*sqa(k2)
                else
                   qu1    = cs*uqcx(k1) + sn*uqcy(k1) - u1(link)*sqa(k1)
                   qu2    = cs*uqcx(k2) + sn*uqcy(k2) - u1(link)*sqa(k2)
                end if

                if (volukk(link-Lb+1) > 0) then
                   adve(link) = adve(link) +  (ac1*qu1 + ac2*qu2) / volukk(link-Lb+1)
                end if

             end do

          else if (layertype == 2 .and. jahazlayer == 1) then

             n1  = ln(1,LL) ; n2 = ln(2,LL)
             call getkbotktop(n1,kb1,kt1) ; ktx1 = kt1-kb1+1
             call getkbotktop(n2,kb2,kt2) ; ktx2 = kt2-kb2+1
             Ltx = Lt-Lb+1

             volukk(1:Ltx) = 0d0

             do link = Lb, Lt
                k1    = ln(1,link) ; k2 = ln(2,link) ; L1 = link-Lb+1
                volukk(L1) = volukk(L1) + ac1*vol1(k1) + ac2*vol1(k2)
             end do

             do k = k1+1, kt1
                volukk(Ltx) = volukk(Ltx) + ac1*vol1(k)
             end do

             do k = k2+1, kt2
                volukk(Ltx) = volukk(Ltx) + ac2*vol1(k)
             end do

             do link = Lb, Lt
                k1    = ln(1,link) ; k2 = ln(2,link) ; L1 = link-Lb+1
                if (volukk(L1) > 0) then
                   if (jasfer3D == 1) then
                      qu1   =  cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) +  sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(link)*sqa(k1)
                      qu2   =  cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) +  sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(link)*sqa(k2)
                   else
                      qu1     = cs*uqcx(k1) + sn*uqcy(k1) - u1(link)*sqa(k1)
                      qu2     = cs*uqcx(k2) + sn*uqcy(k2) - u1(link)*sqa(k2)

                   end if
                   adve(link) = adve(link) + (ac1*qu1 + ac2*qu2) / volukk(link-Lb+1)
                end if
             end do

          else if (layertype == 2 .and. jahazlayer == 2 ) then  ! lineinterp

             n1  = ln(1,LL) ; n2 = ln(2,LL)
             call getkbotktop(n1,kb1,kt1)
             call getkbotktop(n2,kb2,kt2)
             hs1 = max(epshs, zws(kt1) - zws(kb1-1) )
             hs2 = max(epshs, zws(kt2) - zws(kb2-1) )

             ktx01 = kt1 - kb1 + 1
             ktx02 = kt2 - kb2 + 1

             volk1(0) = 0d0 ; quuk1(0) = 0d0 ; sqak1(0) = 0d0 ; sigk1(0) = 0d0
             do k = kb1, kt1
                volk1(k-kb1+1) = volk1(k-kb1) + vol1(k)
                if (jasfer3D == 1) then
                   quuk1(k-kb1+1) = quuk1(k-kb1) + cs*nod2linx(LL,1,uqcx(k),uqcy(k)) + sn*nod2liny(LL,1,uqcx(k),uqcy(k))
                else
                   quuk1(k-kb1+1) = quuk1(k-kb1) + cs*uqcx(k) + sn*uqcy(k)
                end if
                sqak1(k-kb1+1) = sqak1(k-kb1) + sqa(k)
                sigk1(k-kb1+1) = ( zws(k) - zws(kb1-1) ) / hs1
             end do

             volk2(0) = 0d0 ; quuk2(0) = 0d0 ; sqak2(0) = 0d0 ; sigk2(0) = 0d0
             do k = kb2, kt2
                volk2(k-kb2+1) = volk2(k-kb2) + vol1(k)
                if (jasfer3D == 1) then
                   quuk2(k-kb2+1) = quuk2(k-kb2) + cs*nod2linx(LL,2,uqcx(k),uqcy(k)) + sn*nod2liny(LL,2,uqcx(k),uqcy(k))
                else
                   quuk2(k-kb2+1) = quuk2(k-kb2) + cs*uqcx(k) + sn*uqcy(k)
                end if
                sqak2(k-kb2+1) = sqak2(k-kb2) + sqa(k)
                sigk2(k-kb2+1) = ( zws(k) - zws(kb2-1) ) / hs2
             end do

             do link = Lb, Lt  ;  Ltx0 = Lt - Lb + 1 ; siguL(0) = 0d0
                siguL(link-Lb+1) = hu(link) / hu(LL)
             end do

             call lineinterp3( siguL, quuL1, volL1, sqaL1, Ltx0, sigk1, quuk1, volk1, sqak1, ktx01)
             call lineinterp3( siguL, quuL2, volL2, sqaL2, Ltx0, sigk2, quuk2, volk2, sqak2, ktx02)

             do link = Lb, Lt
                vo1  = volL1(link-Lb+1) - volL1(link-Lb)
                vo2  = volL2(link-Lb+1) - volL2(link-Lb)
                volu = vo1*ac1 + vo2*ac2

                if (volu > 0) then
                   qu1   = quuL1(link-Lb+1) - quuL1(link-Lb) - u1(link)*( sqaL1(link-Lb+1) - sqaL1(link-Lb) )
                   qu2   = quuL2(link-Lb+1) - quuL2(link-Lb) - u1(link)*( sqaL2(link-Lb+1) - sqaL2(link-Lb) )
                   advel = ( ac1*qu1 + ac2*qu2 ) / volu
                   adve(link) = adve(link) + advel
                end if
             end do

          else if (layertype == 2 .and. jahazlayer == 4) then

             n1  = ln(1,LL) ; n2 = ln(2,LL)
             call getkbotktop(n1,kb1,kt1) ; ktx1 = kb1 + kmxn(n1) - 1
             call getkbotktop(n2,kb2,kt2) ; ktx2 = kb2 + kmxn(n2) - 1

             Ltx = Lt-Lb+1

             volukk(1:Ltx) = 0d0 ; quuk1(1:Ltx) = 0d0 ; sqak1(1:Ltx) = 0d0

             do k = kb1, ln(1,Lb) - 1                   ! below Lb n1
                volukk(1) = volukk(1)     + ac1*vol1(k)
                if (jasfer3D == 1) then
                   quuk1(1) = quuk1(1) + ac1*(cs*nod2linx(LL,1,uqcx(k),uqcy(k)) + sn*nod2liny(LL,1,uqcx(k),uqcy(k)))
                else
                   quuk1(1) = quuk1 (1)     + ac1*(cs*uqcx(k) + sn*uqcy(k))
                end if
                sqak1 (1) = sqak1 (1)     + ac1*sqa(k)
             end do

             do k = kb2, ln(2,Lb) - 1                   ! below Lb n2
                volukk(1) = volukk(1)     + ac2*vol1(k)
                if (jasfer3D == 1) then
                   quuk1(1) = quuk1(1) + ac2*(cs*nod2linx(LL,2,uqcx(k),uqcy(k)) + sn*nod2liny(LL,2,uqcx(k),uqcy(k)))
                else
                   quuk1(1) = quuk1 (1)     + ac2*(cs*uqcx(k) + sn*uqcy(k))
                end if
                sqak1 (1) = sqak1 (1)     + ac2*sqa(k)
             end do

             do link = Lb, Lt                              ! intermediate
                k1    = ln(1,link) ; k2 = ln(2,link) ; L1 = link-Lb+1
                volukk(L1) = volukk(L1) + ac1*vol1(k1)                    + ac2*vol1(k2)
                if (jasfer3D == 1) then
                   quuk1 (L1) = quuk1(L1) + ac1*(cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) + sn*nod2liny(LL,1,uqcx(k1),uqcy(k1))) +   &
                                            ac2*(cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) + sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)))
                else
                   quuk1 (L1) = quuk1 (L1) + ac1*(cs*uqcx(k1) + sn*uqcy(k1)) + ac2*(cs*uqcx(k2) + sn*uqcy(k2))
                end if
                sqak1 (L1) = sqak1 (L1) + ac1*sqa(k1)                     + ac2*sqa(k2)
             end do

             do k = k1+1, ktx1                          ! above Lt n1
                volukk(Ltx) = volukk(Ltx) + ac1*vol1(k)
                if (jasfer3D == 1) then
                   quuk1 (Ltx) = quuk1(Ltx)  + ac1*(cs*nod2linx(LL,1,uqcx(k),uqcy(k)) + sn*nod2liny(LL,1,uqcx(k),uqcy(k)))
                else
                   quuk1 (Ltx) = quuk1(Ltx)  + ac1*(cs*uqcx(k) + sn*uqcy(k))
                end if
                sqak1 (Ltx) = sqak1(Ltx)  + ac1*sqa(k)
             end do

             do k = k2+1, ktx2                          ! above Lt n2
                volukk(Ltx) = volukk(Ltx) + ac2*vol1(k)
                if (jasfer3D == 1) then
                   quuk1 (Ltx) = quuk1(Ltx)  + ac2*(cs*nod2linx(LL,2,uqcx(k),uqcy(k)) + sn*nod2liny(LL,2,uqcx(k),uqcy(k)))
                else
                   quuk1 (Ltx) = quuk1 (Ltx) + ac2*(cs*uqcx(k) + sn*uqcy(k))
                end if
                sqak1 (Ltx) = sqak1 (Ltx) + ac2*sqa(k)
             end do

             do link  = Lb, Lt
                L1 = link-Lb+1
                if (volukk(L1) > 0) then
                   adveL   = ( quuk1(L1) - u1(link)*sqak1(L1) ) / volukk(L1)
                   if (abs(advel) > 0.05) then
                      advel = 1d0*advel
                   end if
                   adve(link) = adve(link) + adveL
               end if
             end do

          end if

       else if (iadv(LL) == 34) then                          ! Kramer Stelling, ba per cell weighted

          call QucPer3Dsigma(1,LL,Lb,Lt,cs,sn,quk1)           ! sum of (Q*uc cell centre upwind normal) at side 1 of basis link LL
          call QucPer3Dsigma(2,LL,Lb,Lt,cs,sn,quk2)           ! sum of (Q*uc cell centre upwind normal) at side 2 of basis link LL
          baik1 = bai( ln(1,LL) )
          baik2 = bai( ln(2,LL) )
          do link = Lb, Lt
             advel = 0                                        ! advi (1/s), adve (m/s2)
             k1    = ln(1,link) ; k2 = ln(2,link)
             qu1   = 0d0
             if (vol1(k1) > 0) then
                qu1 = quk1(1,link-Lb+1)*ac1*baik1
             end if
             qu2    = 0
             if (vol1(k2) > 0) then
                qu2 = quk2(1,link-Lb+1)*ac2*baik2            ! Perot weigthing
             end if
             huvL    = ac1*(zws(k1)-zws(k1-1)) + ac2*(zws(k2)-zws(k2-1))
             if (huvL > 0d0) then
                advel   = (qu1 + qu2)/huvL                         ! dimension: ((m4/s2) / m3) =   (m/s2)
                adve(link) = adve(link) + advel
             end if
          end do

       else if (iadv(LL) == 5) then

          call QucPer3Dsigmapiaczekteta(LL,Lb,Lt,cs,sn,quk1,quk2)

          do link = Lb, Lt
              adve(link) = adve(link) + quk1(1,link-Lb+1)
              advi(link) = advi(link) + quk2(1,link-Lb+1)
          end do

       else if (iadv(LL) == 44) then

            do link = Lb, Lt
               k1    = ln(1,link) ; k2 = ln(2,link)
               if (vol1(k1) > 0) then
                  if (jasfer3D == 1) then
                     qu1     = cs*nod2linx(LL,1,uqcx(k1),uqcy(k1)) + sn*nod2liny(LL,1,uqcx(k1),uqcy(k1)) - u1(link)*sqa(k1)
                  else
                     qu1     = cs*uqcx(k1)  + sn*uqcy(k1) - u1(link)*sqa(k1)
                  end if
                  adve(link) = adve(link) + ac1*qu1/vol1(k1)
               end if
               if (vol1(k2) > 0) then
                  if (jasfer3D == 1) then
                     qu2    =  cs*nod2linx(LL,2,uqcx(k2),uqcy(k2)) + sn*nod2liny(LL,2,uqcx(k2),uqcy(k2)) - u1(link)*sqa(k2)
                  else
                     qu2     = cs*uqcx(k2)  + sn*uqcy(k2) - u1(link)*sqa(k2)
                  end if
                  adve(link) = adve(link) + ac2*qu2/vol1(k2)
               end if
            end do

       end if   ! advectiontypes

    end if      ! (hu)

 end do         ! LL

end subroutine calculate_advection_for_3d

end subroutine calculate_advection

end module m_advection
    