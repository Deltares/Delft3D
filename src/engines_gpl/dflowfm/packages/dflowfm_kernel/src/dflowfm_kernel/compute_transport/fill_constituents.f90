!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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

!> fill constituent array
subroutine fill_constituents(jas) ! if jas == 1 do sources
   use m_transport,            only: ISED1, ISPIR, NUMCONST, ISALT, ITEMP, ITRA1, ITRAN, constituents, const_sour, const_sink, difsedu, difsedw
   use m_flowgeom,             only: ndx, ndxi, ba
   use m_flow,                 only: kmx, ndkx, zws, hs, sq, vol1, spirint, spirucm, spircrv, fcoris, czssf
   use m_wind,                 only: heatsrc
   use m_physcoef,             only: dicouv, dicoww, difmolsal, difmoltem, difmoltracer, Jaallowcoolingbelowzero, ag, vonkar
   use m_nudge,                only: nudge_rate, nudge_tem, nudge_sal
   use m_turbulence,           only: sigsal, sigtem, sigtracer, sigdifi, sigsed, wsf
   use m_flowexternalforcings, only: wstracers, numsrc, ksrc, qsrc, ccsrc
   use m_sediment,             only: sed, sedtra, stm_included, stmpar, jased, mxgr, ws
   use m_mass_balance_areas,   only: jamba, mbadefdomain, mbafluxheat, mbafluxsorsin
   use m_partitioninfo,        only: jampi, idomain, my_rank
   use m_sferic,               only: jsferic, fcorio
   use m_flowtimes ,           only : dts, time1, tstart_user, tfac
   use m_flowparameters,       only: janudge, jasecflow, jatem, jaequili, epshu, epshs, testdryflood, icorio
   use m_missing,              only: dmiss
   use timers,                 only: timon, timstrt, timstop

   implicit none

   integer, intent(in)         :: jas

   double precision            :: dvoli
   integer                     :: i, iconst, j, kk, kkk, k, kb, kt, n, kk2, L, imba, jamba_src
   double precision, parameter :: dtol=1d-8
   double precision            :: spir_ce, spir_be, spir_e, alength_a, time_a, alpha, fcoriocof, qsrck, qsrckk, dzss

   double precision            :: Trefi

   integer(4) ithndl /0/
   if (timon) call timstrt ( "fill_constituents", ithndl )

   const_sour = 0d0
   const_sink = 0d0

   do k=1,Ndkx
 
      if( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
         constituents(ISPIR,k) = spirint(k)
      endif

      if ( ISED1.ne.0 ) then
         do i=1,mxgr
            iconst = ISED1+i-1
            constituents(iconst,k) = sed(i,k)
         end do
      end if
   end do

   if (stm_included) then
      if (stmpar%morpar%bedupd .and. time1 >= tstart_user + stmpar%morpar%tmor*tfac) then
         if ( ISED1.ne.0 ) then
            do k=1,ndx
               if (hs(k)<stmpar%morpar%sedthr) then
                  do i=1,mxgr
                     iconst = ISED1+i-1
                     call getkbotktop(k,kb,kt)
                     constituents(iconst,kb:kt) = 0d0
                  end do
               endif
            end do
         end if
      endif
   endif

   difsedu = 0d0 ; difsedw = 0d0 ; sigdifi = 0d0

!  diffusion coefficients

   if ( ISALT.ne.0 ) then
      if (dicouv .ge. 0d0) then
          difsedu(ISALT) =          difmolsal
      endif
      if (dicoww .ge. 0d0) then
          difsedw(ISALT) = dicoww + difmolsal
          sigdifi(ISALT) = 1d0/sigsal
      endif
   end if

   if ( ITEMP.ne.0 ) then
      if (dicouv .ge. 0d0) then
          difsedu(ITEMP) =          difmoltem
      endif
      if (dicoww .ge. 0d0) then
          difsedw(ITEMP) = dicoww + difmoltem
          sigdifi(ITEMP) = 1d0/sigtem
      endif
   end if

   if( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
      difsedu(ISPIR) = 0d0
      difsedw(ISPIR) = 0d0 !dicoww + difmoltem
      sigdifi(ISPIR) = 0d0 !/sigspi
   endif

   if ( ISED1.ne.0) then
      do i=1,mxgr
         iconst = ISED1+i-1
         if (dicouv .ge. 0d0) difsedu(iconst) = 0d0
         if (dicoww .ge. 0d0) then
            difsedw(iconst) = dicoww
            sigdifi(iconst) = 1d0/sigsed(iconst)
         endif
         if (jased < 4) wsf(iconst) = ws(i)
      end do
   end if

   if ( ITRA1.gt.0 ) then
      do i=ITRA1,ITRAN
         difsedu(i)   =            difmoltracer
         if (dicoww .ge. 0d0) then
             difsedw(i) = dicoww + difmoltracer
             sigdifi(i) = 1d0/sigtracer
         endif
         wsf(i) = wstracers(i - itra1 + 1)
      end do
   end if

!  sources
   do kk=1,Ndx

 
!     nudging
      Trefi = 0d0
      if ( janudge.eq.1 .and. jas.eq.1 ) then
!        get reference time
         Trefi = nudge_rate(kk)
      end if

      call getkbotktop(kk,kb,kt)
      do k=kb,kt
         dvoli = 1d0/max(vol1(k),dtol)
         if (testdryflood == 2 ) dvoli = 1d0/max(vol1(k),epshu*ba(kk)/max(kt-kb+1,1))

!        temperature
         if (jatem > 1) then
            if (Jaallowcoolingbelowzero == 0) then  ! default behaviour since 2017
                                                    ! no cooling below 0 degrees  
               if (heatsrc(k) > 0d0) then
                  const_sour(ITEMP,k) = heatsrc(k)*dvoli
               else if (heatsrc(k) < 0d0) then
                  const_sink(ITEMP,k) = -heatsrc(k)*dvoli / max(constituents(itemp, k),0.001)
               endif
            else                                    ! allowing cooling below 0 degrees 
               const_sour(ITEMP,k) = heatsrc(k)*dvoli
            endif 
         endif

!        nudging
         if ( Trefi.gt.0d0 ) then
            if ( ITEMP.gt.0 .and. nudge_tem(k).ne.DMISS ) then
               const_sour(ITEMP,k) = const_sour(ITEMP,k) + nudge_tem(k) * Trefi
               const_sink(ITEMP,k) = const_sink(ITEMP,k) + Trefi
            end if

            if ( ISALT.gt.0 .and. nudge_sal(k).ne.DMISS ) then
               const_sour(ISALT,k) = const_sour(ISALT,k) + nudge_sal(k) * Trefi
               const_sink(ISALT,k) = const_sink(ISALT,k) + Trefi
            end if
         end if

!        terms due to non-conservative formulation
         do j=1,NUMCONST
            const_sour(j,k) = const_sour(j,k) - constituents(j,k) * sq(k) * dvoli
         end do
      end do

!     Note: from now on, only _add_ to sources

!     spiral flow source term
      if ( jasecflow > 0 .and. jaequili == 0 .and. kmx == 0 ) then
         if( spirucm(kk) < 1d-3 .or. hs(kk) < epshu ) then
            ! const_sour(ISPIR,kk) = 0d0
            ! const_sink(ISPIR,kk) = 0d0
         else
            fcoriocof = fcorio ; if( icorio > 0 .and. jsferic == 1 ) fcoriocof = fcoris(kk)
            alpha = sqrt( ag ) / vonkar / max( czssf(kk), 20.0d0 )
            spir_ce = fcorio * hs(kk) * 0.5d0
            spir_be = hs(kk) * spircrv(kk) * spirucm(kk)
            spir_e  = spir_be - spir_ce
            alength_a = ( 1.0d0 - 2.0d0 * alpha ) * hs(kk) / ( 2.0d0 * vonkar * vonkar * alpha )  !TO DO: this term should be expanded to prevent negative alength_a for alpha > 0.5
            time_a    = alength_a / spirucm(kk)
            !const_sour(ISPIR,kk) =  ( spir_e - spirint(kk) ) / time_a !* dvoli    ! S=(I_eq - I)/Ta
            const_sour(ISPIR,kk) = const_sour(ISPIR,kk) + spir_e / time_a
            const_sink(ISPIR,kk) = const_sink(ISPIR,kk) + 1d0    / time_a
         endif
      end if

!     sediment (2D sources, also in 3D)
!
      if ( stm_included ) then
         if ( ISED1.gt.0 .and. kk.le.Ndxi ) then
            do i=1,mxgr
               kkk = sedtra%kmxsed(kk,i)
               if ( kkk.gt.0 ) then
                  iconst = i+ISED1-1
                  const_sour(iconst,kkk) = const_sour(iconst,kkk)+sedtra%sourse(kk,i)
                  const_sink(iconst,kkk) = const_sink(iconst,kkk)+sedtra%sinkse(kk,i)
                  
                  if (stmpar%morpar%flufflyr%iflufflyr .gt. 0) then
                     const_sour(iconst,kkk) = const_sour(iconst,kkk) + stmpar%morpar%flufflyr%sourf(i,kk)
                     const_sink(iconst,kkk) = const_sink(iconst,kkk) + stmpar%morpar%flufflyr%sinkf(i,kk)
                  end if

               end if
            end do
         end if
      end if
   end do

   if (jamba > 0 .and. jatem > 0) then   ! Positive and negative sums for jamba, checking just once   
                                         
      do kk=1,Ndx
         imba = mbadefdomain(kk)
         if (imba > 0) then 
            call getkbotktop(kk,kb,kt)
            do k=kb,kt
               if (heatsrc(k) > 0d0) then
                   mbafluxheat(1,imba) = mbafluxheat(1,imba) + heatsrc(k)*dts
               else if (heatsrc(k) < 0d0) then
                   mbafluxheat(2,imba) = mbafluxheat(2,imba) - heatsrc(k)*dts
               endif
            enddo       
         endif
      enddo

   endif


   ! NOTE: apply_tracer_bc has been moved earlier to transport() routine,
   !       but apply_sediment_bc must still be done here, since above the boundary
   !       nodes's constituents(kb,:) = sed(kb,:) has reset it to 0.
   if ( stm_included ) call apply_sediment_bc()
   if (jas == 0) goto 1234                    ! no sources from initialise

   do n  = 1,numsrc
      kk     = ksrc(1,n)                   ! 2D pressure cell nr FROM
      kk2    = ksrc(4,n)                   ! 2D pressure cell nr TO
      qsrckk = qsrc(n)
      qsrck  = qsrckk

      jamba_src = jamba
      if (jampi.eq.1) then
         if(kk > 0) then
            if ( idomain(kk) /= my_rank ) jamba_src = 0
         else
            if(kk2 > 0) then
               if ( idomain(kk2) /= my_rank ) jamba_src = 0
            else
               jamba_src = 0
            endif
         endif
      endif

      if (kk > 0) then                     ! FROM Point
         do k = ksrc(2,n) , ksrc(3,n)
            dvoli  = 1d0/max(vol1(k),dtol)
            if (kmx > 0) then
               dzss  = zws(ksrc(3,n)) - zws(ksrc(2,n)-1)
               if (dzss > epshs) then
                  qsrck = qsrckk*( zws(k) - zws(k-1) ) / dzss
               else
                  qsrck = qsrckk/( ksrc(3,n) - ksrc(2,n) + 1)
               endif
            endif
            if (qsrck > 0) then              ! FROM k to k2
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) - qsrck*constituents(L,k)*dvoli
                  if (jamba_src > 0) then
                     mbafluxsorsin(2,1,L,n) = mbafluxsorsin(2,1,L,n) + qsrck*constituents(L,k)*dts
                  endif
               enddo
            else if  (qsrck  < 0) then       ! FROM k2 to k
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) - qsrck*ccsrc(L,n)*dvoli
                  if (jamba_src > 0) then
                     mbafluxsorsin(1,1,L,n) = mbafluxsorsin(1,1,L,n) - qsrck*ccsrc(L,n)*dts
                  endif
               enddo
            endif
         enddo
      endif

      if (kk2 > 0) then                   ! TO Point
         do k = ksrc(5,n) , ksrc(6,n)
            dvoli = 1d0/max(vol1(k),dtol)
            if (kmx > 0) then
               dzss  = zws(ksrc(6,n)) - zws(ksrc(5,n)-1)
               if (dzss > epshs) then
                  qsrck = qsrckk*( zws(k) - zws(k-1) ) / dzss
               else
                  qsrck = qsrckk/( ksrc(6,n) - ksrc(5,n) + 1)
               endif
            endif
            if (qsrck > 0) then
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) + qsrck*ccsrc(L,n)*dvoli
                  if (jamba_src > 0) then
                     mbafluxsorsin(2,2,L,n) = mbafluxsorsin(2,2,L,n) + qsrck*ccsrc(L,n)*dts
                  endif
               enddo
            else if  (qsrck  < 0) then
               do L = 1,numconst
                  const_sour(L,k) = const_sour(L,k) + qsrck*constituents(L,k)*dvoli
                  if (jamba_src > 0) then
                     mbafluxsorsin(1,2,L,n) = mbafluxsorsin(1,2,L,n) -  qsrck*constituents(L,k)*dts
                  endif
               enddo
            endif
         enddo
      endif

   enddo

1234 continue

   if (timon) call timstop( ithndl )
   return
end subroutine fill_constituents
