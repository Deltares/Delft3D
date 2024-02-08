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

      SUBROUTINE TEKNET(NCOL,ja)

      use m_netw
      use unstruc_colors
      use geometry_module, only: dbdistance
      use unstruc_display

      implicit none
      integer :: ncol, ja

      integer :: k, LMOD
      integer :: k0
      integer :: k1
      integer :: k2
      integer :: k3
      integer :: kk
      integer :: L, LL
      integer :: n
      integer :: ndraw
      double precision :: d1, d2, x, y
!      double precision :: t0, t1
      integer :: is, ie, ip
      integer :: iflip = 1
      logical inview


      COMMON /DRAWTHIS/ ndraw(50)

      IF (NDRAW(2) .LE. 0 .or. NUML == 0 ) RETURN

!      call klok(t0)

     if (ndraw(2) .ne. 3) then  ! net zelf

        ! iflip = -iflip
        ! if (.false. .and. allocated(netlinkpath_xk) .and. iflip==1) then
        ! write (*,*) 'Fast plotter'
        ! is = 1
        ! CALL SETCOL(NCOL)
        ! do L=1,numpath
        !    ie = netlinkpath_end(L)
        !    call POLYLINE(netlinkpath_xk(is:ie), &
        !                     netlinkpath_yk(is:ie), &
        !                     ie-is+1)
        !    is = ie+1
        ! end do

         call setcol(ncoldn)
         DO L = 1,NUML
            if (ja.ne.-1234 .and. mod(L,500) == 0) then
                call halt2(ja)
                if (ja == 1) exit
            endif

            if (kn(3,L) == 2) then
               K1 = KN(1,L)
               K2 = KN(2,L)
               IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
                  IF (INVIEW(XK(K1),YK(K1)) .or. INVIEW(XK(K2),YK(K2)) ) THEN
                     CALL MOVABS( XK(K1),YK(K1))
                     CALL  LNABS( XK(K2),YK(K2))
                  ENDIF
               ENDIF
            endif
         ENDDO

         CALL SETCOL(NCOLNN)
         DO K = 1,NUMK
            if (ja.ne.-1234 .and. mod(k,500) == 0) then
               call halt2(ja)
               if (ja == 1) exit
            endif

            if ( INVIEW( XK(K),YK(K) ) ) then
               CALL PTABS(XK(K),YK(K))
            ENDIF
         ENDDO

         if (ndraw(2) == 4) then
            call setcol(ncoldg)
            Do L = 1, numl
               if (kn(3,L) == 1 .or. kn(3,L) == 3 .or. kn(3,L) == 4) then
                  k1 = kn(1,L)
                  x  = xk(k1)
                  y  = yk(k1)
                  call fbox(x-0.5d0*rcir,y-0.5d0*rcir,x+0.5d0*rcir,y+0.5d0*rcir)
                  k1 = kn(2,L)
                  x  = xk(k1)
                  y  = yk(k1)
                  call fbox(x-0.5d0*rcir,y-0.5d0*rcir,x+0.5d0*rcir,y+0.5d0*rcir)
               endif
            enddo
         endif

         DO L = 1,NUML
             if (ja.ne.-1234 .and. mod(L,500) == 0) then
                call halt2(ja)
                if (ja == 1) exit
             endif
             K3 = KN(3,L)
             if (k3 .ne. 2 .and. k3 .ne. 0) then
                K1 = KN(1,L)
                K2 = KN(2,L)
                IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
                   IF (INVIEW(XK(K1),YK(K1)) .OR. INVIEW(XK(K2),YK(K2)) ) THEN
                      CALL MOVABS( XK(K1),YK(K1))  ;
                      CALL  LNABS( XK(K2),YK(K2))
                      CALL SETLINKCOLOUR(L,1)
                      CALL MOVABS( XK(K1),YK(K1))  ;
                      call CIR(1.2d0*rcir)
                      CALL  LNABS( XK(K2),YK(K2))
                      call CIR(1.2d0*rcir)
                   ENDIF
                ENDIF
             endif
         ENDDO
      endif


      IF ( (NDRAW(2) == 2 .or. NDRAW(2) == 3 ).AND. SIZE(LNN) .GE. NUML) THEN !outline
         CALL SETCOL(NCOLRN)
         LMOD = MAX(1,NUML/100)

         DO L = 1,NUML
             if (ja.ne.-1234 .and. mod(L,LMOD) == 0) then
                call halt2(ja)
                if (ja == 1) exit
             endif
             IF (LNN(L) == 1) THEN
                K1 = KN(1,L)
                K2 = KN(2,L)
                IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
                   CALL MOVABS( XK(K1),YK(K1) )
                   CALL  LNABS( XK(K2),YK(K2) )
                ENDIF
             ENDIF
         ENDDO
      ENDIF

      IF ( NDRAW(2) == 4) THEN
         CALL TEKXZ(221)
      ENDIF

      IF (NDRAW(22) .GE. 2) CALL TEKFACES()

      if (NDRAW(2)==5) then
        ! Draw link crossings (precomputed by checknet)
        DO L = 1,nlinkcross
          call TEKLINK (linkcross(1,L),NCOLWARN1)
          call TEKLINK (linkcross(2,L),NCOLWARN2)

          LL = linkcross(1,L)
          if (kn(1,linkcross(1,L)) <= 0 .or. kn(1,linkcross(1,L)) > numk .or. &
              kn(2,linkcross(1,L)) <= 0 .or. kn(2,linkcross(1,L)) > numk .or. &
              kn(1,linkcross(2,L)) <= 0 .or. kn(1,linkcross(2,L)) > numk .or. &
              kn(2,linkcross(2,L)) <= 0 .or. kn(2,linkcross(2,L)) > numk) cycle
          d1 = max(abs(xk(kn(2,linkcross(1,L)))-xk(kn(1,linkcross(1,L)))), &
                   abs(yk(kn(2,linkcross(1,L)))-yk(kn(1,linkcross(1,L)))))

          d2 = max(abs(xk(kn(2,linkcross(2,L)))-xk(kn(1,linkcross(2,L)))), &
                   abs(yk(kn(2,linkcross(2,L)))-yk(kn(1,linkcross(2,L)))))

          ! If zoom is very small: plot large dots to mark crossings clearly.
          if (max(d1, d2) < 2*RCIR) then
              CALL CIRR(xk(kn(1,linkcross(1,L))), yk(kn(1,linkcross(1,L))), NCOLWARN1)
          end if
        end do

        ! Also draw bad orthogonality links (precomputed by cosphiucheck)
        ! and too short flow links (precomputed by flow_geominit) .
        DO L = 1,nlinkbadortho+nlinktoosmall
          LL = linkbadqual(L)
          if (LL <= 0 .or. LL > numl) cycle
          if (kn(1,LL) <= 0 .or. kn(1,LL) > numk .or. &
              kn(2,LL) <= 0 .or. kn(2,LL) > numk) cycle
          call TEKLINK (LL,NCOLWARN3)
          d1 = max(abs(xk(kn(2,LL))-xk(kn(1,LL))), &
                   abs(yk(kn(2,LL))-yk(kn(1,LL))))

          ! If zoom is very small: plot large dots to mark crossings clearly.
          if (d1 < 2*RCIR) then
              CALL CIRR(xk(kn(1,LL)), yk(kn(1,LL)), NCOLWARN3)
          end if
        end do
      end if

!      call klok(t1)

!      write(6,"('time elapsed in teknet: ', F15.5, 'seconds')") t1-t0

      RETURN
      END SUBROUTINE TEKNET
