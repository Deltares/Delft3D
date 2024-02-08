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

  SUBROUTINE DELNET(KEY, jacheckcells, JASAVE)
  use m_netw
  USE m_missing
  use m_sferic, only: jsferic, jasfer3D
  use m_polygon, only: NPL, xpl, ypl, zpl
  use geometry_module, only: dbpinpol, half
  use gridoperations

  implicit none
  integer :: KEY, jacheckcells, JASAVE

  integer :: inhul, inall, ip, ic, n, k, nn, nzero
  integer :: ja
  integer :: k1
  integer :: k2
  integer :: l
  integer, allocatable :: Lc2(:)
! delete grid

  DOUBLE PRECISION :: XL, YL

  inhul = -1 ; inall = 1

  IF (JASAVE .EQ. 1) CALL SAVENET()

  KEY = 3
  IF (NPL .LE. 2) THEN
     CALL CONFRM('NO POLYON, SO DELETE all NET POINTS ? ',JA)
     IF (JA .EQ. 0) THEN
        KEY = 0
        RETURN
     ENDIF
  ENDIF

  if (jadelnetlinktyp > 0) then
     do L = 1,numL
        if (kn(3,L) == jadelnetlinktyp) then
           k1 = kn(1,L) ; k2 = kn(2,L)
           call half(xk(k1),yk(k1),xk(k2),yk(k2),xL,yL, jsferic, jasfer3D)
           CALL DBPINPOL( XL, YL, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
           if (inhul == 1) then
              kn(1,L) = 0 ; kn(2,L) = 0
           endif
        endif
     enddo
     CALL SETNODADM(0)
     CALL DELPOL()
     return
  endif

  if (jacheckcells == 1) then
    call savepol()
    NPL = 0
    call findcells(0)
    call restorepol()

      DO L = 1,NUML
         K1 = KN(1,L) ; K2 = KN(2,L)
         IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
            ! Delete links IF all of the cells they participate in are in pol.
            IF (jacheckcells == 1 .and. (kn(3,L) == 0 .or. kn(3,L) == 2)) then
                if (lnn(L) > 0) THEN
                    inall = 1 ! todo: check als LNN(L) == 0
                    DO ip = 1,LNN(L)
                      n = netcell(LNE(ip,L))%n
                      XL = 0d0 ; YL = 0d0
                      DO ic = 1,n
                        XL = XL + XK(netcell(LNE(ip,L))%nod(ic))
                        YL = YL + YK(netcell(LNE(ip,L))%nod(ic))
                      end do
                      XL = XL / n
                      YL = YL / n

                      CALL DBPINPOL( XL, YL, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
                      IF (INALL == 1) then
                        inall = INHUL
                      end if
                    end do
                else
                    ! Rare case: 2D link without surrounding cells.
                    XL = 0.5D0*( XK(K1) + XK(K2) )
                    YL = 0.5D0*( YK(K1) + YK(K2) )
                    CALL DBPINPOL( XL, YL, inall,dmiss, JINS, NPL, xpl, ypl, zpl)
                end if
                IF (inall .EQ. 1) THEN
                    KN(1,L) = 0 ; KC(K1) = 0
                    KN(2,L) = 0 ; KC(K2) = 0
                ENDIF
            ELSE ! Old behaviour: just check by link mids.
                XL = 0.5D0*( XK(K1) + XK(K2) )
                YL = 0.5D0*( YK(K1) + YK(K2) )
                CALL DBPINPOL( XL, YL, INHUL,dmiss, JINS, NPL, xpl, ypl, zpl)
                IF (INHUL .EQ. 1) THEN
                    KN(1,L) = 0 ; KC(K1) = 0
                    KN(2,L) = 0 ; KC(K2) = 0
                ENDIF
            END IF
         ELSE IF (K1 .NE. 0) THEN
             XL = XK(K1)
             YL = YK(K1)
             CALL DBPINPOL( XL, YL, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
             IF (INHUL .EQ. 1) THEN
                 KN(1,L) = 0 ; KC(K1) = 0
                 KN(2,L) = 0 ; KC(K2) = 0
             ENDIF
         ELSE IF (K2 .NE. 0) THEN
             XL = XK(K2)
             YL = YK(K2)
             CALL DBPINPOL( XL, YL, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
             IF (INHUL .EQ. 1) THEN
                 KN(1,L) = 0 ; KC(K1) = 0
                 KN(2,L) = 0 ; KC(K2) = 0
             ENDIF
         ENDIF

      ENDDO

  else if (jacheckcells == 0) then    ! netnodes inside

      do k = 1,numk
         CALL DBPINPOL( Xk(k), Yk(k), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
         if (inhul == 1) then
            xk(k) = dmiss ; yk(k) = dmiss
         endif
      enddo

  else if (jacheckcells == 2) then

      call savepol()
      NPL = 0
      call findcells(0)
      call restorepol()

      kc = 0
      do k = 1,numk
         CALL DBPINPOL( Xk(k), Yk(k), INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
         if (inhul == 1) then
            kc(k) = 1
         endif
      enddo

      Lc    = 0
      do L  = 1,numL
         k1 = kn(1,L) ; k2 = kn(2,L)
         if (kc(k1) == 1 .and. kc(k2) == 1) then
            Lc(L) = 1
         endif
      enddo

      allocate (LC2(numL) ) ; Lc2(1:numL) = Lc(1:numL)

      do n = 1, nump

         nzero = 0
         do nn = 1,size(netcell(n)%lin)    ! check if any link should be kept for cell n
            L  = iabs(netcell(n)%lin(nn))
            if (L > 0) then
               if (Lc(L) == 0) then
                  nzero = 1 ; exit
               endif
            endif
         enddo

         if (nzero == 1) then         ! if it should be kept, flag all links of that cell to be kept.
            do nn = 1,size(netcell(n)%lin)
               L  = iabs(netcell(n)%lin(nn))
               if (L > 0) then
                  LC2(L) = 0
               endif
            enddo
         endif
      enddo

      do L  = 1,numL
         if (LC2(L) == 1) then
            kn(1,L) = 0 ; kn(2,L) = 0
         endif
      enddo

      deallocate (LC2)

  end if

  CALL SETNODADM(0)

  if ( jacheckcells == 0  .or. jacheckcells == 2) then

     do k = 1,numk
        if ( nmk(k) == 1 ) then
            L = nod(k)%lin(1)
            if (kn(3,L) == 2) then
                xk(k) = dmiss ; yk(k) = dmiss
            endif
        endif
     enddo

     CALL SETNODADM(0)

  endif

  CALL DELPOL()

  RETURN
  END SUBROUTINE DELNET
