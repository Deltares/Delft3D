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

!----------------- splrgfr stuff below
!      SUBROUTINE SPLRGFR (XSP, YSP, MCS, NCS,  X,  Y,  &
!                          MC, NC, MERR, MFAC, NFAC,    &
!                          IJC,IJYES,TIJ,XH,YH,         &
!                          X1,Y1,X2,Y2,X3,Y3,X4,Y4)
    SUBROUTINE SPLRGFR ()
!      USE DIMENS
    USE M_SPLINES
    use m_missing
    use m_grid
    use M_GRIDSETTINGS
    use m_netw, only: zkUNI
    use m_alloc
    implicit none

    double precision, allocatable, dimension(:,:) :: xspc, yspc, xh, yh, tij
    double precision, allocatable, dimension(:)   :: x1, x2, x3, x4, y1, y2, y3, y4
    integer,          allocatable, dimension(:,:) :: mn12
    double precision, allocatable, dimension(:)   :: xi1, yi1, ti1
    integer,          allocatable, dimension(:)   :: ntyp
    double precision, allocatable, dimension(:)   :: tt
! x, y -> xc, yc (m_grid)
! ijc, ijyes in m_grid
    CHARACTER TEX*6
    integer :: ierr, mspl
    integer :: imax, i1, ni1, nti1, l1max, jj, ii1, ii2, k, ii, i, j, L, ki, LJ, no, &
               numspl, numpx, numi, &
               ms, ns, &
               mcr, ncr, &
               mfa, nfa

    IF (MFAC > 1000) THEN
        CALL QNERROR('Please reduce MFAC and NFAC to about < 50 or so', ' ', ' ')
        return
    endif

    call increasegrid(mfac*mcs, nfac*mcs)

    imax = max( max(mfac,nfac)*mcs, maxsplen )  ! mnmax

    allocate ( xspc(mcs, maxsplen), yspc(mcs, maxsplen), TIJ(mcs, maxsplen), stat=ierr)
    call aerr('xspc(mcs, maxsplen), yspc(mcs, maxsplen), TIJ(mcs,maxsplen)', ierr, 3*mcs*maxsplen)
    allocate ( xh(mmax, nmax), yh(mmax, nmax), stat = ierr)
    call aerr('xh(mmax, nmax), yh(mmax, nmax)', ierr, 2*mmax*nmax)
    allocate(XI1(IMAX),      YI1(IMAX), TI1(IMAX),       &
              X1(IMAX),      Y1(IMAX),                  &
              X2(IMAX),      Y2(IMAX),                  &
              X3(IMAX),      Y3(IMAX),                  &
              X4(IMAX),      Y4(IMAX), TT(IMAX),        &
              stat=ierr)
    call aerr('XI1(imax),YI1(imax),TI1(imax), X/Y1..4(imax), TT(imax)', ierr, 11*imax)

    allocate ( NTYP(IMAX), MN12(IMAX,3), stat=ierr)
    call aerr('NTYP(IMAX), MN12(IMAX,3)', ierr, 2*imax)
    mn12 = 0
    ntyp = 0

    xspc = xymis
    yspc = xymis
    xspc(1:mcs,1:maxsplen) = xsp(1:mcs,1:maxsplen)
    yspc(1:mcs,1:maxsplen) = ysp(1:mcs,1:maxsplen)
    numspl = mcs
    !maxsplen = 2*maxsplen


    CALL READYY('TRANSFORMING SPLINES INTO DESIGN-GRID',0d0)
    ierr = 0

    CALL NEWFIL(mspl,'asave.spl')
    CALL writeSplines(mspl)

    CALL READYY(' ',0.05d0)

    CALL SECTR(    XSPc,    YSPc,    TIJ, mcs, maxsplen, imax, &
                   ierr,   NUMI,                               &
                 NUMSPL,  NUMPX,   NTYP, MN12,                 &
                  X1,Y1,X2,Y2)


      IF (ierr .LT. 1) THEN
         MS = 0
         NS = 0

        !  grof rooster aanmaken uit TIJ of nieuwe interpolatie
         CALL savegrd()
         Xc = dxymis
         Yc = dxymis
         zc = zkuni
!        vul voor alle splines de lijninterpolaties in
         DO 10 I1 = 1, NUMSPL
!           Alle horizontaaltjes
            CALL GETIJ(XSPc, XI1, mcs, maxsplen, imax,I1, I1, 1, NUMPX)
            CALL GETIJ(YSPc, YI1, mcs, maxsplen, imax,I1, I1, 1, NUMPX)
            CALL NUMPold (XSPc,  mcs, maxsplen, I1,NI1)
            CALL PAKTIJ(TIJ,mcs, maxsplen, TI1,imax,I1,I1,1,NUMSPL,NTI1)
            IF (I1 .LE. NUMI) THEN
               CALL MAKESPL(TI1,XI1,YI1,imax, NI1,NTI1,MFAC,X1,Y1,L1MAX,TT,0d0)
               JJ  = (MN12(I1,1) - 1)*NFAC + 1
               II1 = (MN12(I1,2) - 1)*MFAC + 1
               II2 = (MN12(I1,3) - 1)*MFAC + 1
            ELSE
               CALL MAKESPL(TI1,XI1,YI1,imax, NI1,NTI1,NFAC,X1,Y1,L1MAX,TT,0d0)
               JJ  = (MN12(I1,1) - 1)*MFAC + 1
               II1 = (MN12(I1,2) - 1)*NFAC + 1
               II2 = (MN12(I1,3) - 1)*NFAC + 1
            ENDIF
            K   = 0
            DO 20 II = II1,II2
               K        = K + 1
               IF (K .LE. L1MAX) THEN
                  IF (I1 .LE. NUMI) THEN
                     Xc(II,JJ) = X1(K)
                     Yc(II,JJ) = Y1(K)
                  ELSE
                     Xc(JJ,II) = X1(K)
                     Yc(JJ,II) = Y1(K)
                  ENDIF
               ENDIF
!              CALL RCIRC( X1(K),Y1(K) )
    20      CONTINUE
!           CALL TOEMAAR()
            IF (I1 .LE. NUMI) THEN
               NS = MAX( NS,MN12(I1,1) )
            ELSE
               MS = MAX( MS,MN12(I1,1) )
            ENDIF
    10   CONTINUE

         NCR = (NS - 1)*NFAC + 1
         MCR = (MS - 1)*MFAC + 1
         IF (MCR .GE. MMAX-1) THEN
             CALL READYY(' ',-1d0)
             CALL QNERROR('TOO MANY GRIDPOINTS IN', 'M-DIRECTION',' ')
             CALL RESTOREgrd()
             RETURN
         ENDIF
         IF (NCR .GE. NMAX-1) THEN
             CALL READYY(' ',-1d0)
             CALL QNERROR('TOO MANY GRIDPOINTS IN', 'N-DIRECTION',' ')
             CALL RESTOREgrd()
             RETURN
         ENDIF

!        CALL CLS1()
!        CALL TEKGRIDPUNTEN(X,Y,MCR,NCR)
!        CALL TOEMAAR()

         DO 30 I = 1, MS-1
            DO 30 J = 1, NS-1
               X1(2) = XYMIS
               X2(2) = XYMIS
               X3(2) = XYMIS
               X4(2) = XYMIS

               DO 40 K = 1,MFAC+1
                  DO 40 L = 1,NFAC+1
                     KI  = (I-1)*MFAC + K
                     LJ  = (J-1)*NFAC + L
                     IF (Xc(KI,LJ) .NE. XYMIS) THEN
                        IF (K .EQ. 1) THEN
                           X1(L) = Xc(KI,LJ)
                           Y1(L) = Yc(KI,LJ)
                        ENDIF
                        IF (K .EQ. MFAC+1) THEN
                           X2(L) = Xc(KI,LJ)
                           Y2(L) = Yc(KI,LJ)
                        ENDIF
                        IF (L .EQ. 1) THEN
                           X3(K) = Xc(KI,LJ)
                           Y3(K) = Yc(KI,LJ)
                        ENDIF
                        IF (L .EQ. NFAC+1) THEN
                           X4(K) = Xc(KI,LJ)
                           Y4(K) = Yc(KI,LJ)
                        ENDIF
                     ENDIF
    40         CONTINUE
               NO = 0
               IF (X1(2) .EQ. XYMIS) NO = 1
               IF (X2(2) .EQ. XYMIS) NO = 1
               IF (X3(2) .EQ. XYMIS) NO = 1
               IF (X4(2) .EQ. XYMIS) NO = 1
               IF (NO .EQ. 0) THEN
                  CALL TRANFN2(X1, X2, X3, X4,      &
                               Y1, Y2, Y3, Y4,      &
                               imax, mmax, nmax, XH, YH)
                  DO 60 K = 1,MFAC+1
                     DO 60 L = 1,NFAC+1
                        KI   = (I-1)*MFAC + K
                        LJ   = (J-1)*NFAC + L
                        IF (Xc(KI,LJ) .EQ. XYMIS) THEN
                           Xc(KI,LJ) = XH(K,L)
                           Yc(KI,LJ) = YH(K,L)
                        ENDIF
    60            CONTINUE
               ENDIF
    30   CONTINUE
         MC = MCR
         NC = NCR
!         CALL ISITU (Xc, Yc, MC, NC, IJC, IJYES)
      ENDIF

      CALL READYY(' ',-1d0)


    deallocate(xspc, yspc)
    call aerr('xspc, yspc', 0, -2*mmax*nmax) ! AvD: TODO
    deallocate(xh, yh, TIJ)
    call aerr('xh, yh, TIJ', 0, -3*mmax*nmax)
    deallocate(XI1, YI1, TI1,       &
               X1, Y1, X2, Y2, X3, Y3, X4, Y4, TT)
    call aerr('XI1, YI1, TI1, X/Y1..4, TT', 0, -11*imax)

    deallocate(NTYP, MN12)
    call aerr('NTYP, MN12', 0, -2*imax)

      RETURN
      END subroutine splrgfr
