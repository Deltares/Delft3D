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

     SUBROUTINE EDITflow(MODE,KEY,NL)
      use m_netw
      use m_flowgeom, only : iadv, kcu
      use m_flow
      use unstruc_colors
      USE M_MISSING
      use unstruc_api
      use m_snappol
      use dfm_error
      use unstruc_messages
      use gridoperations
      use unstruc_display, only: idisLink, dis_info_1d_link, nhlFlowLink
      use m_inquire_flowgeom
      use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, ITRAN0, constituents, itrac2const, const_names, const_units

      implicit none
      integer :: MODE, KEY, kb , kt ,k, NL
      integer :: newmode
      integer :: ncol, nput
      integer :: nlevel
      integer :: KK=0, LL, L
      integer :: num
      integer :: numb
      integer :: nwhat
      double precision :: xp, yp, zp, ZNOD

      double precision :: vmax, vmin, dv, val
      integer          :: ncols, nv, nis, nie, jaauto

      integer                                     :: i, Nin, Nout, ierror
      double precision, dimension(:), allocatable :: xin, yin, xout, yout  ! testing, for snappol
      integer,          dimension(:), allocatable :: ipoLout    ! testing, for snappol

      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO


      COMMON /HELPNOW/ WRDKEY,NLEVEL

      CHARACTER TEX*26, WRDKEY*40
      character(len=IdLen) :: strucid
      integer :: iresult

      TEX    = ' Edit FLOW            '
      WRDKEY = TEX
      NLEVEL =  2
      NUM    =  0
      NWHAT  =  0
      NPUT   =  NL
      NUMB   =  16
      NCOL   =  NCOLDN
      L      =  0

      CALL SAVENET()

      CALL BOTLIN(0,NUMB,KEY)

   10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      CALL SETCOL(NCOLDN)
      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY OF LINKERMUIS, kijken welk punt

         ! key = 3
         IF (NPUT .EQ. 51 .or. NPUT == 53 .or. NPUT == 54) THEN   ! NODE mode
            call isflownode1D2D(xp, yp, KK)
            if (kk > 0) then
               nplot = kk
               call tekprofs()
               call textflow()
            endif
            CALL DISND(KK, 1)
         ELSE IF (NPUT .EQ. 52 .or. NPUT .EQ. 57 ) THEN   ! LINK mode
            call isflowlink(xp, yp, LL)

            if (nput == 57 .and. LL > 0 ) then
                zp = iadv(LL)
                CALL TYPEVALUE(zp,KEY)
                iadv(LL) = int(zp)
            endif
            if ( nput.eq.52 .and. LL.gt.0 ) then
               call plotklnup(LL)

               if (abs(kcu(LL)) /= 2) then
                  idisLink = LL ! Save the link index for later display
                  call dis_info_1d_link(LL)
                  nhlFlowLink = LL
                  call highlight_nodesnlinks()
               end if
            end if

         ENDIF

         IF ( NPUT .EQ. 53 ) THEN ! Click flow node to set min value for isocol
            KEY  = 3
            if (KK == 0) then ! Miss click: reset iscol scaling to auto.
                jaauto = 1
            else
                vmin = znod(KK)
                jaauto = 0
                if (vmin > vmax) then
                    key = 0
                end if
            end if
            call minmxnds()
         ELSE IF ( NPUT .EQ. 54 ) THEN ! Click flow node to set max value for isocol
            KEY  = 3
            if (KK == 0) then ! Miss click: reset iscol scaling to auto.
                jaauto = 1
            else
                vmax = znod(KK)
                jaauto = 0
                if (vmin > vmax) then
                    key = 0
                end if
            end if
            call minmxnds()
         ENDIF

      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY ENKEL DISPLAY
         iresult = FLOW()
         if (iresult == DFM_SIGINT) then
            call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
            call STOPINT()
         else if (iresult /= DFM_NOERR) then
            call qnerror('Error occurred while running, please inspect your diagnostic output.',' ', ' ')
         end if
         key = 3
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         KEY   = 3
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
      ELSE IF (KEY .EQ. 78 .OR. KEY .EQ. 78+32) THEN    ! N-key voor node mode
         NPUT = 51
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN    ! L-key voor link mode
         NPUT = 52
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN    ! I-key voor setiadvec mode
         NPUT = 57
      ELSE IF (                 KEY .EQ. 77+32) THEN    ! m (case sensitive!)
!        click flow node to set minimum for isocol
         NPUT = 53
      ELSE IF (KEY .EQ. 77                    ) THEN    ! M (case sensitive!)
!        click flow node to set maximum for isocol
         NPUT = 54
      ELSE IF (KEY .EQ. 86 .OR. KEY .EQ. 86+32) THEN    ! V-key
         CALL VIEWCYCLE(KEY)
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN    ! Q-key stop flow info screen display for 1D flowlink
         idisLink = 0
         nhlFlowLink = 0
         key = 3
      else if (KEY == 72 .or. KEY == 72+32) then        ! H-key search for a hydraulic structure
         call getstring(' SEARCH: structure id = ', strucid)
         iresult = findlink(strucid, L)
         if (L > 0 .and. L <= lnx) then
            nhlFlowLink = L
            call highlight_nodesnlinks()
         end if
      else if (KEY == 70 .or. KEY == 70+32) then        ! F-key search for a flowlink
         call GETINT(' SEARCH: flowlink =  ', L)
         if (L > 0 .and. L <= lnx) then
            nhlFlowLink = L
            call highlight_nodesnlinks()
         end if
      ELSE IF (KEY .EQ. 83 .OR. KEY .EQ. 83+32) THEN    ! S-key add salt
         if (jasal > 0) then
            call getkbotktop(nplot,kb , kt )
            k = kb + kplot - 1
            constituents(isalt,k) = constituents(isalt,k) + 1d0
         endif
      ELSE IF (KEY .EQ. 43 .or. KEY .EQ. 140) THEN     ! -
         CALL KPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (KEY .EQ. 45 .or. KEY .EQ. 141) THEN      ! +
         call KPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 42) THEN                        ! *
         CALL nPLOTPLUSMIN(1)
         key = 3
      ELSE IF (KEY .EQ. 47) THEN                        ! /
         call nPLOTPLUSMIN(-1)
         key = 3
      ELSE IF (KEY .EQ. 32) THEN
         call flow_spatietimestep()
         key = 3
      ELSE IF (KEY .EQ. 119 .or. KEY .EQ. 119-32) then ! w key write diff with obs
         call write_flowdiff()
      ELSE IF (KEY .EQ. 81 .OR. KEY .EQ. 81+32) THEN    ! Q-key: snap polygon to flow network
         Nin = NPL
         allocate(xin(Nin), yin(Nin))
         do i=1,Nin
            xin(i) = XPL(i)
            yin(i) = YPL(i)
         end do
         !call snappol(Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         !call snappnt(Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         if ( KEY.eq.81 ) then
            call snapbnd('dischargebnd', Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         else
            call snapbnd('waterlevelbnd', Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
         end if
         NPL = Nout
         call increasepol(NPL,0)
         do i=1,Nout
            XPL(i) = xout(i)
            YPL(i) = yout(i)
            ZPL(i) = dble(ipoLout(i))
         end do
         if ( allocated(xin)     ) deallocate(xin, yin)
         if ( allocated(xout)    ) deallocate(xout, yout)
         if ( allocated(ipoLout) ) deallocate(ipoLout)

      else if ( key.ge.49 .and. key.le.57 ) then   ! keypad, for moving around
         call moveprobe(key-48,kk,xp,yp)
         if (kk > 0) then
            nplot = kk
            call tekprofs()
            call textflow()
         endif
         CALL DISND(KK, 1)
      ENDIF
!
      GOTO 10
!
      END SUBROUTINE EDITflow
