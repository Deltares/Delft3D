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

module m_editflow
use m_write_flowdiff, only: write_flowdiff
use m_viewcycle
use m_typevalue
use m_textflow
use m_tekprofs
use m_selecteditmode
use m_plotklnup
use m_moveprobe
use m_minmxnds
use m_highlight_nodesnlinks
use m_getstring

implicit none

contains

     subroutine EDITflow(MODE, KEY, NL)
        use m_disnd
        use m_netw
        use m_flow
        use unstruc_colors
        use unstruc_api
        use m_snappol
        use dfm_error
        use unstruc_messages
        use gridoperations
        use unstruc_display, only: idisLink, dis_info_1d_link, nhlFlowLink
        use m_inquire_flowgeom
        use m_transport, only: ISALT, constituents
        use m_depmax
        use m_helpnow
        use m_qnerror
        use m_ktext
        use m_putget_un
        use m_botlin
        use m_getint
        use m_get_kbot_ktop
        use m_n_plot_plus_min
        use m_k_plot_plus_min
        use m_draw_nu
        use m_set_col
        use m_znod

        integer :: MODE, KEY, kb, kt, k, NL
        integer :: newmode
        integer :: ncol, nput
        integer :: KK = 0, LL, L
        integer :: num
        integer :: numb
        integer :: nwhat
        double precision :: xp, yp, zp

        integer :: i, Nin, Nout, ierror
        double precision, dimension(:), allocatable :: xin, yin, xout, yout ! testing, for snappol
        integer, dimension(:), allocatable :: ipoLout ! testing, for snappol

        character TEX * 26
        character(len=IdLen) :: strucid
        integer :: iresult

        TEX = ' Edit FLOW            '
        WRDKEY = TEX
        NLEVEL = 2
        NUM = 0
        NWHAT = 0
        NPUT = NL
        NUMB = 16
        NCOL = NCOLDN
        L = 0

        call SAVENET()

        call BOTLIN(0, NUMB, KEY)

10      continue
        call DRAWNU(KEY)
        call KTEXT(TEX, 1, 2, 15)
        call putget_un(NUM, NWHAT, NPUT, NUMB, XP, YP, KEY)

        call SETCOL(NCOLDN)
        if (NUM /= 0) then
!        ER IS EEN KEUZE
           if (NUM == 4) then
              MODE = NWHAT
              return
           else
              call CHOICES(NUM, NWHAT, KEY)
           end if
        else if (KEY >= 577) then ! Alt+letter switches edit mode.
           call selecteditmode(newmode, key)
           if (newmode > 0 .and. newmode /= mode) then
              mode = newmode
              return
           end if
        else if (KEY == 21) then
!        INS KEY OF LINKERMUIS, kijken welk punt

           ! key = 3
           if (NPUT == 51 .or. NPUT == 53 .or. NPUT == 54) then ! NODE mode
              call isflownode1D2D(xp, yp, KK)
              if (kk > 0) then
                 nplot = kk
                 call tekprofs()
                 call textflow()
              end if
              call DISND(KK, 1)
           else if (NPUT == 52 .or. NPUT == 57) then ! LINK mode
              call isflowlink(xp, yp, LL)

              if (nput == 57 .and. LL > 0) then
                 zp = iadv(LL)
                 call TYPEVALUE(zp, KEY)
                 iadv(LL) = int(zp)
              end if
              if (nput == 52 .and. LL > 0) then
                 call plotklnup(LL)

                 if (abs(kcu(LL)) /= 2) then
                    idisLink = LL ! Save the link index for later display
                    call dis_info_1d_link(LL)
                    nhlFlowLink = LL
                    call highlight_nodesnlinks()
                 end if
              end if

           end if

           if (NPUT == 53) then ! Click flow node to set min value for isocol
              KEY = 3
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
           else if (NPUT == 54) then ! Click flow node to set max value for isocol
              KEY = 3
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
           end if

        else if (KEY == 22) then
!        ENTER KEY ENKEL DISPLAY
           iresult = FLOW()
           if (iresult == DFM_SIGINT) then
              call mess(LEVEL_ERROR, 'Final handling of SIGINT signal. Stopping program.')
              call STOPINT()
           else if (iresult /= DFM_NOERR) then
              call qnerror('Error occurred while running, please inspect your diagnostic output.', ' ', ' ')
           end if
           key = 3
        else if (KEY == 23) then
!        ESCAPE KEY
           KEY = 3
        else if (KEY == 27) then
!        TAB
        else if (KEY == 78 .or. KEY == 78 + 32) then ! N-key voor node mode
           NPUT = 51
        else if (KEY == 76 .or. KEY == 76 + 32) then ! L-key voor link mode
           NPUT = 52
        else if (KEY == 73 .or. KEY == 73 + 32) then ! I-key voor setiadvec mode
           NPUT = 57
        else if (KEY == 77 + 32) then ! m (case sensitive!)
!        click flow node to set minimum for isocol
           NPUT = 53
        else if (KEY == 77) then ! M (case sensitive!)
!        click flow node to set maximum for isocol
           NPUT = 54
        else if (KEY == 86 .or. KEY == 86 + 32) then ! V-key
           call VIEWCYCLE(KEY)
        else if (KEY == 81 .or. KEY == 81 + 32) then ! Q-key stop flow info screen display for 1D flowlink
           idisLink = 0
           nhlFlowLink = 0
           key = 3
        else if (KEY == 72 .or. KEY == 72 + 32) then ! H-key search for a hydraulic structure
           call getstring(' SEARCH: structure id = ', strucid)
           iresult = findlink(strucid, L)
           if (L > 0 .and. L <= lnx) then
              nhlFlowLink = L
              call highlight_nodesnlinks()
           end if
        else if (KEY == 70 .or. KEY == 70 + 32) then ! F-key search for a flowlink
           call GETINT(' SEARCH: flowlink =  ', L)
           if (L > 0 .and. L <= lnx) then
              nhlFlowLink = L
              call highlight_nodesnlinks()
           end if
        else if (KEY == 83 .or. KEY == 83 + 32) then ! S-key add salt
           if (jasal > 0) then
              call getkbotktop(nplot, kb, kt)
              k = kb + kplot - 1
              constituents(isalt, k) = constituents(isalt, k) + 1d0
           end if
        else if (KEY == 43 .or. KEY == 140) then ! -
           call KPLOTPLUSMIN(-1)
           key = 3
        else if (KEY == 45 .or. KEY == 141) then ! +
           call KPLOTPLUSMIN(1)
           key = 3
        else if (KEY == 42) then ! *
           call nPLOTPLUSMIN(1)
           key = 3
        else if (KEY == 47) then ! /
           call nPLOTPLUSMIN(-1)
           key = 3
        else if (KEY == 32) then
           call flow_spatietimestep()
           key = 3
        else if (KEY == 119 .or. KEY == 119 - 32) then ! w key write diff with obs
           call write_flowdiff()
        else if (KEY == 81 .or. KEY == 81 + 32) then ! Q-key: snap polygon to flow network
           Nin = NPL
           allocate (xin(Nin), yin(Nin))
           do i = 1, Nin
              xin(i) = XPL(i)
              yin(i) = YPL(i)
           end do
           !call snappol(Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
           !call snappnt(Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
           if (KEY == 81) then
              call snapbnd('dischargebnd', Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
           else
              call snapbnd('waterlevelbnd', Nin, xin, yin, DMISS, Nout, Xout, Yout, ipoLout, ierror)
           end if
           NPL = Nout
           call increasepol(NPL, 0)
           do i = 1, Nout
              XPL(i) = xout(i)
              YPL(i) = yout(i)
              ZPL(i) = dble(ipoLout(i))
           end do
           if (allocated(xin)) deallocate (xin, yin)
           if (allocated(xout)) deallocate (xout, yout)
           if (allocated(ipoLout)) deallocate (ipoLout)

        else if (key >= 49 .and. key <= 57) then ! keypad, for moving around
           call moveprobe(key - 48, kk, xp, yp)
           if (kk > 0) then
              nplot = kk
              call tekprofs()
              call textflow()
           end if
           call DISND(KK, 1)
        end if
!
        goto 10
!
     end subroutine EDITflow

end module m_editflow
