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

module m_textflow
use m_model_specific, only: textflowspecific

implicit none

contains

 subroutine TEXTFLOW()
    use m_setxor
    use m_ictext
    use time_module, only: seconds_to_datetimestring
    use m_flowgeom, only: ndx, lnx
    use Timers
    use m_flow
    use m_flowtimes
    use m_reduce, only: nocg, nogauss
    use m_raaitek
    use unstruc_model, only: md_ident
    use m_transport, only: nsubsteps, numnonglobal
    use m_drawthis
    use m_get_link1
    use m_zlin
    use m_znod
    implicit none
    double precision :: cpuperstep, solrest, znn, dtav
    double precision :: tsteps, tsol, tstepinc
    integer :: nn, LL, nl
    character TEX * 210
    character, save :: TEX1 * 210 = '@'
    character, save :: TEX2 * 210 = ''
    character, save :: TEX3 * 210 = ''
    character(len=4) :: c_nsubsteps
    character(len=7) :: c_numnonglobal
    character(len=15) :: c_lts

    if (jtextflow < 1) return

    if (ndx < 1) return

! erase previous text
    if (trim(tex1) /= '@') then
       call setxor(0)
       call ITEXTCOLOUR('BWHITE', 'BBLUE')
       call IClearLine(2)
       call IClearLine(3)
       call IClearLine(4)
       call IClearLine(5)
    end if

! call setxor(1)

    TEX = ' '
    solrest = 0
    tsteps = tim_get_wallclock(handle_steps)
    tsol = tim_get_wallclock(handle_sol)
    if (tsteps - tsol /= 0) solrest = tsol / (tsteps - tsol)
    tstepinc = tim_get_wallclock_inc(handle_steps)
    cpuperstep = max(0d0, min(100d0, tstepinc))

    call seconds_to_datetimestring(tex, refdat, time1)

    dtav = (time1 - tstart_user) / max(1d0, dnt)

    write (TEX(18:), '( A4, F8.3, A8, F7.3, A10, F7.3, A5, F8.1, A, E9.2, A8, E15.8, A8, E15.8)') &
       'dt: ', dts, ' Avg.dt: ', dtav, ' CPU/step: ', cpuperstep, ' Tot: ', tsteps, &
        ' Sol/Rest:', solrest, ' Samer: ', samerr, ' Samtot: ', sam1tot
    call ICTEXT(trim(TEX), 13, 2, 221)
    TEX1 = TEX

    nn = min(nplot, ndx)
    TEX = ' '
    if (ndraw(29) < 2) then
       znn = znod(nn)
       write (TEX, '( A, I3, I6, A, e15.8, A, e15.8, A, e15.8, A, I6, A, I10, A, I10)') &
          'k/nplot: ', KPLOT, nplot, ' znod(nn): ', znn, ' Vol1: ', vol1tot, ' Vler: ', volerrcum, &
          ' #setb: ', int(dsetb), ' #dt: ', int(dnt), ' #itsol: ', itsol
    else
       call getlink1(nn, nl)
       znn = zlin(nl)
       write (TEX, '(A9,I3,1X,I6,1X,A,e15.8,1X,A10,e15.8,1X,A8,e15.8,1X,A7,I6,1X,A5,I10,1X,A8,I5)') &
          'k/nplot: ', KPLOT, nplot, 'zlin(nn): ', znn, 'Vol1: ', vol1tot, 'Vler: ', volerrcum, &
          '#setb: ', int(dsetb), '#dt: ', int(dnt), '#itsol: ', itsol
    end if

    call ICTEXT(trim(TEX), 13, 3, 221)
    TEX2 = TEX

    TEX = ' '

! make string for local time-stepping
    if (nsubsteps == 1) then
       write (c_lts, "(15A)") ' '
       if (nonlin >= 2) then
          c_lts = '#s1mit: '
          write (c_lts(9:), '(i4)') min(9999, nums1mit)
       end if
    else
       write (c_nsubsteps, "(i4)") min(nsubsteps, 9999) ! min: safe text width
       write (c_numnonglobal, "(i7)") min(numnonglobal, 9999999) ! min: safe text width
       c_lts = 'lts:'//trim(adjustl(c_nsubsteps))//'|'//trim(adjustl(c_numnonglobal))
    end if

    if (kmx == 0) then
       write (TEX, '( A,i8,  A,I8,  A,I4,  A,I8,1  A,I8,  A,I4, A, I2.0, I1, I1, I1, I1, A, A, A15 )') &
          '#ndx: ', ndx, ' #lnx: ', lnx, ' #kmx : ', kmx, ' #CG: ', nocg, ' #Gauss: ', nogauss, &
          ' #s1it: ', min(9999, nums1it), ' iad: ', iadvec, limtypmom, limtypsa, javasal, javau, ' runid: '//trim(md_ident), ' ', c_lts
    else
       call getlink1(nn, LL)
       write (TEX, '( A,i8,  A,I8,  A,I4,  A, F8.5, A, F8.5,  A,I4, A, I2.0, I1, I1, I1, I1, A, A, A14)') &
          '#ndx: ', ndx, ' #lnx: ', lnx, ' #kmx : ', kmx, ' ustB ', min(ustb(LL), 1d2), ' ustW ', ustw(LL), &
          ' #s1it: ', min(9999, nums1it), ' iad: ', iadvec, limtypmom, limtypsa, javasal, javau, ' runid: '//trim(md_ident), ' ', c_lts
    end if

    call ICTEXT(trim(TEX), 13, 4, 221)
    TEX3 = TEX

    call setxor(0)

    call textflowspecific()

    return
 end subroutine

end module m_textflow
