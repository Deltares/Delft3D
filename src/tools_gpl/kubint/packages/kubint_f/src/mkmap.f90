subroutine mkmap(code      ,x1        ,y1        ,m1        ,n1        , &
               & x2        ,y2        ,n2        ,xs        ,ys        , &
               & nrx       ,nry       ,iflag     ,nrin      ,w         , &
               & iref      ,iprint    ,lunlog)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_kubint
!
! Global variables
!
    integer                    , intent(in)  :: iprint
    integer                    , intent(in)  :: lunlog
    integer                    , intent(in)  :: m1
    integer                    , intent(in)  :: n1
    integer                                  :: n2
    integer , dimension(4, n2) , intent(out) :: iref
    integer , dimension(m1,n1) , intent(in)  :: code
    integer , dimension(n2)                  :: iflag
    integer , dimension(n2)                  :: nrin
    integer , dimension(n2)                  :: nrx
    integer , dimension(n2)                  :: nry
    real(hp), dimension(4, n2)               :: w
    real(hp), dimension(m1, n1), intent(in)  :: x1
    real(hp), dimension(m1, n1), intent(in)  :: y1
    real(hp), dimension(n2)                  :: x2
    real(hp), dimension(n2)                  :: xs
    real(hp), dimension(n2)                  :: y2
    real(hp), dimension(n2)                  :: ys
!
!
! Local variables
!
    integer                        :: i
    integer                        :: i1
    integer                        :: i2
    integer                        :: ier
    integer                        :: iin
    integer                        :: inout
    integer                        :: ip
    integer                        :: ipt
    integer                        :: j1
    integer                        :: lomaxx
    integer                        :: lominx
    integer                        :: lomnx
    integer                        :: lomny
    integer                        :: m1max
    integer                        :: m1min
    integer                        :: n1max
    integer                        :: n1min
    integer                        :: nin
    real(hp)                       :: xpmax
    real(hp)                       :: xpmean
    real(hp)                       :: xpmin
    real(hp)                       :: ypmax
    real(hp)                       :: ypmean
    real(hp)                       :: ypmin
    real(hp), dimension(5)         :: xp
    real(hp), dimension(5)         :: yp
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !     SUBROUTINE MKMAP
    !     Interpolation of curvilinear, numerically ordered grid (grid1)
    !     to random points, with weighting points (grid2).
    !     Deduced from routine MATRANT
    !
    !     J.A. Roelvink
    !     Deltares
    !     14-5-1991 (MATRANT)
    !     24-2-1992 (MKMAP
    !
    !     Given: numerically ordered grid M1*N1
    !     with coordinates X1 (1:M1,1:N1)
    !                  and Y1 (1:M1,1:N1)
    !
    !     Also given: random points X2(1:N2)
    !                           and Y2(1:N2)
    !
    !     To be determined:  weighting factors and pointers for bilinear interpolation
    !     Weighting factors and locations of the requested (random) points in the
    !     ordered grid are saved in resp.
    !     W(1:4,1:N2) and Iref(1:4,1:N2)
    !
    !     Initialise tables
    !
    if (iprint==1) write (*, *) 'in mkmap', m1, n1, n2
    lomnx = 1
    lomny = 1
    m1min = 1
    m1max = m1
    n1min = 1
    n1max = n1
    do i2 = 1, n2
       nrx(i2)   = 0
       nry(i2)   = 0
       iflag(i2) = 0
       nrin(i2)  = 0
       xs(i2)    = 0.
       ys(i2)    = 0.
       do ipt = 1, 4
          iref(ipt, i2) = 0
          w(ipt, i2)    = 0.
       enddo
    enddo
    !
    ! Sort X2 en Y2
    !
    call sort(n2        ,x2        ,xs        ,nrx       )
    call sort(n2        ,y2        ,ys        ,nry       )
    !
    ! Loop over all cels of grid1
    !
    do j1 = n1min, n1max - 1
       do i1 = m1min, m1max - 1
          !
          ! Check for points with 'nonsense' co-ordinates in grid1;
          ! this depends on agreements made
          !
          if (code(i1    , j1    )>=0 .and. code(i1 + 1, j1    )>=0 .and. &
              code(i1 + 1, j1 + 1)>=0 .and. code(i1    , j1 + 1)>=0       ) then
             !
             ! Cell definition
             !
             xp(1) = x1(i1, j1)
             xp(2) = x1(i1 + 1, j1)
             xp(3) = x1(i1 + 1, j1 + 1)
             xp(4) = x1(i1, j1 + 1)
             yp(1) = y1(i1, j1)
             yp(2) = y1(i1 + 1, j1)
             yp(3) = y1(i1 + 1, j1 + 1)
             yp(4) = y1(i1, j1 + 1)
             !
             ! Determine minimum and maximum X and Y of the cell
             !
             xpmin =  1.D30
             xpmax = -1.D30
             ypmin =  1.D30
             ypmax = -1.D30
             do ip = 1, 4
                xpmin = min(xp(ip), xpmin)
                xpmax = max(xp(ip), xpmax)
                ypmin = min(yp(ip), ypmin)
                ypmax = max(yp(ip), ypmax)
             enddo
             xpmean = .5*(xpmin + xpmax)
             ypmean = .5*(ypmin + ypmax)
             !
             ! First selection of points of grid2 that can be located in the cell
             !
             ! Find centre of the cell in tables Xs and Ys
             !
             call hunt(xs        ,n2        ,xpmean    ,lomnx     )
             call hunt(ys        ,n2        ,ypmean    ,lomny     )
             !
             ! For points with X-values between Xpmin and Xpmax set: iFlag(i)=1
             !
             lominx = lomnx
             lomaxx = lomnx
             do i = lomnx, 1, -1
                if (xs(i)>=xpmin) then
                   lominx        = i
                   iflag(nrx(i)) = 1
                else
                   exit
                endif
             enddo
             do i = lomnx + 1, n2
                if (xs(i)<=xpmax) then
                   lomaxx        = i
                   iflag(nrx(i)) = 1
                else
                   exit
                endif
             enddo
             !
             ! For the points with Y-values between Ypmin and Ypmax,
             ! that also lie between Xpmin and Xpmax: Save them in NrIn
             !
             iin = 1
             do i = lomny, 1, -1
                if (ys(i)>=ypmin) then
                   nrin(iin) = nry(i)*iflag(nry(i))
                   iin       = iin + iflag(nry(i))
                else
                   exit
                endif
             enddo
             do i = lomny + 1, n2
                if (ys(i)<=ypmax) then
                   nrin(iin) = nry(i)*iflag(nry(i))
                   iin       = iin + iflag(nry(i))
                else
                   exit
                endif
             enddo
             nin = iin - 1
             !
             ! Put iFlag back to 0
             !
             do i = lominx, lomaxx
                if (i/=0) iflag(nrx(i)) = 0
             enddo
             !
             ! Check whether selected points of grid2 lie within the cell
             ! using subroutine IPON; if so, determine weights W of the surrounding
             ! values in grid1 using subroutine INTRP4. Save the weights in Wtab
             ! The reference to grid1 is saved in arrays Iref and Jref.
             !
             do iin = 1, nin
                i2 = nrin(iin)
                inout = -1
                call ipon(xp        ,yp        ,4         ,x2(i2)    , &
               &          y2(i2)    ,inout     , lunlog)
                if (inout>=0) then
                   call bilin5(xp       ,yp       ,x2(i2)   ,y2(i2)  , &
               &               w(1, i2) ,ier       )
                   iref(1, i2) = i1 + (j1 - 1)*m1
                   iref(2, i2) = i1 + 1 + (j1 - 1)*m1
                   iref(3, i2) = i1 + 1 + j1*m1
                   iref(4, i2) = i1 + j1*m1
                endif
             enddo
          endif
       enddo
    enddo
end subroutine mkmap
