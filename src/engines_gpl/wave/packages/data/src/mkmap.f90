subroutine mkmap(code      ,x1        ,y1        ,m1        ,n1        , &
               & x2        ,y2        ,n2        ,xs        ,ys        , &
               & nrx       ,nry       ,iflag     ,nrin      ,w         , &
               & iref      ,iprint    ,covered   ,xymiss)
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
    use precision_basics
    use buffer
    use stdlib_sorting, only: sort_index
    !
    implicit none
!
! Global variables
!
    integer                    , intent(in)  :: iprint
    integer                    , intent(in)  :: m1
    integer                    , intent(in)  :: n1
    integer                                  :: n2
    integer , dimension(4 , n2), intent(out) :: iref
    integer , dimension(m1, n1), intent(in)  :: code
    integer , dimension(n2)                  :: covered !  0: target point is not covered by source grid (default)
                                                        !  1: target point is covered by   valid points of source grid
                                                        ! -1: target point is covered by invalid points of source grid
    integer , dimension(n2)                  :: iflag
    integer , dimension(n2)                  :: nrin
    integer , dimension(n2)                  :: nrx
    integer , dimension(n2)                  :: nry
    real(hp)                   , intent(in)  :: xymiss  ! missing value in grid 1
    real    , dimension(4 , n2)              :: w       ! Remains single precision!
    real(hp), dimension(m1, n1), intent(in)  :: x1
    real(hp), dimension(m1, n1), intent(in)  :: y1
    real(hp), dimension(n2)                  :: x2
    real(hp), dimension(n2)                  :: xs
    real(hp), dimension(n2)                  :: y2
    real(hp), dimension(n2)                  :: ys
!
! Local variables
!
    integer                           :: i
    integer                           :: i1
    integer                           :: i2
    integer                           :: ier
    integer                           :: iin
    integer                           :: inout
    integer                           :: ip
    integer                           :: j1
    integer                           :: lomaxx
    integer                           :: lominx
    integer                           :: lomnx
    integer                           :: lomny
    integer                           :: m1max
    integer                           :: m1min
    integer                           :: n1max
    integer                           :: n1min
    integer                           :: nin
    real(hp)                          :: eps
    real(hp)                          :: xpmax
    real(hp)                          :: xpmean
    real(hp)                          :: xpmin
    real(hp)                          :: ypmax
    real(hp)                          :: ypmean
    real(hp)                          :: ypmin
    real(hp), dimension(5)            :: xp
    real(hp), dimension(5)            :: yp
    real(hp), dimension(:), pointer   :: hbuff
!
!! executable statements -------------------------------------------------------
!
    eps = 0.00001_hp
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
    nrx  = 0
    nry = 0
    iflag = 0
    nrin = 0
    xs = x2
    ys = y2
    iref = 0
    w = 0.

    call sort_index(xs, nrx)
    call sort_index(ys, nry)
    !
    ! Loop over all cels of grid1
    !
    ! Allocate buffer for the 2d double precision array for the weighting factors
    ! needed in bilin5
    !
    call get_buffer(hbuff, 4)
    !  
    do j1 = n1min, n1max - 1
       do i1 = m1min, m1max - 1
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
          if (     (xp(1) > xymiss-eps .and. xp(1) < xymiss+eps) &
            & .or. (xp(2) > xymiss-eps .and. xp(2) < xymiss+eps) &
            & .or. (xp(3) > xymiss-eps .and. xp(3) < xymiss+eps) &
            & .or. (xp(4) > xymiss-eps .and. xp(4) < xymiss+eps) &
            & .or. (yp(1) > xymiss-eps .and. yp(1) < xymiss+eps) &
            & .or. (yp(2) > xymiss-eps .and. yp(2) < xymiss+eps) &
            & .or. (yp(3) > xymiss-eps .and. yp(3) < xymiss+eps) &
            & .or. (yp(4) > xymiss-eps .and. yp(4) < xymiss+eps) ) cycle
          !
          ! Determine minimum and maximum X and Y of the cell
          !
          xpmin =  1.e10_hp
          xpmax = -1.e10_hp
          ypmin =  1.e10_hp
          ypmax = -1.e10_hp
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
            &          y2(i2)    ,inout     )
             if (inout>=0) then
                !
                ! Check on point with nonsense information on source grid 1;
                ! This depends on agreements
                !
                if (      (code(i1    , j1    )==1 .or. code(i1    , j1    )==-1 .or. code(i1    , j1    )==3) &
                  & .and. (code(i1 + 1, j1    )==1 .or. code(i1 + 1, j1    )==-1 .or. code(i1 + 1, j1    )==3) &
                  & .and. (code(i1 + 1, j1 + 1)==1 .or. code(i1 + 1, j1 + 1)==-1 .or. code(i1 + 1, j1 + 1)==3) &
                  & .and. (code(i1    , j1 + 1)==1 .or. code(i1    , j1 + 1)==-1 .or. code(i1    , j1 + 1)==3) &
                  & .and. (code(i1    , j1    )==1 .or. code(i1 + 1, j1    )==1 .or. code(i1 + 1, j1 + 1)==1 .or. code(i1    , j1 + 1)==1)) then
                   !
                   ! grid2 point is covered by 4 valid grid1 points
                   !
                   covered(i2) = 1
                   call bilin5(xp, yp, x2(i2), y2(i2), hbuff, ier)
                   w(:, i2) = real(hbuff(:), sp)
                   !
                   iref(1, i2) = i1 + (j1 - 1)*m1
                   iref(2, i2) = i1 + 1 + (j1 - 1)*m1
                   iref(3, i2) = i1 + 1 + j1*m1
                   iref(4, i2) = i1 + j1*m1
                elseif (code(i1, j1)/=0 .and. code(i1 + 1, j1)/=0 .and. &
                  & code(i1 + 1, j1 + 1)/=0 .and. code(i1, j1 + 1)/=0) then
                   !
                   ! Grid2 point is covered by 4 valid grid1 points, containing
                   ! one or more boundary points:
                   ! - do not use grid1 values
                   ! - use extrapolation values
                   !
                   covered(i2) = 0
                else
                   !
                   ! Grid2 point is not covered by 4 valid grid1 points:
                   ! - do not use grid1 values
                   ! - do not use extrapolation values
                   !
                   covered(i2) = -1
                endif
             endif
          enddo
       enddo
    enddo
    !
    ! Deallocate buffer
    !
    call dealloc_buffer
end subroutine mkmap
