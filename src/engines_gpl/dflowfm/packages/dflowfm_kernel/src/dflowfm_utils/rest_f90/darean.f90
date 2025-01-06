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

module m_darean

implicit none

private

public :: darean

contains

 !> Computes the enclosed area and length of a polygon.
 !!
 !! Only the first polygon is considered; whenever a missing value
 !! is encountered, the polygon is 'closed'.
 subroutine dAREAN(XX, YY, N, DAREA, DLENGTH, DLENMX)
    use precision, only: dp
    use m_missing
    use m_sferic
    use geometry_module, only: dbdistance, get_startend, comp_masscenter

    integer, intent(in) :: n !< Nr. of polygon points.
    real(kind=dp), intent(in) :: XX(N), YY(N) !< Polygon points.
    real(kind=dp), intent(out) :: DAREA !< Area enclosed within polygon.
    real(kind=dp), intent(out) :: DLENGTH !< Length of polygon contour.
    real(kind=dp), intent(out) :: DLENMX !< Length of longest segment in polygon contour.

    integer :: i, iu, nend, jstart, jend
    real(kind=dp) :: Y0, DLE
    real(kind=dp) :: xcg, ycg
    integer :: jacounterclockwise
    DAREA = 0d0
    DLENGTH = 0d0
    Y0 = 1d30
    NEND = 0
    DLENMX = 0.d0

    call get_startend(N, XX, YY, jstart, jend, dmiss)

    if (jend <= jstart) return

    call comp_masscenter(jend - jstart + 1, xx(jstart), yy(jstart), xcg, ycg, darea, jacounterclockwise, jsferic, jasfer3D, dmiss)

    !DO I  = jstart,jend
    !   IF (XX(I) .NE.  dXYMIS) THEN
    !      Y0   = MIN(Y0,YY(I))
    !      NEND = I
    !   ELSE
    !      ! dmiss encountered: end of first polygon.
    !      ! Only compute area for first polygon.
    !      EXIT
    !   ENDIF
    !ENDDO
    !
    do I = jstart, jend
       IU = I + 1

       if (iu > jend) iu = jstart

       !     IF (IU .GT. NEND) IU = 1
       !     IF (JSFERIC .EQ. 0) THEN
       !        DX    = ( XX(IU) - XX(I) )
       !        Y     = 0.5d0*(YY(IU)-Y0) + 0.5d0*(YY(I) -Y0)
       !     ELSE
       !        DX    = ( XX(IU) - XX(I) )*DG2RD*RA*COS( DG2RD*( YY(IU)+YY(I) )/2 )
       !        Y     = RA*DG2RD*(0.5d0*( YY(IU) + YY(I) )-Y0)
       !     ENDIF
       !     DAREA    = DAREA - DX*Y
       DLE = DBDISTANCE(XX(I), YY(I), XX(IU), YY(IU), jsferic, jasfer3D, dmiss)
       DLENGTH = DLENGTH + DLE
       DLENMX = max(DLENMX, DLE)

    end do
    !
    !DAREA = ABS(DAREA)
    return
 end subroutine dAREAN

end module m_darean
