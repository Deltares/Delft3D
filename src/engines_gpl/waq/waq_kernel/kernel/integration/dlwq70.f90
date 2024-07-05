!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_dlwq70
    use m_waq_precision
    implicit none

    contains

    !> Fills matrix according to central differencing in space.
    subroutine dlwq70(disp,   disper, area  , flow , aleng , & 
                      velo,   bound, ipoint, notot, isys  , & 
                      nsys,   noq1,  noq2  , noq  , nodisp, & 
                      novelo, idpnt,   ivpnt , deriv, amat  , & 
                      jtrack, integration_id, ilflag )

        use timers

        real(kind=real_wp), intent(in   ) :: disp(3)   !< Dispersion in 3 directions
        real(kind=real_wp), intent(in   ) :: disper(*) !< Additional dispersion (NODISP*NOQ)
        real(kind=real_wp), intent(in   ) :: flow(*)   !< Flows accross exchange surfaces (noq)
        real(kind=real_wp), intent(in   ) :: area(*)   !< Exchange surface area (noq)
        real(kind=real_wp), intent(in   ) :: aleng(*)  !< From- and to lengths (2*NOQ)
        real(kind=real_wp), intent(in   ) :: velo(*)   !< Additional velocity (NOVELO*NOQ)
        real(kind=real_wp), intent(in   ) :: bound(*)  !< Boundary concentrations (NOTOT*?)
        real(kind=real_wp), intent(inout) :: amat(*)   !< Matrix to be updated
        real(kind=real_wp), intent(  out) :: deriv(*)  !< Derivatives (NOTOT*NOSEG)

        integer(kind=int_wp), intent(in   ) :: ipoint(4,*)    !< Indices ("pointers") for exchanges (4*NOQ)
        integer(kind=int_wp), intent(in   ) :: idpnt(*)       !< Indices ("pointers") for additional dispersions (NOSYS)
        integer(kind=int_wp), intent(in   ) :: ivpnt(*)       !< Indices ("pointers") for additional velocities (NOSYS)
        integer(kind=int_wp), intent(in   ) :: isys           !< Index of system currently considered
        integer(kind=int_wp), intent(in   ) :: jtrack         !< Number of codiagonals of AMAT
        integer(kind=int_wp), intent(in   ) :: noq            !< Total number of exchanges
        integer(kind=int_wp), intent(in   ) :: noq1           !< Number of exchanges in first direction
        integer(kind=int_wp), intent(in   ) :: noq2           !< Number of exchanges in second direction
        integer(kind=int_wp), intent(in   ) :: nodisp         !< Number of additional dispersions
        integer(kind=int_wp), intent(in   ) :: novelo         !< Number of additional velocities
        integer(kind=int_wp), intent(in   ) :: notot          !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: nsys           !< Number of systems considered
        integer(kind=int_wp), intent(in   ) :: integration_id !< = 0, 2 DISP at zero flow
                                                              !< = 1, 3 no DISP at zero flow
                                                              !< = 0, 1 DISP over boundary
                                                              !< = 2, 3 no DISP over boundary
        integer(kind=int_wp), intent(in   ) :: ilflag         !< If 0 then 3 length values

        ! Local variables
        integer(kind=int_wp) ::iband, iq, i, it, i3, i4
        integer(kind=int_wp) ::kt, k1, k2, j, jt
        integer(kind=int_wp) ::ithandl = 0

        real(kind=real_wp) ::a, q, q1, q2, e, al, f1, f2, dl

        if ( timon ) call timstrt ( "dlwq70", ithandl )

        iband = 2*jtrack + 1
        do iq = 1 , noq
            ! initialisations , check for transport anyhow
            i    = ipoint(1,iq)
            j    = ipoint(2,iq)
            if ( i == 0 .or. j == 0 ) goto 50
            a    = area(iq)
            q    = flow(iq)
            if ( mod(integration_id,2) == 1 .and. abs(q) < 10.0e-25 ) goto 50
            if ( a < 1.0e-25 )  a = 1.0
            e  = disp(1)
            al = aleng(1)
            if ( iq > noq1 ) then
                e  = disp (2)
                al = aleng(2)
            endif
            if ( iq > noq1+noq2 ) then
                e  = disp (3)
                al = aleng(3)
            endif
            if ( ilflag == 1 ) then
                dl = a/(aleng(2*iq-1) + aleng(2*iq))
                f1 = aleng(2*iq  )*dl/a
                f2 = aleng(2*iq-1)*dl/a
            else
                dl = a/al
                f1 = 0.5
                f2 = 0.5
            endif
            e  = e*dl
            if (idpnt(isys)>0) e = e + disper((iq-1)*nodisp+idpnt(isys))*dl
            if (ivpnt(isys)>0) q = q + velo  ((iq-1)*novelo+ivpnt(isys))*a
            q1 = f1*q
            q2 = f2*q
            if ( i < 0 ) goto 10
            if ( j < 0 ) goto 30

            ! the regular case
            jt = (i-1)*iband + jtrack + 1
            kt = jt + (j-i)
            amat(jt) = amat(jt) + q1 + e
            amat(kt) = amat(kt) + q2 - e
            it = (j-1)*iband + jtrack + 1
            kt = it + (i-j)
            amat(it) = amat(it) - q2 + e
            amat(kt) = amat(kt) - q1 - e
            goto 50

            ! the 'from' segment is a boundary
            10 if ( j<0 ) goto 50
            if ( mod(integration_id,4) > 1 ) e = 0.0
            if ( mod(integration_id,8) >= 4 ) then
                if ( q>0.0 ) then
                    q1 = q
                    q2 = 0.0
                else
                    q1 = 0.0
                    q2 = q
                endif
            endif
            k1 = (-i-1)*notot
            i4 = ( j-1)*nsys  + 1
            do i3=isys,isys+nsys-1
            deriv(i4) = deriv(i4) + ( q1+e) * bound(k1+i3)
            i4=i4+1
            end do
            it = (j-1)*iband + jtrack + 1
            amat(it) = amat(it) - q2 + e
            goto 50

            ! the 'to' element was a boundary.
            30 if ( mod(integration_id,4) > 1 ) e = 0.0
            if ( mod(integration_id,8) >= 4 ) then
                if ( q > 0.0 ) then
                    q1 = q
                    q2 = 0.0
                else
                    q1 = 0.0
                    q2 = q
                endif
            endif
            k2 = (-j-1)*notot
            i4 = ( i-1)*nsys + 1
            do i3=isys, isys+nsys-1
                deriv(i4) = deriv(i4) + (-q2+e) * bound(k2+i3)
                i4=i4+1
            end do
            jt = (i-1)*iband + jtrack + 1
            amat(jt) = amat(jt) + q1 + e
            ! end of the loop over exchanges
            50 continue
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwq70
end module m_dlwq70
