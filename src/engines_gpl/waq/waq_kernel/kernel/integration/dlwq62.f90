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
 module m_dlwq62
    use m_waq_precision

    implicit none

    contains

    !> Fills band matrix according to backward differencing in space.
    subroutine dlwq62( disp   , disper , area   , flow   , aleng  , & 
                       velo   , bound  , ipoint , nosys  , isys   , & 
                       nsys   , noq1   , noq2   , noq    , nodisp , & 
                       novelo , idpnt  , ivpnt  , deriv  , amat   , & 
                       jtrack , integration_id  , ilflag )

    use timers

    implicit none

    integer(kind=int_wp), intent(in   ) :: nosys               !< Number of transported substances
    integer(kind=int_wp), intent(in   ) :: isys                !< Start number of substances
    integer(kind=int_wp), intent(in   ) :: nsys                !< Number of substances with same matrix
    integer(kind=int_wp), intent(in   ) :: noq1                !< Number of fluxes first direction
    integer(kind=int_wp), intent(in   ) :: noq2                !< Number of fluxes second direction
    integer(kind=int_wp), intent(in   ) :: noq                 !< Total number fluxes in the water phase
    integer(kind=int_wp), intent(in   ) :: ipoint(4, noq)      !< From, to, from-1, to+1 volume numbers per flux
    integer(kind=int_wp), intent(in   ) :: nodisp              !< Number of additional dispersion arrays
    integer(kind=int_wp), intent(in   ) :: novelo              !< Number of additional velocity   arrays
    integer(kind=int_wp), intent(in   ) :: idpnt(nosys)        !< Dispersion array to be applied per substance
    integer(kind=int_wp), intent(in   ) :: ivpnt(nosys)        !< Velocity   array to be applied per substance
    real(kind=real_wp),   intent(in   ) :: area(noq)           !< Cross-sectional surface areas of the fluxes
    real(kind=real_wp),   intent(in   ) :: flow(noq)           !< Fluxes
    real(kind=real_wp),   intent(in   ) :: aleng(2,noq)        !< From and to distances to the surface area
    real(kind=real_wp),   intent(in   ) :: disp(3)             !< Default dispersions in the 3 directions
    real(kind=real_wp),   intent(in   ) :: disper(nodisp, noq) !< Additional dispersion arrays
    real(kind=real_wp),   intent(in   ) :: velo(novelo,noq)    !< Additional velocity arrays
    real(kind=real_wp),   intent(in   ) :: bound(nosys, *)     !< Values at the open boundaries
    real(kind=real_wp),   intent(inout) :: deriv(nsys, *)      !< Right hand side of the equations
    integer(kind=int_wp), intent(in   ) :: jtrack              !< Number of codiagonals of amat
    integer(kind=int_wp), intent(in   ) :: integration_id      !< = 0 or 2 => DISP at zero flow
                                                               !< = 1 or 3 => no DISP at zero flow
                                                               !< = 0 or 1 => DISP over boundary
                                                               !< = 2 or 3 => no DISP over boundary
    integer(kind=int_wp), intent(in   ) :: ilflag              !< If 0 then only 3 length values in the 3 direction
    real(kind=real_wp),   intent(  out) :: amat(2*jtrack+1,*)  !< Matrix with transports

    ! Local variables
    logical    zerof               !<  If true, then NO dispersion at zero flow
    logical    zerob               !<  If true, then NO dispersion accross open boundaries
    logical    length              !<  If true, an array of lengths is provided
    integer(kind=int_wp) ::iq      !<  Loop counter over exchange surfaces
    integer(kind=int_wp) ::ifrom   !<  from volume number
    integer(kind=int_wp) ::ito     !<  to   volume number
    real(kind=real_wp) ::a         !<  Auxiliary variable for exchange surface area in m2
    real(kind=real_wp) ::q         !<  Auxiliary variable for the flux in m3/s
    real(kind=real_wp) ::e         !<  Auxiliary variable for diffusive flux in m3/s
    real(kind=real_wp) ::dl        !<  Auxiliary variable for the diffusive multiplier area/leng in m
    integer(kind=int_wp) ::idp     !<  Auxiliary variable for idpnt(isys)
    integer(kind=int_wp) ::ivp     !<  Auxiliary variable for ivpnt(isys)
    integer(kind=int_wp) ::idiag   !<  Auxiliary variable for the location of the diagonal in amat
    integer(kind=int_wp) ::i3, i4  !<  Auxiliary variables for the boundaries
    real(kind=real_wp) ::q1 , q2   !<  Auxiliary variables for upwind flow schematisation
    integer(kind=int_wp) ::noq12   !<  Auxiliary variable number of horizontal exchanges

    integer(kind=int_wp) ::ithandl = 0
    if ( timon ) call timstrt ( "dlwq62", ithandl )

    zerof  = btest( integration_id, 0 )
    zerob  = btest( integration_id, 1 )
    length = ilflag == 1
    idp    = idpnt(isys)
    ivp    = ivpnt(isys)
    idiag  = jtrack + 1
    noq12  = noq1 + noq2

    do iq = 1, noq
        ! initialisations, check for transport anyhow
        ifrom = ipoint(1, iq)
        ito   = ipoint(2, iq)
        if ( ifrom == 0 .or. ito == 0 ) cycle
        a = area(iq)
        q = flow(iq)
        if ( zerof .and. iq <= noq12 .and. abs(q) < 10.0e-25 ) goto 50
        if ( a < 1.0e-25 )  a = 1.0
        if ( iq <= noq1 ) then
            e  = disp(1)
            if ( length ) then
                dl = a / (aleng(1,iq) + aleng(2,iq))
            else
                dl = a / aleng(1,1)         ! first element of the array
            end if
        else if ( iq <= noq1+noq2 ) then
            e  = disp(2)
            if ( length ) then
                dl = a / (aleng(1,iq) + aleng(2,iq))
            else
                dl = a / aleng(2,1)         ! second element of the array
            end if
        else
            e  = disp(3)
            if ( length ) then
                dl = a / (aleng(1,iq) + aleng(2,iq))
            else
                dl = a / aleng(1,2)         ! third element of the array
            end if
        end if
        e  = e*dl
        if ( idp > 0 ) e = e + disper(idp,iq)*dl
        if ( ivp > 0 ) q = q + velo  (ivp,iq)*a
        if ( q > 0.0 ) then
            q1 =   q
            q2 = 0.0
        else
            q1 = 0.0
            q2 =   q
        end if
        if ( ifrom < 0 ) goto 10
        if ( ito   < 0 ) goto 30

        ! the regular case
        amat(idiag          ,ifrom) = amat(idiag          ,ifrom) + q1 + e
        amat(idiag+ito-ifrom,ifrom) = amat(idiag+ito-ifrom,ifrom) + q2 - e
        amat(idiag          ,ito  ) = amat(idiag          ,ito  ) - q2 + e
        amat(idiag+ifrom-ito,ito  ) = amat(idiag+ifrom-ito,ito  ) - q1 - e
        cycle

        ! The 'from' volume is a boundary
10      if ( ito  < 0 ) cycle
        if ( zerob ) e = 0.0
        amat(idiag          ,ito  ) = amat(idiag          ,ito  ) - q2 + e
        do i3 = 1, nsys
            i4 = i3 + isys - 1
            deriv(i3,ito  ) = deriv(i3,ito  ) + (  q1 + e ) * bound(i4,-ifrom)
        end do
        cycle

        ! The 'to' element was a boundary.
30      if ( zerob ) e = 0.0
        amat(idiag          ,ifrom) = amat(idiag          ,ifrom) + q1 + e
        do i3 = 1, nsys
            i4 = i3 + isys - 1
            deriv(i3,ifrom) = deriv(i3,ifrom) + ( -q2 + e ) * bound(i4,-ito  )
        end do

        ! end of the loop over exchanges
50      continue
    end do

    if ( timon ) call timstop ( ithandl )
   end subroutine dlwq62
end module m_dlwq62
