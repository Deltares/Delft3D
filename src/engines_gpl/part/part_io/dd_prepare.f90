!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module dd_prepare_mod
    interface
        subroutine dd_prepare (lunut, num_rows, num_columns, lgrid, nbmax, &
                &                        ndoms, nbnds, nconn, doms, bnds, &
                &                        conn)
            use m_waq_precision       ! single and double precision
            use typos           ! the derived types
            integer  (int_wp), intent(in) :: lunut             !< output report file
            integer  (int_wp), intent(in) :: num_rows              !< first dimension of the grid
            integer  (int_wp), intent(in) :: num_columns              !< second dimension of the grid
            integer  (int_wp), intent(inout) :: lgrid (num_rows, num_columns) !< the active grid matrix
            integer  (int_wp), intent(in) :: nbmax             !< maximum number of real boundaries
            integer  (int_wp), intent(in) :: ndoms             !< number of domains
            integer  (int_wp), intent(in) :: nbnds             !< number of DD-boundaries
            integer  (int_wp), intent(out) :: nconn             !< number of connection points
            type (domain), intent(in) :: doms  (ndoms)     !< the domains
            type (boundp), intent(in) :: bnds  (nbnds)     !< the domain boundaries
            type (pnt), pointer :: conn  (:)       !< the array with connectivity points
        end subroutine
    end interface
end module

subroutine dd_prepare (lunut, num_rows, num_columns, lgrid, nbmax, &
        &                        ndoms, nbnds, nconn, doms, bnds, &
        &                        conn)

    !     Deltares Software Centre

    !>/File
    !>            Makes the DD-connectivities on a per cell basis
    !>
    !>            Replaces zero's in the LGRID DD locations with minus values counting from
    !>            -nbmax, where nbmax is the number of real open boundaries of the total area.\n
    !>            Fills the connectivity array:
    !>            - a negative value in the LGRID table <= nbmax is a normal open boundary
    !>            - a negative value above refers to the value-nbmax entry in conn(:)
    !>            - conn(:) contains
    !>              - the indication that it enters in the receiving call at position 0.0 or 1.0
    !>              - a (n,m) elsewhere in the grid where the particle should go to
    !>              - an integer refinement f factor in the grid
    !>              - an integer refinement indicator
    !>                - if 0 then the bigger cell refines to f cells starting at (n,m)
    !>                  the first 1.0/f part goest to the (n,m) cell, the second part to the next cell etc.
    !>                - if 1 and f is 1 then it is a 1 to 1 coupling, no problem
    !>                - if 1, 2, ... and f > 1 a fine cell coarsens to a coarser (n,m) cell
    !>                  the 0 - 1 coordinate should be divided by f and put in the i-th part of the coarse cell
    !>              - the conn contains 2 of these sets, one for each direction in the grid
    !>                this allows for inner edges to be resolved in both direction with one entry

    !     system administration : Antoon Koster

    !     created               : October  2011 by Leo Postma

    !     last modified         :

    !     logical unit numbers  : lunut, the output report file

    use m_waq_precision       ! single and double precision
    use typos           ! the derived types
    use timers          ! performance timers

    implicit none

    !     Arguments

    !     kind           function         name                 description

    integer  (int_wp), intent(in) :: lunut             !< output report file
    integer  (int_wp), intent(in) :: num_rows              !< first dimension of the grid
    integer  (int_wp), intent(in) :: num_columns              !< second dimension of the grid
    integer  (int_wp), intent(inout) :: lgrid (num_rows, num_columns) !< the active grid matrix
    integer  (int_wp), intent(in) :: nbmax             !< maximum number of real boundaries
    integer  (int_wp), intent(in) :: ndoms             !< number of domains
    integer  (int_wp), intent(in) :: nbnds             !< number of DD-boundaries
    integer  (int_wp), intent(out) :: nconn             !< number of connection points
    type (domain), intent(in) :: doms  (ndoms)     !< the domains
    type (boundp), intent(in) :: bnds  (nbnds)     !< the domain boundaries
    type (pnt), pointer :: conn  (:)       !< the array with connectivity points

    !     Locals

    integer(4)  len11, len12, len21, len22   ! distances along the zippers
    integer(4)  ifact                        ! factor of both distances
    integer(4)  inflag1, inflag2             ! entering at 0.0 or 1.0
    integer(4)  n1, m1, n2, m2               ! help variables in the grid
    integer(4)  np1, np2                     ! help variables in the conn array
    integer(4)  i, i1, i2                    ! loop counters
    integer(4)  iacc                         ! refinement counter
    type (pnt), pointer :: con2  (:)       ! help to re-allocate conns
    integer(4)  ithndl                       ! handle to time this subroutine
    data        ithndl / 0 /

    if (timon) call timstrt("dd_prepare", ithndl)

    !     solve the zippers

    nconn = 0
    do i = 1, nbnds
        len11 = bnds(i)%r1%tn - bnds(i)%r1%fn
        len12 = bnds(i)%r1%tm - bnds(i)%r1%fm
        len21 = bnds(i)%r2%tn - bnds(i)%r2%fn
        len22 = bnds(i)%r2%tm - bnds(i)%r2%fm
        if (nconn == 0) then
            allocate (conn(len11 + len12 + len21 + len22))
        else
            allocate (con2(nconn + len11 + len12 + len21 + len22))
            con2(1:nconn) = conn
            deallocate (conn)
            conn => con2
        endif
        if (len11 > 0) then                             ! It is in 1st, 'n' direction
            ifact = len21 / len11                               ! if > 1 refines
            if (ifact == 0) ifact = -len11 / len21          ! if < 0 this contracts
            n1 = bnds(i)%r1%fn + 1
            m1 = bnds(i)%r1%fm + doms(bnds(i)%r1%did)%moff
            if (lgrid(n1, m1) <= 0) then                   ! If from outer to inner:
                inflag1 = 0                                    !    coordinate starts at 0.0
            else                                              !    at the active side
                m1 = m1 + 1                                    ! If from inner to outer:
                inflag1 = 1                                    !    boundary is one m-line further
            endif                                             !    entering is at 1.0
            n2 = bnds(i)%r2%fn + 1                            ! Also the receiver runs n
            m2 = bnds(i)%r2%fm + doms(bnds(i)%r2%did)%moff
            if (lgrid(n2, m2) <= 0) then                   ! Same as above
                inflag2 = 0
            else
                m2 = m2 + 1
                inflag2 = 1
            endif
            iacc = 0
            do i1 = n1, n1 + len11 - 1                        ! Walk the first zipper
                if (lgrid(i1, m1) < 0) then                ! was already a boundary (corner)
                    np1 = -lgrid(i1, m1) - nbmax
                else                                           ! new point
                    nconn = nconn + 1
                    lgrid(i1, m1) = -nconn - nbmax                 ! give it a boundary number
                    np1 = nconn
                    conn(np1)%in2 = -1                          ! initialize other part
                endif
                conn(np1)%in1 = inflag2                        ! Give it the receivers edge
                conn(np1)%n1 = n2
                if (inflag2 == 0) then                     ! We must point to an active cell
                    conn(np1)%m1 = m2 + 1
                else
                    conn(np1)%m1 = m2 - 1
                endif
                if (ifact < 0) then                       ! It contracts
                    conn(np1)%f1 = -ifact                       ! Contraction factor
                    conn(np1)%i1 = mod(iacc, -ifact) + 1         ! Number in the receiving domain
                    if (mod(iacc, -ifact) == 0) then         ! Do the same at the receiver side
                        if (lgrid(n2, m2) < 0) then          ! May also be a corner
                            np2 = -lgrid(n2, m2) - nbmax             !          |
                        else                                     !          |
                            nconn = nconn + 1                     !          |
                            lgrid(n2, m2) = -nconn - nbmax           ! This happens only for each
                            np2 = nconn                           ! first cell at sender side
                            conn(np2)%in2 = -1                    ! initialize other part
                        endif                                    !          |
                        conn(np2)%in1 = inflag1                  !          |
                        conn(np2)%n1 = i1                       !          |
                        if (inflag1 == 0) then               !          |
                            conn(np2)%m1 = m1 + 1                !          |
                        else                                     !          |
                            conn(np2)%m1 = m1 - 1                !          |
                        endif                                    !          |
                        conn(np2)%f1 = -ifact                   !          |
                        conn(np2)%i1 = 0                        ! Starts always at 0
                    endif
                    if (mod(iacc, -ifact) == -ifact - 1) then         ! Do the same at the receiver side
                        n2 = n2 + 1
                    endif
                    iacc = iacc + 1                             ! Increase part nr in receiver
                else
                    conn(np1)%f1 = ifact
                    conn(np1)%i1 = 0
                    if (ifact == 1) conn(np1)%i1 = 1
                    do i2 = n2, n2 + ifact - 1                      ! Maybe 1 for 1 to 1 coupling
                        if (lgrid(i2, m2) < 0) then
                            np2 = -lgrid(i2, m2) - nbmax
                        else
                            nconn = nconn + 1
                            lgrid(i2, m2) = -nconn - nbmax
                            np2 = nconn
                            conn(np2)%in2 = -1                    ! initialize other part
                        endif
                        conn(np2)%in1 = inflag1
                        conn(np2)%n1 = i1
                        if (inflag1 == 0) then
                            conn(np2)%m1 = m1 + 1
                        else
                            conn(np2)%m1 = m1 - 1
                        endif
                        conn(np2)%f1 = ifact
                        conn(np2)%i1 = mod(iacc, ifact) + 1
                        iacc = iacc + 1
                    enddo
                    n2 = n2 + ifact
                endif
            enddo
        else                                                 ! It is in 2nd, 'm' direction
            ifact = len22 / len12
            if (ifact == 0) ifact = -len12 / len22
            n1 = bnds(i)%r1%fn
            m1 = bnds(i)%r1%fm + 1 + doms(bnds(i)%r1%did)%moff
            if (lgrid(n1, m1) <= 0) then
                inflag1 = 0
            else
                n1 = n1 + 1
                inflag1 = 1
            endif
            n2 = bnds(i)%r2%fn
            m2 = bnds(i)%r2%fm + 1 + doms(bnds(i)%r2%did)%moff
            if (lgrid(n2, m2) <= 0) then
                inflag2 = 0
            else
                n2 = n2 + 1
                inflag2 = 1
            endif
            iacc = 0
            do i1 = m1, m1 + len12 - 1
                if (lgrid(n1, i1) < 0) then
                    np1 = -lgrid(n1, i1) - nbmax
                else
                    nconn = nconn + 1
                    lgrid(n1, i1) = -nconn - nbmax
                    np1 = nconn
                    conn(np1)%in1 = -1                          ! initialize other part
                endif
                conn(np1)%in2 = inflag2
                if (inflag2 == 0) then
                    conn(np1)%n2 = n2 + 1
                else
                    conn(np1)%n2 = n2 - 1
                endif
                conn(np1)%m2 = m2
                if (ifact < 0) then
                    conn(np1)%f2 = -ifact
                    conn(np1)%i2 = mod(iacc, -ifact) + 1
                    if (mod(iacc, -ifact) == 0) then
                        if (lgrid(n2, m2) < 0) then
                            np2 = -lgrid(n2, m2) - nbmax
                        else
                            nconn = nconn + 1
                            lgrid(n2, m2) = -nconn - nbmax
                            np2 = nconn
                            conn(np2)%in1 = -1                    ! initialize other part
                        endif
                        conn(np2)%in2 = inflag1
                        if (inflag1 == 0) then
                            conn(np2)%n2 = n1 + 1
                        else
                            conn(np2)%n2 = n1 - 1
                        endif
                        conn(np2)%m2 = i1
                        conn(np2)%f2 = -ifact
                        conn(np2)%i2 = 0
                    endif
                    if (mod(iacc, -ifact) == -ifact - 1) then
                        m2 = m2 + 1
                    endif
                    iacc = iacc + 1
                else
                    conn(np1)%f2 = ifact
                    conn(np1)%i2 = 0
                    if (ifact == 1) conn(np1)%i2 = 1
                    do i2 = m2, m2 + ifact - 1
                        if (lgrid(n2, i2) < 0) then
                            np2 = -lgrid(n2, i2) - nbmax
                        else
                            nconn = nconn + 1
                            lgrid(n2, i2) = -nconn - nbmax
                            np2 = nconn
                            conn(np2)%in1 = -1                    ! initialize other part
                        endif
                        conn(np2)%in2 = inflag1
                        if (inflag1 == 0) then
                            conn(np2)%n2 = n1 + 1
                        else
                            conn(np2)%n2 = n1 - 1
                        endif
                        conn(np2)%m2 = i1
                        conn(np2)%f2 = ifact
                        conn(np2)%i2 = mod(iacc, ifact) + 1
                        iacc = iacc + 1
                    enddo
                    m2 = m2 + ifact
                endif
            enddo
        endif
    enddo
    if (timon) call timstop(ithndl)
    return
end
