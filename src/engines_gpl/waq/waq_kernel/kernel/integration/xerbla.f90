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
module m_xerbla
    use m_waq_precision

    implicit none

contains


    SUBROUTINE XERBLA (SRNAME, INFO)
        use m_logger, only : terminate_execution
        use timers

        !          ..    Scalar Arguments ..
        INTEGER(kind = int_wp) :: INFO
        character(len=6)        SRNAME
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("xerbla", ithandl)
        !          ..
        !
        !       Purpose
        !       =======
        !
        !       XERBLA  is an error handler for the Level 2 BLAS routines.
        !
        !       It is called by the Level 2 BLAS routines if an input parameter is
        !       invalid.
        !
        !       Installers should consider modifying the STOP statement in order to
        !       call system-specific exception-handling facilities.
        !
        !       Parameters
        !       ==========
        !
        !       SRNAME - character(len=6).
        !                On entry, SRNAME specifies the name of the routine which
        !                called XERBLA.
        !
        !       INFO   - INTEGER.
        !                On entry, INFO specifies the position of the invalid
        !                parameter in the parameter-list of the calling routine.
        !
        !
        !       Auxiliary routine for Level 2 Blas.
        !
        !       Written on 20-July-1986.
        !
        !          .. Executable Statements ..
        !
        WRITE (*, 99999) SRNAME, INFO
        !
        CALL terminate_execution(1)
        !
        99999 FORMAT (' ** On entry to ', A6, ' parameter number ', I2, &
                ' had an illegal value')
        !
        !          End of XERBLA.
        !
        if (timon) call timstop (ithandl)
    END

end module m_xerbla
