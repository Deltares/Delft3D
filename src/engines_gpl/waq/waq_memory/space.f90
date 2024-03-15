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

MODULE WORKSPACE

    use m_waq_precision
    use m_srstop
    USE DHMMAR_MOD
    USE DHMMCA_MOD
    USE DHMMJA_MOD
    USE DHMMRA_MOD
    use m_sysn          ! System characteristics
    use m_sysi          ! Timer characteristics
    use m_sysa          ! Pointers in real array workspace
    use m_sysj          ! Pointers in integer array workspace
    use m_sysc          ! Pointers in character array workspace
    use m_array_manipulation, only : memory_partition ! module for computing the pointers into the arrays


    CONTAINS

    SUBROUTINE SPACE  ( LUNREP , L_DECL , A      , J      , C      , &
                         IMAXA  , IMAXI  , IMAXC  )
        ! Sets the array pointers in the SYSA, SYSI and SYSC common blocks.
        ! This is the only place where these common blocks are changed.
        ! WARNING: The order in the common block must be the same as the order in which the
        ! pointers are set.
        !
        !     LOGICAL UNITNUMBERS : LUNREP- monitoring output file
        !
        !     SUBROUTINES CALLED  : SRSTOP, stops execution
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNREP  INTEGER       1     INPUT   logical unitnumber output file
        !     L_DECL  LOGICAL       1     INPUT   Declare memory y/n
        !     A       INTEGER       *     OUTPUT  real workspace array
        !     J       INTEGER       *     OUTPUT  integer workspace array
        !     C       CHAR*20       *     OUTPUT  character workspace array
        !     IMAXA   INTEGER       1     INPUT   Maximum real array space
        !     IMAXI   INTEGER       1     INPUT   Maximum integer array space
        !     IMAXC   INTEGER       1     INPUT   Maximum character array space
        !
        INTEGER(kind=int_wp) ::LUNREP, IMAXA  , IMAXI  , IMAXC
        LOGICAL       L_DECL
        REAL(kind=real_wp), DIMENSION(:), allocatable              ::A
        INTEGER(kind=int_wp), DIMENSION(:), allocatable           ::J
        CHARACTER(LEN=*), DIMENSION(:), allocatable :: C

        INTEGER(kind=int_wp), DIMENSION(:), allocatable                ::JNEW
        CHARACTER(LEN=LEN(C)), DIMENSION(:), allocatable :: CNEW
        CHARACTER(LEN=20),     DIMENSION(:), ALLOCATABLE :: CNAME

        INTEGER(kind=int_wp) ::K1, K2
        INTEGER(kind=INT64) ::ITOT

        TYPE(MEMORY_PARTITION) :: PART

        ! Allocate initial space
        NOARR  = IASIZE + IJSIZE + ICSIZE

        IF ( allocated(J)    ) DEALLOCATE( J )
        IF ( allocated(C)    ) DEALLOCATE( C )
        IF ( allocated(CNAME) ) DEALLOCATE( CNAME )

        ALLOCATE( J(     IASIZE + 1 + IJSIZE + ICSIZE + 1 + 8 * NOARR  ) )
        ALLOCATE( C( 20*(IASIZE + 1 + IJSIZE + ICSIZE + 1) ) )
        ALLOCATE( CNAME (IASIZE + 1 + IJSIZE + ICSIZE + 1) )

        J = 0
        C = ' '
        CNAME = ' '

        ! Total number of "separate" variables
        NOVAR = 5 + NOCONS + NOPA   + NOFUN  + NOSFUN + NOTOT + NOTOT + &
                    NOTOT  + NODISP + NOVELO + NODEF  + NOLOC + NDSPX + &
                    NVELX  + NLOCX  + NFLUX

        ! Sets the array pointers for the array administration array's.
        CALL DHMMAR( LUNREP  , J       , CNAME   , PART )

        ! Set the real array workspace
        CALL DHMMRA( LUNREP   ,L_DECL   ,J(IAPOI:), J(IATYP:), J(IABYT:), &
                     J(IALEN:),J(IAKND:),J(IADM1:), J(IADM2:), J(IADM3:), &
                     CNAME, ITOTA, PART )

        ! Set the integer array workspace

        CALL DHMMJA( LUNREP   ,L_DECL   ,J(IAPOI:), J(IATYP:), J(IABYT:), &
                     J(IALEN:),J(IAKND:),J(IADM1:), J(IADM2:), J(IADM3:), &
                     CNAME, ITOTI, PART )

        !     Set the character array workspace
        CALL DHMMCA( LUNREP   ,L_DECL   ,J(IAPOI:), J(IATYP:), J(IABYT:), &
                     J(IALEN:),J(IAKND:),J(IADM1:), J(IADM2:), J(IADM3:), &
                     CNAME, ITOTC, PART )

        ! messages and tests on array space

        ITOT = INT8(ITOTA+ITOTI+ITOTC)*4_2
        WRITE ( LUNREP, 2000 ) ITOTA, ITOTI, ITOTC, ITOT/4, &
                               ITOT/1000000000, &
                               MOD(ITOT,1000000000)/1000000, &
                               MOD(ITOT,   1000000)/   1000, &
                               MOD(ITOT,      1000)
        IEFLAG = 0
        IF ( ITOTA > IMAXA .AND. IMAXA /= 0 .AND. L_DECL ) THEN
            WRITE ( LUNREP, 2010 ) ITOTA, IMAXA
            IEFLAG = 1
        ENDIF
        IF ( ITOTI > IMAXI .AND. IMAXI /= 0 .AND. L_DECL ) THEN
            WRITE ( LUNREP, 2020 ) ITOTI, IMAXI
            IEFLAG = 1
        ENDIF
        IF ( ITOTC > IMAXC .AND. IMAXC /= 0 .AND. L_DECL ) THEN
            WRITE ( LUNREP, 2030 ) ITOTC, IMAXC
            IEFLAG = 1
        ENDIF
        IF ( IEFLAG ==    1 ) THEN
            WRITE ( LUNREP, 2040 )
            CALL SRSTOP(1)
        ENDIF
        IMAXA = ITOTA
        IMAXI = ITOTI
        IMAXC = ITOTC

        ! Allocate the arrays, first C then J then A for least memory requirement

        IF ( L_DECL ) THEN

            DEALLOCATE( A )                ! It was allocated at the start

            ALLOCATE( CNEW(part%char_pointer) )
            CNEW = ' '
            DO K2 = 1,SIZE(CNAME)
                DO K1 = 1,20
                    CNEW(1+K1+(K2-1)*20) = CNAME(K2)(K1:K1)
                ENDDO
            ENDDO
            DEALLOCATE( C )
            C = CNEW

            ALLOCATE( JNEW(part%index_pointer) )
            JNEW = 0
            JNEW(1:SIZE(J)) = J
            DEALLOCATE( J )
            J = JNEW

            ALLOCATE( A(part%alpha_pointer) )
            A    = 0.0

        ENDIF

        RETURN

        ! output formats
        2000 FORMAT ( ' total real      array space: ',I10,/ &
                    ' total integer   array space: ',I10,/ &
                    ' total character array space: ',I10,/ &
                    ' grand total in 4-byte words: ',I10, &
                    ' = ',i3,'-GB ',i3,'-MB ',i3'-KB ',i3,'-Byte.' )
        2010 FORMAT ( ' ERROR. Real      array space exceeded !!! ',/, &
                    ' total real    array space: ',I10,', allowed = ',I10)
        2020 FORMAT ( ' ERROR. Integer   array space exceeded !!! ',/, &
                    ' total integer array space: ',I10,', allowed = ',I10)
        2030 FORMAT ( ' ERROR. Character array space exceeded !!! ',/, &
                    ' total Character*20  space: ',I10,', allowed = ',I10)
        2040 FORMAT ( ' EXECUTION HALTED, CONSULT YOUR SYSTEM MANAGER !!!')

    END SUBROUTINE SPACE

END MODULE WORKSPACE
