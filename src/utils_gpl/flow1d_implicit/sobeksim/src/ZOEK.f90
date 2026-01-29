SUBROUTINE ZOEK (NAAM,NOTOT,SYNAME,NZOEK,INDEX)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         -
!
! Module:             ZOEK name in list
!
! Module description:  Searches for name in list
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: zoek.pf,v $
! Revision 1.1  1998/02/13  13:23:51  kuipe_j
! Adapt to CMT
!
!
!***********************************************************************
!     ZOEK IS NIET AFHANKELIJK VAN UPPERCASE/LOWERCASE
!
!     De routine maakt gebruik van ICHAR()
!     a t/m z hebben codes 97 t/m 122
!     A t/m Z hebben codes 65 t/m 90
!
! Argument Type Lengte I/O Omschrijving
! NAAM     C    1      I   Te identificeren string
! NOTOT    I    1      I   Lengte van lijst bekende strings
! SYNAME   C    NOTOT  I   Lijst bekende strings
! NZOEK    I    1      I   Aantal karakters voor verg. strings
! INDEX    I    1      O   Index van NAAM in SYNAME
!                          (-1 als NAAM niet in SYNAME aanwezig is)
!
   integer       notot,nzoek,index
   CHARACTER*(*) NAAM
   CHARACTER*(*) SYNAME(NOTOT)

   integer i,i1,i2,k

   INDEX = -1
!     WRITE (*,'(A)') NAAM(1:NZOEK)
   DO 100 I = 1,NOTOT
!         WRITE (*,'(I5,A)') I,SYNAME(I)(1:NZOEK)
      DO 50 K = 1,NZOEK
         I1 = ICHAR(NAAM     (K:K))
         I2 = ICHAR(SYNAME(I)(K:K))
         IF (I1.GE. 97.AND.I1.LE.122) THEN
            IF (I1.NE.I2.AND.I1.NE.(I2+32)) GOTO 100
         ELSEIF (I1.GE. 65.AND.I1.LE. 90) THEN
            IF (I1.NE.I2.AND.I1.NE.(I2-32)) GOTO 100
         ELSE
            IF (I1.NE.I2                  ) GOTO 100
         ENDIF
50    CONTINUE
      INDEX = I
      GOTO 200
!         IF (NAAM(1:NZOEK).EQ.SYNAME(I)(1:NZOEK)) GOTO 200
100 CONTINUE
!     WRITE (*,*) ' ZOEK:',INDEX
   RETURN
200 CONTINUE
!     WRITE (*,*) ' ZOEK:',INDEX
   RETURN
END
