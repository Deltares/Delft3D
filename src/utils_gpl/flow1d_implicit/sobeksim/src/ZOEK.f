      SUBROUTINE ZOEK (NAAM,NOTOT,SYNAME,NZOEK,INDEX)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module                        
c
c Programmer:         -                
c
c Module:             ZOEK name in list
c
c Module description:  Searches for name in list
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: zoek.pf,v $
c Revision 1.1  1998/02/13  13:23:51  kuipe_j
c Adapt to CMT
c
c
c***********************************************************************
C     ZOEK IS NIET AFHANKELIJK VAN UPPERCASE/LOWERCASE
C
C     De routine maakt gebruik van ICHAR()
C     a t/m z hebben codes 97 t/m 122
C     A t/m Z hebben codes 65 t/m 90
C
C Argument Type Lengte I/O Omschrijving
C NAAM     C    1      I   Te identificeren string
C NOTOT    I    1      I   Lengte van lijst bekende strings
C SYNAME   C    NOTOT  I   Lijst bekende strings
C NZOEK    I    1      I   Aantal karakters voor verg. strings
C INDEX    I    1      O   Index van NAAM in SYNAME
C                          (-1 als NAAM niet in SYNAME aanwezig is)
C
      integer       notot,nzoek,index
      CHARACTER*(*) NAAM
      CHARACTER*(*) SYNAME(NOTOT)

      integer i,i1,i2,k

      INDEX = -1
C     WRITE (*,'(A)') NAAM(1:NZOEK)
      DO 100 I = 1,NOTOT
C         WRITE (*,'(I5,A)') I,SYNAME(I)(1:NZOEK)
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
   50     CONTINUE
          INDEX = I
          GOTO 200
C         IF (NAAM(1:NZOEK).EQ.SYNAME(I)(1:NZOEK)) GOTO 200
  100 CONTINUE
C     WRITE (*,*) ' ZOEK:',INDEX
      RETURN
  200 CONTINUE
C     WRITE (*,*) ' ZOEK:',INDEX
      RETURN
      END
