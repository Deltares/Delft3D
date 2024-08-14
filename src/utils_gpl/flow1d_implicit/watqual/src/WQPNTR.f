      subroutine wqpntr (lupntr ,npntr, pntr)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module 
c
c Programmer:         J.Kuipers
c
c Module:             WQPNTR (Water Quality print PoiNTeRs)
c
c Module description: Writes the pointer table to a ascii file.
c                     This is a interface file to delwaq.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 lupntr            I  Unit of interface file to Delwaq.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqpntr.pf,v $
c Revision 1.1  1996/10/31  09:51:46  kuipe_j
c Calculation of exchanges added
c
c
c***********************************************************************
c
      integer  lupntr ,npntr
      integer  pntr (4,npntr)

      integer       i, j

      do 10 i = 1, npntr
         write(lupntr,100) (pntr(j,i), j=1,2)
 10   continue
C
C     Close file
C
      close ( unit = lupntr )
C
C     Integer format
C
 100  format ( 2I8,'       0       0')

      end
