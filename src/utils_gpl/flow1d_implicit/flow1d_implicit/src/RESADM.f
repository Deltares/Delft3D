      subroutine resadm (nentri ,code ,codpre )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             RESADM (RESults; make ADMinistration for element selection)
c
c Module description: Make array CODPRE for a module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 code(nentri)      I  Main codes of all possible elements of a
c                         block.
c  3 codpre(*)         O  codpre(i) = index in block tabel (1..nentri)
c                         for main code i.
c  1 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: resadm.pf,v $
c Revision 1.2  1995/05/30  06:57:13  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:14  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:43  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nentri
      integer    code(nentri)  ,codpre(*)
c
c     Declaration of local variables
c
      integer    codlst ,i ,j
c
      j      = 0
      codlst = 0
      do 10 i = 1,nentri
         if (code(i) .ne. codlst) then
            j         = j+1
            codpre(j) = i
         endif
         codlst = code(i)
  10  continue
c
      end
