subroutine resadm (nentri ,code ,codpre )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             RESADM (RESults; make ADMinistration for element selection)
!
! Module description: Make array CODPRE for a module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 code(nentri)      I  Main codes of all possible elements of a
!                         block.
!  3 codpre(*)         O  codpre(i) = index in block tabel (1..nentri)
!                         for main code i.
!  1 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: resadm.pf,v $
! Revision 1.2  1995/05/30  06:57:13  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:14  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:43  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nentri
   integer    code(nentri)  ,codpre(*)
!
!     Declaration of local variables
!
   integer    codlst ,i ,j
!
   j      = 0
   codlst = 0
   do 10 i = 1,nentri
      if (code(i) .ne. codlst) then
         j         = j+1
         codpre(j) = i
      endif
      codlst = code(i)
10 continue
!
end
