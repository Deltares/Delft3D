      function FLFRIK (r,sk,a)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLow FRIction head K1ioc
c
c Module description: Calculate the friction head k1ioc for use in the
c                     structure routines. This routine is taken from
c                     WENDY. WENDY history is:
c                       Projekt: Construction-Module
c                       Programmeur: H. van Zanten
c                       Funktie: Calculation of the factor K in the
c                       friction head
c                       No updates
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flfrik.pf,v $
c Revision 1.2  1998/06/08  12:35:19  kuipe_j
c log added
c
c
c
c***********************************************************************
*
*  Parameters  (Input / Output) :
*  ------------------------------
*
*   Number    Name         I/O    Description
*   ------    ----         ---    ------------
*      1      r             I     hydralic radius
*      2      sk            I     roughness coefficient
*      3      a             I     wet area
*
*  The function is used for structure type IV
*
************************************************************************
*  Aanroepen  (Funkties  en / of  Subroutines) : none
*  ---------------------------------------------
************************************************************************

c
c declare arguments
      real   r, sk, a

c
c declare local variables
      real   hulp, f1root, rn1

c
c declare function
      real   FLFRIK

      hulp   = 12. * r / sk
      f1root = 1. / (2. * LOG10(hulp))
      rn1    = r**(1./6.) * f1root / 8.86
      FLFRIK = a * r**(2./3.) / rn1

      return
      end
