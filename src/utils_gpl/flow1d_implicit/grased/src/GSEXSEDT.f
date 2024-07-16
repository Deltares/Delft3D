      subroutine gsexsedt ( nbran  ,ngrid  ,nfrac ,branch ,
     &                      sedtr  ,disgse )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSEXSEDT (Graded Sediment EXchange SEDiment
c                               Transports)
c
c Module description: Exchange sediment transports per fraction on grid 
c                     points at nodes between distributed (DISGSE) and
c                     local velocity determined transports (SEDTR).
c
c-----------------------------------------------------------------------
c
c     Declaration of parameters
c
      integer  nbran , ngrid ,nfrac
      integer  branch(4,nbran)
      real     sedtr (ngrid,nfrac), disgse(nfrac,2,nbran)
c
c     Declaration of local variables
c
      integer  ibr,  i1, i2 ,ifrac
      real     temp
c
      do ibr = 1, nbran
c
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
c
c        Assign nodal values from array dissed
c
         do ifrac=1,nfrac
            temp                = sedtr(i1,ifrac)        
            sedtr(i1,ifrac)     = disgse(ifrac,1,ibr)
            disgse(ifrac,1,ibr) = temp
            temp                = sedtr(i2,ifrac)        
            sedtr(i2,ifrac)     = disgse(ifrac,2,ibr)
            disgse(ifrac,2,ibr) = temp
         enddo
      enddo
      
      end
