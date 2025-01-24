subroutine gsexsedt ( nbran  ,ngrid  ,nfrac ,branch ,&
&sedtr  ,disgse )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSEXSEDT (Graded Sediment EXchange SEDiment
!                               Transports)
!
! Module description: Exchange sediment transports per fraction on grid
!                     points at nodes between distributed (DISGSE) and
!                     local velocity determined transports (SEDTR).
!
!-----------------------------------------------------------------------
!
!     Declaration of parameters
!
   integer  nbran , ngrid ,nfrac
   integer  branch(4,nbran)
   real     sedtr (ngrid,nfrac), disgse(nfrac,2,nbran)
!
!     Declaration of local variables
!
   integer  ibr,  i1, i2 ,ifrac
   real     temp
!
   do ibr = 1, nbran
!
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
!
!        Assign nodal values from array dissed
!
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
