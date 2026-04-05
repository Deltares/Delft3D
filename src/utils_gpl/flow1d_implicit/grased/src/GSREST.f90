subroutine gsrest (fd_nefis_rst ,grnams ,nentri ,ngrid ,nfrac ,&
&nunlay ,nlayer ,ptrla2 ,pexla2 ,p0la  ,nrdzdl,&
&lanrinbt       ,zbave  ,zbfl   ,levunl,dzr   ,&
&deff2  ,dmed0  ,depos  ,ncelst ,nameel,neferr)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment module
!
! Programmer:         J.kuipers
!
! Module:             GSREST (Graded Sediment REading of reSTart
!                     information)
!
! Module description: Read restart information into memory
!
!                     Depending on the number of layers used se-
!                     veral variables must be read from the restart
!                     file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnams            P  -
!  7 hlev              P  -
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  9 nameel            P  -
!  8 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
! 10 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  5 ngrid             I  Number of grid points in network.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getiel  GET Integer ELement from a nefis file
! getrel  GET Real ELement from a nefis file
! getlel  GET Logical ELement from a nefis file
!=======================================================================
!
!     Parameters
!
   integer       nentri ,ngrid  ,nfrac     ,nunlay ,nlayer ,ncelst,&
   &neferr
   integer       fd_nefis_rst,&
   &nrdzdl(ngrid)             ,lanrinbt(ngrid)
   real          ptrla2 (ngrid,nfrac)      ,pexla2 (ngrid,nfrac)  ,&
   &p0la   (ngrid,nfrac,nunlay)                      ,&
   &zbave  (ngrid)            ,zbfl   (ngrid)        ,&
   &levunl (ngrid)            ,dzr    (ngrid)        ,&
   &deff2  (ngrid)            ,dmed0  (ngrid)
   logical       depos  (ngrid)
   character*(*) grnams
   character*(*) nameel(nentri)
!
!     Local variables
!
   integer       error, buflen
   integer       uindex(3), usrord(1)
!
!     Declaration of external functions
!
   integer       getrel ,getiel ,getlel
   external      getrel ,getiel ,getlel
!
   data          usrord /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   buflen = ngrid * nfrac * 4
   error  = getrel(fd_nefis_rst, grnams, nameel(2),&
   &uindex, usrord, buflen, ptrla2   )
   if (error .ne. 0) goto 1000
!
   if (nlayer.eq.2) then
      error  = getrel(fd_nefis_rst, grnams, nameel(3),&
      &uindex, usrord, buflen, pexla2   )
      if (error .ne. 0) goto 1000
   endif

   buflen = ngrid * nfrac * nunlay * 4
   error  = getrel(fd_nefis_rst, grnams, nameel(4),&
   &uindex, usrord, buflen, p0la      )
   if (error .ne. 0) goto 1000
!
   buflen = ngrid * 4
   error  = getrel(fd_nefis_rst, grnams, nameel(5),&
   &uindex, usrord, buflen, dzr       )
   if (error .ne. 0) goto 1000
!
   error  = getrel(fd_nefis_rst, grnams, nameel(6),&
   &uindex, usrord, buflen, deff2     )
   if (error .ne. 0) goto 1000
!
   error  = getrel(fd_nefis_rst, grnams, nameel(7),&
   &uindex, usrord, buflen, levunl    )
   if (error .ne. 0) goto 1000
!
   error  = getrel(fd_nefis_rst, grnams, nameel(8),&
   &uindex, usrord, buflen, zbave     )
   if (error .ne. 0) goto 1000
!
   error  = getrel(fd_nefis_rst, grnams, nameel(9),&
   &uindex, usrord, buflen, zbfl      )
   if (error .ne. 0) goto 1000
!
   error  = getrel(fd_nefis_rst, grnams, nameel(10),&
   &uindex, usrord, buflen, dmed0     )
   if (error .ne. 0) goto 1000
!
   error  = getiel(fd_nefis_rst, grnams, nameel(11),&
   &uindex, usrord, buflen, nrdzdl    )
   if (error .ne. 0) goto 1000
!
   error  = getiel(fd_nefis_rst, grnams, nameel(12),&
   &uindex, usrord, buflen, lanrinbt  )
   if (error .ne. 0) goto 1000
!
   error  = getlel(fd_nefis_rst, grnams, nameel(13),&
   &uindex, usrord, buflen, depos     )
   if (error .ne. 0) goto 1000
!
1000 continue
   neferr = error
end
