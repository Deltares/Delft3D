subroutine dimgranout (kode   ,nsetim ,sedtim ,ngrain ,grain ,&
&submin ,subplus)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Graded Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             DIMGRANOUT (DIMension GRAINsizes OUTput)
!
! Module description: Determine the dimensions of array GRSIZUN that
!                     will contain the grainsizes of the multi under
!                     layer. In this array memory is allocated only for
!                     those sub under layers which are selected for
!                     output of grain sizes.
!
!                     This routine will be called to process the output
!                     requests for histories and maps specified in the
!                     arrays Sedtim and Sedmap respectively.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 grain(4)          O  Contains codes for characteristic grainsizes
!                         requested for output.
!                         1 = D10         2 = D50
!                         3 = D50         3 = Dmed
!  1 kode              I  Calling code
!                         1 = First call. Process Sedtim.
!                         2 = Second call. Process Sedmap
!  4 ngrain            O  Number of characteristic grainsizes that are
!                         requested for output.
!  2 nsetim            I  Number of entries in sedtim or sedmap.
!  3 sedtim(nsetim)    I  Parameter list for sediment results.
!    (contains sedmap)    For maps:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nsemap) = Report parameter n sub code
!    (contains sedtim) I  For histories
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                       [ (.)      = Report begin time   ]
!                       [ (.)      = Report end time     ]
!                       [ (.)      = Report time step    ]
!                         (i+1)    = Report parameter 1 main code
!                         (i+2)    = Report parameter code 1
!                         (i+3)    = Report parameter sub code 1
!                         (nsetim) = Report parameter n sub code
!  6 submin            O  Number of lowest sub layer with respect to
!                         reference sub layer requested for output.
!  7 subplus           O  Number of highest sub layer with respect to
!                         reference sub layer requested for output.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!     Declaration of parameters
!
   integer      nsetim ,kode   ,ngrain ,submin ,subplus
   integer      sedtim(nsetim) ,grain(4)
!
!     Declaration of local variables
!
   integer      nul    ,i      ,nlc  ,nsk  ,istart ,main ,sub   ,&
   &kgrain ,klay   ,main1unla
   parameter   (main1unla=4 )
   logical      new
!
   if (kode.eq.1) then
      do i=1,4
         grain(i) = 0
      enddo
      submin  = -1
      subplus = -1
      nlc = sedtim(1)
      new = mod(nsetim-nlc,2) .eq. 0
      if (new) then
         nsk = 3
      else
         nsk = 0
      endif
      istart = sedtim(1)+2+nsk
      if ( sedtim(1).eq.0 ) istart = nsetim+1
   else
      istart = 4
   endif

   nul = 0
   do i = istart,nsetim,2
      main = sedtim(i)
      if (main.ge.main1unla) then
         sub    = sedtim(i+1)
         kgrain = mod(main-main1unla,4)+1
         klay   = (main-main1unla)/4
         grain(kgrain) = 1
         if (klay.eq.0) then
            nul = 1
         else if (klay.eq.1) then
            subplus = max(subplus,sub)
         else if (klay.eq.2) then
            submin = max(submin,sub)
         endif
      endif
   enddo
!
   submin  = max(nul-1,submin)
   subplus = max(nul-1,subplus)
   if (kode.eq.2) then
      ngrain = 0
      do i=1,4
         if (grain(i) .gt. 0) then
            ngrain = ngrain + 1
            grain(i) = ngrain
         endif
      enddo
!
      submin  = -submin
      subplus = max(submin,subplus)
   endif

end
