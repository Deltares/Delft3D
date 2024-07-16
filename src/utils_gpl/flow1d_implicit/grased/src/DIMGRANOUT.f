      subroutine dimgranout (kode   ,nsetim ,sedtim ,ngrain ,grain ,
     &                       submin ,subplus)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Graded Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             DIMGRANOUT (DIMension GRAINsizes OUTput)
c
c Module description: Determine the dimensions of array GRSIZUN that
c                     will contain the grainsizes of the multi under
c                     layer. In this array memory is allocated only for
c                     those sub under layers which are selected for 
c                     output of grain sizes.
c
c                     This routine will be called to process the output
c                     requests for histories and maps specified in the 
c                     arrays Sedtim and Sedmap respectively.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 grain(4)          O  Contains codes for characteristic grainsizes 
c                         requested for output.
c                         1 = D10         2 = D50
c                         3 = D50         3 = Dmed
c  1 kode              I  Calling code
c                         1 = First call. Process Sedtim.
c                         2 = Second call. Process Sedmap
c  4 ngrain            O  Number of characteristic grainsizes that are 
c                         requested for output.
c  2 nsetim            I  Number of entries in sedtim or sedmap.
c  3 sedtim(nsetim)    I  Parameter list for sediment results.
c    (contains sedmap)    For maps:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nsemap) = Report parameter n sub code
c    (contains sedtim) I  For histories
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                       [ (.)      = Report begin time   ]
c                       [ (.)      = Report end time     ]
c                       [ (.)      = Report time step    ]
c                         (i+1)    = Report parameter 1 main code
c                         (i+2)    = Report parameter code 1
c                         (i+3)    = Report parameter sub code 1
c                         (nsetim) = Report parameter n sub code
c  6 submin            O  Number of lowest sub layer with respect to 
c                         reference sub layer requested for output.    
c  7 subplus           O  Number of highest sub layer with respect to 
c                         reference sub layer requested for output.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c     Declaration of parameters
c
      integer      nsetim ,kode   ,ngrain ,submin ,subplus
      integer      sedtim(nsetim) ,grain(4)
c
c     Declaration of local variables
c
      integer      nul    ,i      ,nlc  ,nsk  ,istart ,main ,sub   ,
     &             kgrain ,klay   ,main1unla
      parameter   (main1unla=4 )
      logical      new
c
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
c
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
c       
         submin  = -submin
         subplus = max(submin,subplus)
      endif

      end
