!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling system
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          SOBEK
!
! Programmer:         J. Kuipers
!
! Module:             Sobek file names
!
! Module description: Include file with file names for CMT
!
!
!
! Pre condition:
!
! Post condition:
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!c
!***********************************************************************
! CVS log information:
!
! $Id: filsim.i,v 1.5 1999/06/01 13:42:26 kuipe_j Exp $
!
! History:
! $Log: filsim.i,v $
! Revision 1.5  1999/06/01  13:42:26  kuipe_j
! names in messages substituted + message template
!
! Revision 1.4  1998/11/13  09:03:57  kuipe_j
! aggregationfile in CMT
!
! Revision 1.3  1998/06/11  11:46:59  kuipe_j
! Estuary special integrated
!
! Revision 1.2  1998/06/08  13:15:19  kuipe_j
! time lag hydr controller
!
! Revision 1.1  1998/02/13  12:23:40  kuipe_j
! Adapt to CMT
!
!
!
!
!***********************************************************************
!
character(len=256) rtncod
character(len=256) flwmap, flwhis,                                              &
&              fstrhs  , fqlths  ,                                               &
&              minmax  ,                                                         &
&              fihmap  , fihhis  ,                                               &
&              fipmap  , fiphis  ,                                               &
&              firmap  , firhis  ,                                               &
&              prhmap  , prhhis  ,                                               &
&              prpmap  , prphis  ,                                               &
&              sltmap  , slthis  ,                                               &
&              sdtmap  , sdthis  ,                                               &
&              mrpmap  , mrphis  ,                                               &
&              gsedmap , gsedhis ,                                               &
&              gfrcmap , gfrchis
character(len=256) trainp, traout,                                              &
&              griout
character(len=256) graout  , gralog
character(len=256) fsgfun, fexare,                                              &
&              fexflo  , flenth  ,                                               &
&              fvolum  , fpoint  ,                                               &
&              fwqino  , fwqinp
character(len=256) nefrda, nefrdf,                                              &
&              nefnda  , nefndf
character(len=256) logfil  , statfl
character(len=256) fresid, ffroud,                                              &
&              fdmprs  , fdmpst  ,                                               &
&              fdmpsl
character(len=256) nefmda  , nefmdf
character(len=256) nefwda  , nefwdf
character(len=256) errtem
character(len=256) gaprinam, gawlev,                                            &
&              gadisch , gaveloc ,                                               &
&              gaconcen
common /simfls/                                                                 &
&             rtncod  ,                                                          &
&             flwmap  , flwhis  , fstrhs  , fqlths  ,                            &
&             minmax  ,                                                          &
&             fihmap  , fihhis  , fipmap  , fiphis  ,                            &
&             firmap  , firhis  , prhmap  , prhhis  ,                            &
&             prpmap  , prphis  , sltmap  , slthis  ,                            &
&             sdtmap  , sdthis  , mrpmap  , mrphis  ,                            &
&             gsedmap , gsedhis , gfrcmap , gfrchis ,                            &
&             graout  , gralog  ,                                                &
&             trainp  , traout  , griout  ,                                      &
&             fsgfun  , fexare  , fexflo  , flenth  ,                            &
&             fvolum  , fpoint  , fwqino  , fwqinp  ,                            &
&             logfil  , statfl  , nefrda  , nefrdf  ,                            &
&             nefnda  , nefndf  , fresid  , ffroud  ,                            &
&             fdmprs  , fdmpst  , fdmpsl  , nefmda  ,                            &
&             nefmdf  , nefwda  , nefwdf  ,                                      &
&             errtem  ,                                                          &
&             gaprinam, gawlev  , gadisch , gaveloc ,                            &
&             gaconcen
