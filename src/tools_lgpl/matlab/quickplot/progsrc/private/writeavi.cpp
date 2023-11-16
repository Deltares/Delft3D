/****************************************************************************
 *
 *  WRITEAVI.CPP: MATLAB MEX interface for writing AVI files.
 *
 ***************************************************************************/

/*----- LGPL --------------------------------------------------------------------
 *
 *   Copyright (C) 2011-2023 Stichting Deltares.
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License as published by the Free Software Foundation version 2.1.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 *   contact: delft3d.support@deltares.nl
 *   Stichting Deltares
 *   P.O. Box 177
 *   2600 MH Delft, The Netherlands
 *
 *   All indications and logos of, and references to, "Delft3D" and "Deltares"
 *   are registered trademarks of Stichting Deltares, and remain the property of
 *   Stichting Deltares. All rights reserved.
 *
 *-------------------------------------------------------------------------------
 *   http://www.deltaressystems.com
 *   $HeadURL: https://repos.deltares.nl/repos/ds/trunk/src/tools/matlab/Delft3D-toolbox/progsrc/private/writeavi.cpp $
 *   $Id: writeavi.cpp 14474 2011-01-07 12:56:41Z jagers $
 */

#define  STRICT
#define  INC_OLE2
#include <windows.h>
#include <windowsx.h>
#include <memory.h>
#include <mmsystem.h>
#include <vfw.h>

#include "mex.h"

//---------------------------------------------------------------------------
// Defines
//---------------------------------------------------------------------------
#define LPLPBI LPBITMAPINFOHEADER *

typedef struct {
   PAVIFILE            pfile;          // pfile
   PAVISTREAM          ps;
   PAVISTREAM          psCompressed;
   LPBITMAPINFO        bitinf;
} AVIFILESTRUCT, FAR * PAVIFILESTRUCT;

/********************************************************************
 *      Check and get string
 ********************************************************************/
char * chkGetString(const mxArray * mxA, const char * StrName)
{
   const int *dimarray;
   int StrLen = -1;
   int ndims = mxGetNumberOfDimensions(mxA);
   if (mxIsChar(mxA) & (ndims == 2)) {
      dimarray = mxGetDimensions(mxA);
      if (dimarray[0]==1)
         StrLen = dimarray[1];
   }

   if (StrLen < 0) {
      char * temp = (char*)mxCalloc(128,sizeof(char));
      strcat(temp,"Invalid ");
      strcat(temp,StrName);
      strcat(temp,".");
      mexErrMsgTxt(temp);
   }
   char * Name = mxArrayToString(mxA);
   if (Name == NULL) {
      mexErrMsgTxt("Memory allocation error.");
   }

   return Name;
}

/********************************************************************
 *      Check and get positive integer
 ********************************************************************/
int chkGetPosInt(const mxArray * mxA)
{
   const int *dimarray;
   int Value = -1;
   int ndims = mxGetNumberOfDimensions(mxA);
   if (mxIsDouble(mxA) & !mxIsComplex(mxA) & (ndims == 2)) {
      dimarray = mxGetDimensions(mxA);
      if ((dimarray[0]==1) & (dimarray[1]==1)) {
         double rValue = mxGetScalar(mxA);
         if (rValue >= 0.0)
            Value = (int)rValue;
      }
   }
   return Value;
}

/********************************************************************
 *      Get pointer to AVI file struct
 ********************************************************************/
PAVIFILESTRUCT GetAVI(const mxArray * mxA)
{
   int pavih = chkGetPosInt(mxA);
   if (pavih < 0)
      mexErrMsgTxt("Invalid AVI handle.");

   PAVIFILESTRUCT pavi = (PAVIFILESTRUCT) pavih;

  /*
   * Check AVIFILE for validity; if not valid
   * DLL will cause segment violation
   *
  AVIFILEINFO pfi;
  int x =  AVIFileInfo(pfile,(LPAVIFILEINFO)(&pfi),sizeof(pfi));
  printf("error = %i",x);
   */
   return pavi;
}


void mexFunction(
int nlhs,              // Number of left hand side (output) arguments
mxArray *plhs[],       // Array of left hand side arguments
int nrhs,              // Number of right hand side (input) arguments
const mxArray *prhs[]  // Array of right hand side arguments
)
{
   int i;
   int ndims, d;
   const int *dimarray;
   LPBYTE vals, frame;

   char str[256];
  /*
   printf("%i input argument(s) and %i output argument(s).\n",nrhs,nlhs);
   for (i=0; i<nrhs; i++) {
      printf("arg%2.2i: ",i);
      ndims=mxGetNumberOfDimensions(prhs[i]);
      dimarray=mxGetDimensions(prhs[i]);
      printf("%i",dimarray[0]);
      for (d=1; d<ndims; d++) {
         printf("x%i",dimarray[d]);
      }
      printf(" %s\n",mxGetClassName(prhs[i]));
   }
   */
   if (nrhs<1)
      mexErrMsgTxt("Missing AVI command.");
   else if ( mxIsChar(prhs[0]) != 1)
      mexErrMsgTxt("Missing AVI command.");
   char * cmdStr = chkGetString(prhs[0],"AVI command");

   PAVIFILESTRUCT pavifil = NULL;
   HRESULT hr;
  /******************************************************************
   *    Compare command string with implemented commands
   ******************************************************************/
   if (strcmp(cmdStr,"initialize")==0) {
   /*****************************************************************
    *   INITIALIZE
    *****************************************************************/
      if (nlhs != 1)
         mexErrMsgTxt("Invalid number of output arguments.");
      else if (nrhs != 1)
         mexErrMsgTxt("Too many input arguments.");
      else {
         WORD wVer;

         /* first let's make sure we are running on 1.1 */
         wVer = HIWORD(VideoForWindowsVersion());
         if (wVer < 0x010a) {
            /* oops, we are too old, blow out of here */
            sprintf(str,"Video for Windows version too old: %i.%i<1.10\n",
            wVer/0x0100,wVer-(wVer/0x0100)*0x0100);
            mexErrMsgTxt(str);
         }
         else {
            AVIFileInit();

            pavifil = (PAVIFILESTRUCT) malloc(sizeof(AVIFILESTRUCT));
            pavifil->pfile        = NULL;
            pavifil->ps           = NULL;
            pavifil->psCompressed = NULL;
            pavifil->bitinf       = NULL;
         }

         int pavih = (int) pavifil;
         plhs[0] = mxCreateDoubleMatrix(1,1,mxREAL);
         *mxGetPr(plhs[0])=(double)(pavih);
      }
   }
   else if (strcmp(cmdStr,"getoptions")==0) {
   /*****************************************************************
    *   GETOPTIONS
    *****************************************************************/
      if (nlhs != 1)
         mexErrMsgTxt("Invalid number of output arguments.");
      else if (nrhs != 2 && nrhs != 3)
         mexErrMsgTxt("Invalid number of input arguments.");
      else {
         mxArray * val;

         // Color resolution ...
         int bits = chkGetPosInt(prhs[1]);
         if (bits != 8 && bits != 24 && bits != 32)
            mexErrMsgTxt("Invalid colour depth");
         //
         if (nrhs == 3) {
            if (strcmp(mxGetClassName(prhs[2]),"struct") != 0)
               mexErrMsgTxt("Invalid compression structure");
         }
         //
         // Fill in the header for the video stream....
         LPBITMAPINFO  bitinf;
         int LenCMap = (bits <= 8) ? 1<<bits : 0;
         int sz_bitinf = sizeof(BITMAPINFOHEADER)+LenCMap*sizeof(RGBQUAD);
         //
         bitinf = (LPBITMAPINFO) calloc(sz_bitinf,sizeof(BYTE));
         _fmemset(bitinf, 0, sz_bitinf);
         //
         int width = 100;  // dummy width
         int height = 100; // dummy height
         bitinf->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
         bitinf->bmiHeader.biWidth = width ;
         bitinf->bmiHeader.biHeight = height ;
         bitinf->bmiHeader.biPlanes = 1 ;
         bitinf->bmiHeader.biBitCount = (WORD) bits ;
         bitinf->bmiHeader.biCompression = BI_RGB ;
         bitinf->bmiHeader.biSizeImage = ((width*bits+31)/32 * 4)*height ;
         bitinf->bmiHeader.biXPelsPerMeter = 0 ;
         bitinf->bmiHeader.biYPelsPerMeter = 0 ;
         bitinf->bmiHeader.biClrUsed = LenCMap;
         bitinf->bmiHeader.biClrImportant = 0 ;
         if (bits == 8) {
            // leave bmiColors random
         }

         /* Use Foreground Window to make sure the options dialog is
          * shown on top!
          */
         HWND win = GetForegroundWindow();

         COMPVARS xcompvars;
         _fmemset(&xcompvars, 0, sizeof(xcompvars));
         xcompvars.cbSize=sizeof(xcompvars);
         xcompvars.fccType=ICTYPE_VIDEO; // cdiv / vidc
         if (nrhs==3) {
            //
            xcompvars.dwFlags = ICMF_COMPVARS_VALID;
            //
            val = mxGetField(prhs[2], 0, "fccHandler");
            if (val != NULL) {
               double rValue = mxGetScalar(val);
               xcompvars.fccHandler = (int)rValue;
            }
            else xcompvars.dwFlags = 0;
            //
            val = mxGetField(prhs[2], 0, "KeyFrames");
            if (val != NULL) {
               double rValue = mxGetScalar(val);
               xcompvars.lKey = (int)rValue;
            }
            else xcompvars.dwFlags = 0;
            //
            val = mxGetField(prhs[2], 0, "Quality");
            if (val != NULL) {
               double rValue = mxGetScalar(val);
               xcompvars.lQ = (int)rValue;
            }
            else xcompvars.dwFlags = 0;
            //
            val = mxGetField(prhs[2], 0, "BytesPerSec");
            if (val != NULL) {
               double rValue = mxGetScalar(val);
               xcompvars.lDataRate = (int)rValue;
            }
            else xcompvars.dwFlags = 0;
            //
            val = mxGetField(prhs[2], 0, "Parameters");
            if (val != NULL) {
               vals = (LPBYTE)mxGetData(val);
               int nel = mxGetNumberOfElements(val);
               xcompvars.lpState = (void *) calloc(nel,sizeof(BYTE));
               frame = (LPBYTE)xcompvars.lpState;
               for (i=0; i<xcompvars.cbState; i++) {
                  frame[i] = vals[i]; //lpState
               }
            }
            else xcompvars.dwFlags = 0;
         }
/*
         printf("cbSize      = %i [LONG]\n",xcompvars.cbSize);
         printf("dwFlags     = %i [DWORD]\n",xcompvars.dwFlags);
         printf("hic         = %i [HIC]\n",xcompvars.hic);
         printf("fccType     = %i [DWORD]\n",xcompvars.fccType);
         printf("fccHandler  = %i [DWORD]\n",xcompvars.fccHandler);
         printf("lpbiIn      = %i [LPBITMAPINFO]\n",xcompvars.lpbiIn);
         printf("lpbiOut     = %i [LPBITMAPINFO]\n",xcompvars.lpbiOut);
         printf("lpBitsOut   = %i [LPVOID]\n",xcompvars.lpBitsOut);
         printf("lpBitsPrev  = %i [LPVOID]\n",xcompvars.lpBitsPrev);
         printf("lFrame      = %i [LONG]\n",xcompvars.lFrame);
         printf("lKey        = %i [LONG]\n",xcompvars.lKey);
         printf("lDataRate   = %i [LONG]\n",xcompvars.lDataRate);
         printf("lQ          = %i [LONG]\n",xcompvars.lQ);
         printf("lKeyCount   = %i [LONG]\n",xcompvars.lKeyCount);
         printf("lpState     = %i [LPVOID]\n",xcompvars.lpState);
         printf("cbState     = %i [LONG]\n",xcompvars.cbState);
         printf("--------------------------------\n");
 */
         ICCompressorChoose(win,
         ICMF_CHOOSE_KEYFRAME || ICMF_CHOOSE_DATARATE ,
         bitinf, NULL, &xcompvars,"Compression Options");
/*
         printf("cbSize      = %i [LONG]\n",xcompvars.cbSize);
         printf("dwFlags     = %i [DWORD]\n",xcompvars.dwFlags);
         printf("hic         = %i [HIC]\n",xcompvars.hic);
         printf("fccType     = %i [DWORD]\n",xcompvars.fccType);
         printf("fccHandler  = %i [DWORD]\n",xcompvars.fccHandler);
         printf("lpbiIn      = %i [LPBITMAPINFO]\n",xcompvars.lpbiIn);
         printf("lpbiOut     = %i [LPBITMAPINFO]\n",xcompvars.lpbiOut);
         printf("lpBitsOut   = %i [LPVOID]\n",xcompvars.lpBitsOut);
         printf("lpBitsPrev  = %i [LPVOID]\n",xcompvars.lpBitsPrev);
         printf("lFrame      = %i [LONG]\n",xcompvars.lFrame);
         printf("lKey        = %i [LONG]\n",xcompvars.lKey);
         printf("lDataRate   = %i [LONG]\n",xcompvars.lDataRate);
         printf("lQ          = %i [LONG]\n",xcompvars.lQ);
         printf("lKeyCount   = %i [LONG]\n",xcompvars.lKeyCount);
         printf("lpState     = %i [LPVOID]\n",xcompvars.lpState);
         printf("cbState     = %i [LONG]\n",xcompvars.cbState);
 */
         const char * fld[5];
         fld[0]="fccHandler";
         fld[1]="KeyFrames";
         fld[2]="Quality";
         fld[3]="BytesPerSec";
         fld[4]="Parameters";
         plhs[0] = mxCreateStructMatrix(1,1,5,&fld[0]);
         //
         // fccHandler
         val = mxCreateDoubleMatrix(1,1,mxREAL);
         *mxGetPr(val)=(double)(xcompvars.fccHandler);
         mxSetField(plhs[0], 0, fld[0], val);
         //
         // KeyFrames
         val = mxCreateDoubleMatrix(1,1,mxREAL);
         *mxGetPr(val)=(double)(xcompvars.lKey);
         mxSetField(plhs[0], 0, fld[1], val);
         //
         // Quality
         val = mxCreateDoubleMatrix(1,1,mxREAL);
         *mxGetPr(val)=(double)(xcompvars.lQ);
         mxSetField(plhs[0], 0, fld[2], val);
         //
         // BytesPerSec
         val = mxCreateDoubleMatrix(1,1,mxREAL);
         *mxGetPr(val)=(double)(xcompvars.lDataRate);
         mxSetField(plhs[0], 0, fld[3], val);
         //
         // Parameters
         val = mxCreateNumericMatrix(1,xcompvars.cbState,mxUINT8_CLASS,mxREAL);
         vals = (LPBYTE)mxGetData(val);
         frame = (LPBYTE)xcompvars.lpState;
         for (i=0; i<xcompvars.cbState; i++) {
            vals[i] = frame[i]; //lpState
         }
         mxSetField(plhs[0], 0, fld[4], val);
      }
   }
   else {
      if (nlhs != 1)
         mexErrMsgTxt("Invalid number of output arguments.");
      else if (nrhs<2)
         mexErrMsgTxt("Missing AVI File ID.");
      else {
         pavifil = GetAVI(prhs[1]);
         /*
         printf("AVI PFile = %i\n",pavifil->pfile);
          */
         if (strcmp(cmdStr,"open")==0) {
   /*****************************************************************
    *   OPEN
    *****************************************************************/
            if (nlhs != 1)
               mexErrMsgTxt("Invalid number of output arguments.");
            else if (nrhs<3)
               mexErrMsgTxt("Missing File Name.");
            else if ( mxIsChar(prhs[2]) != 1)
               mexErrMsgTxt("Missing File Name.");
            else {
               char * FileName = chkGetString(prhs[2],"File Name");

               hr = AVIFileOpen(&pavifil->pfile, // returned file pointer
               FileName,                  // file name
               OF_WRITE | OF_CREATE,      // mode to open file with
               NULL);                     // use handler determined
               /*
               printf("AVI PFile = %i\n",pavifil->pfile);
                */
            }
         }
         else if (strcmp(cmdStr,"finalize")==0) {
   /*****************************************************************
    *   FINALIZE
    *****************************************************************/
            if (nlhs != 1)
               mexErrMsgTxt("Invalid number of output arguments.");
            else if (nrhs != 2)
               mexErrMsgTxt("Invalid number of input arguments.");
            else {
               AVIFileExit();
               free(pavifil);

               plhs[0] = mxCreateDoubleMatrix(1,1,mxREAL);
               *mxGetPr(plhs[0])=(double)(0);
            }
         }
         else if (strcmp(cmdStr,"addvideo")==0) {
   /*****************************************************************
    *   ADDVIDEO
    *****************************************************************/
            if (nlhs != 1)
               mexErrMsgTxt("Invalid number of output arguments.");
            else if (nrhs != 7 && nrhs != 8)
               mexErrMsgTxt("Invalid number of input arguments.");
            else {
               // Frame rate (fps) ...
               int fps = chkGetPosInt(prhs[2]);
               if (fps<=0)
                  mexErrMsgTxt("Invalid frame rate");
               // Frame size ...
               int height = chkGetPosInt(prhs[3]);
               if (height<=0)
                  mexErrMsgTxt("Invalid frame height");
               int width = chkGetPosInt(prhs[4]);
               if (width<=0)
                  mexErrMsgTxt("Invalid frame width");
               // Color resolution ...
               int bits = chkGetPosInt(prhs[5]);
               int NClrs = 0;
               if (bits == 8) {
                  if (nrhs != 8)
                     mexErrMsgTxt("Missing colour table");
                  else {
                     if (strcmp(mxGetClassName(prhs[6]),"uint8") != 0)
                        mexErrMsgTxt("Colour table must be Nx3 uint8 array");
                     ndims=mxGetNumberOfDimensions(prhs[6]);
                     if (ndims != 2)
                        mexErrMsgTxt("Invalid size of colour table");
                     dimarray=mxGetDimensions(prhs[6]);
                     if (dimarray[1] != 3 )
                        mexErrMsgTxt("Invalid number of columns in colour table");
                     if (dimarray[0] > 256)
                        mexErrMsgTxt("Too many colours in colour table");
                     NClrs = dimarray[0];

                  }
               }
               else if (bits == 24 || bits == 32) {
                  if (nrhs != 7)
                     mexErrMsgTxt("Too many input arguments");
               }
               else
                  mexErrMsgTxt("Invalid colour depth");
               if (strcmp(mxGetClassName(prhs[nrhs-1]),"struct") != 0)
                  mexErrMsgTxt("Invalid compression structure");
               //
               // Fill in the header for the video stream....
               AVISTREAMINFO strhdr;
               /*
               printf("AVI PS = %i\n",pavifil->ps);
               printf("AVI PS-COMPRESSED = %i\n",pavifil->psCompressed);
                */
               int LenCMap = (bits <= 8) ? 1<<bits : 0;
               int sz_bitinf = sizeof(BITMAPINFOHEADER)+LenCMap*sizeof(RGBQUAD);
               pavifil->bitinf = (LPBITMAPINFO) calloc(sz_bitinf,sizeof(BYTE));
               _fmemset(pavifil->bitinf, 0, sz_bitinf);
               //
               pavifil->bitinf->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
               pavifil->bitinf->bmiHeader.biWidth = width ;
               pavifil->bitinf->bmiHeader.biHeight = height ;
               pavifil->bitinf->bmiHeader.biPlanes = 1 ;
               pavifil->bitinf->bmiHeader.biBitCount = (WORD) bits ;
               pavifil->bitinf->bmiHeader.biCompression = BI_RGB ;
               pavifil->bitinf->bmiHeader.biSizeImage = ((width*bits+31)/32 * 4)*height ;
               pavifil->bitinf->bmiHeader.biXPelsPerMeter = 0 ;
               pavifil->bitinf->bmiHeader.biYPelsPerMeter = 0 ;
               pavifil->bitinf->bmiHeader.biClrUsed = LenCMap;
               pavifil->bitinf->bmiHeader.biClrImportant = 0 ;
               if (bits == 8) {
                  vals = (LPBYTE) mxGetData(prhs[6]);
                  for (i=0; i<NClrs; i++) {
                     pavifil->bitinf->bmiColors[i].rgbRed   = vals[i];
                     pavifil->bitinf->bmiColors[i].rgbGreen = vals[i+NClrs];
                     pavifil->bitinf->bmiColors[i].rgbBlue  = vals[i+2*NClrs];
                  }
               }

               // Fill in the header for the video stream....
               AVICOMPRESSOPTIONS opts;
               mxArray * val;

               // Set the compressor options ...
               _fmemset(&opts, 0, sizeof(opts));
               //
               opts.fccType           = streamtypeVIDEO;
               //
               val = mxGetField(prhs[nrhs-1], 0, "fccHandler");
               if (val != NULL) {
                  double rValue = mxGetScalar(val);
                  opts.fccHandler = (int)rValue;
               }
               else mexErrMsgTxt("Missing fccHandler: Invalid compression structure");
               //
               val = mxGetField(prhs[nrhs-1], 0, "KeyFrames");
               if (val != NULL) {
                  double rValue = mxGetScalar(val);
                  opts.dwKeyFrameEvery = (int)rValue;
               }
               else mexErrMsgTxt("Missing KeyFrames: Invalid compression structure");
               //
               val = mxGetField(prhs[nrhs-1], 0, "Quality");
               if (val != NULL) {
                  double rValue = mxGetScalar(val);
                  opts.dwQuality = (int)rValue;
               }
               else mexErrMsgTxt("Missing Quality: Invalid compression structure");
               //
               val = mxGetField(prhs[nrhs-1], 0, "BytesPerSec");
               if (val != NULL) {
                  double rValue = mxGetScalar(val);
                  opts.dwBytesPerSecond = (int)rValue;
               }
               else mexErrMsgTxt("Missing BytesPerSec: Invalid compression structure");
               //
               opts.dwFlags           = AVICOMPRESSF_VALID;
               if (opts.dwKeyFrameEvery>0)
                  opts.dwFlags = opts.dwFlags && AVICOMPRESSF_KEYFRAMES;
               opts.lpFormat          = NULL;
               opts.cbFormat          = 0;
               //
               val = mxGetField(prhs[nrhs-1], 0, "Parameters");
               if (val != NULL) {
                  vals = (LPBYTE)mxGetData(val);
                  opts.cbParms = mxGetNumberOfElements(val);
                  opts.lpParms = (void *) calloc(opts.cbParms,sizeof(BYTE));
                  frame = (LPBYTE)opts.lpParms;
                  for (i=0; i<(int)opts.cbParms; i++) {
                     frame[i] = vals[i]; //lpParms
                  }
               }
               else mexErrMsgTxt("Missing Parameters: Invalid compression structure");
               //
               opts.dwInterleaveEvery = 0;
               //
/*
               AVICOMPRESSOPTIONS FAR * aopts[1] = {&opts};
               if (!AVISaveOptions(win, ICMF_CHOOSE_KEYFRAME, 1, &pavifil->ps, (LPAVICOMPRESSOPTIONS FAR *) &aopts))
                  goto error;

               printf("fccType          = %i [DWORD]\n",opts.fccType);
               printf("fccHandler       = %i [DWORD]\n",opts.fccHandler);
               printf("dwKeyFrameEvery  = %i [DWORD]\n",opts.dwKeyFrameEvery);
               printf("dwQuality        = %i [DWORD]\n",opts.dwQuality);
               printf("dwBytesPerSecond = %i [DWORD]\n",opts.dwBytesPerSecond);
               printf("dwFlags          = %i [DWORD]\n",opts.dwFlags);
               printf("lpFormat         = %i [LPVOID]\n",opts.lpFormat);
               printf("cbFormat         = %i [DWORD]\n",opts.cbFormat);
               printf("lpParms          = %i [LPVOID]\n",opts.lpParms);
               printf("cbParms          = %i [DWORD]\n",opts.cbParms);
               printf("dwInterleaveEvery= %i [DWORD]\n",opts.dwInterleaveEvery);
 */

               // The video stream will run with fps Frames per Second ....
               _fmemset(&strhdr, 0, sizeof(strhdr));
               strhdr.fccType                = streamtypeVIDEO;// stream type
               strhdr.fccHandler             = opts.fccHandler;
               strhdr.dwScale                = 1;
               strhdr.dwRate                 = fps;
               strhdr.dwSuggestedBufferSize  = pavifil->bitinf->bmiHeader.biSizeImage;
               SetRect(&strhdr.rcFrame, 0, 0, width, height);

               // And create the stream;
               hr = AVIFileCreateStream(pavifil->pfile,    // file pointer
               &pavifil->ps,    // returned stream pointer
               &strhdr);        // stream header
               if (hr != AVIERR_OK)
                  goto error;

/*               char *hh;
               hh = "INFOILNGA000Undefined IART8000Artist  ";
               hh[8]=10;
               hh[9]=0;
               hh[10]=0;
               hh[11]=0;
               hh[26]=8;
               hh[27]=0;
               hh[28]=0;
               hh[29]=0;
               //hr = AVIFileWriteData(pavifil->pfile, 1414744396,
               //   hh,38);
               hr = AVIStreamWriteData(pavifil->ps, 1414744396,
                  hh,38); */

               // Create compressed stream;
               hr = AVIMakeCompressedStream(&pavifil->psCompressed,
               pavifil->ps, &opts, NULL);
               if (hr != AVIERR_OK)
                  goto error;

               hr = AVIStreamSetFormat(pavifil->psCompressed, 0,
               pavifil->bitinf,    // stream format
               pavifil->bitinf->bmiHeader.biSize +   // format size
               pavifil->bitinf->bmiHeader.biClrUsed * sizeof(RGBQUAD));
               if (hr != AVIERR_OK)
                  goto error;

               // error trap
               error:
               /*
               printf("AVI PS = %i\n",pavifil->ps);
               printf("AVI PS-COMPRESSED = %i\n",pavifil->psCompressed);
                */
                  ;
            }
         }
         else if (strcmp(cmdStr,"addframe")==0) {
   /*****************************************************************
    *   ADDFRAME
    *****************************************************************/
            if (nlhs != 1)
               mexErrMsgTxt("Invalid number of output arguments.");
            else if (nrhs != 4)
               mexErrMsgTxt("Invalid number of input arguments.");
            else {
               // Frame iframe ...
               int iframe = chkGetPosInt(prhs[3]);
               if (iframe<=0)
                  mexErrMsgTxt("Invalid frame iframe");
               iframe = iframe-1; // first frame is zero
               //
               if (pavifil->bitinf->bmiHeader.biBitCount==8) {
                  sprintf(str,"Frame should be an %ix%i uint8 array.",
                  pavifil->bitinf->bmiHeader.biWidth,
                  pavifil->bitinf->bmiHeader.biHeight);
                  if (strcmp(mxGetClassName(prhs[2]),"uint8")==0) {
                     ndims=mxGetNumberOfDimensions(prhs[2]);
                     if (ndims == 2) {
                        dimarray=mxGetDimensions(prhs[2]);
                        if (dimarray[0] == pavifil->bitinf->bmiHeader.biHeight &&
                        dimarray[1] == pavifil->bitinf->bmiHeader.biWidth) {
                           //
                           vals = (LPBYTE) mxGetData(prhs[2]);
                           frame = (LPBYTE) mxCalloc(pavifil->bitinf->bmiHeader.biSizeImage,
                           sizeof(BYTE));
                           //
                           int m,n,i1,i2;
                           int bh = pavifil->bitinf->bmiHeader.biHeight;
                           int bw = pavifil->bitinf->bmiHeader.biWidth;
                           i2=0;
                           for (m=0; m<pavifil->bitinf->bmiHeader.biHeight; m++)
                              for (n=0; n<pavifil->bitinf->bmiHeader.biWidth; n++) {
                                 frame[i2] = vals[bh-m-1 + n*bh];
                                 i2=i2+1;
                              }
                           //
                           hr = AVIStreamWrite(pavifil->psCompressed, // stream pointer
                           iframe, // time of this frame
                           1, // number to write
                           frame,
                           pavifil->bitinf->bmiHeader.biSizeImage, // size of this frame
                           0, // flags....
                           NULL,
                           NULL);
                           //
                           mxFree(frame);
                        }
                        else mexErrMsgTxt(str);
                     }
                     else mexErrMsgTxt(str);
                  }
                  else mexErrMsgTxt(str);
               }
               else {
                  sprintf(str,"Frame should be an %ix%ix3 uint8 array.",
                  pavifil->bitinf->bmiHeader.biWidth,
                  pavifil->bitinf->bmiHeader.biHeight);
                  if (strcmp(mxGetClassName(prhs[2]),"uint8")==0) {
                     ndims=mxGetNumberOfDimensions(prhs[2]);
                     if (ndims == 3) {
                        dimarray=mxGetDimensions(prhs[2]);
                        if (dimarray[0] == pavifil->bitinf->bmiHeader.biHeight &&
                        dimarray[1] == pavifil->bitinf->bmiHeader.biWidth  &&
                        dimarray[2] == 3) {
                           //
                           vals = (LPBYTE) mxGetData(prhs[2]);
                           frame = (LPBYTE) mxCalloc(pavifil->bitinf->bmiHeader.biSizeImage,
                           sizeof(BYTE));
                           //
                           int m,n,k,i1,i2;
                           int bh = pavifil->bitinf->bmiHeader.biHeight;
                           int bw = pavifil->bitinf->bmiHeader.biWidth;
                           i2=0;
                           for (m=0; m<pavifil->bitinf->bmiHeader.biHeight; m++)
                              for (n=0; n<pavifil->bitinf->bmiHeader.biWidth; n++) {
                                 for (k=0; k<3; k++) {
                                    frame[i2] = vals[bh-m-1 + n*bh + (2-k)*bw*bh];
                                    i2=i2+1;
                                 }
                                 if (pavifil->bitinf->bmiHeader.biBitCount==32)
                                    i2=i2+1;
                              }
                           //
                           hr = AVIStreamWrite(pavifil->psCompressed, // stream pointer
                           iframe, // time of this frame
                           1, // number to write
                           frame,
                           pavifil->bitinf->bmiHeader.biSizeImage, // size of this frame
                           AVIIF_KEYFRAME, // flags.... AVIIF_KEYFRAME
                           NULL,
                           NULL);
                           //
                           mxFree(frame);
                        }
                        else mexErrMsgTxt(str);
                     }
                     else mexErrMsgTxt(str);
                  }
                  else mexErrMsgTxt(str);
               }
            }
         }
         else if (strcmp(cmdStr,"close")==0) {
   /*****************************************************************
    *   CLOSE
    *****************************************************************/
            if (nlhs != 1)
               mexErrMsgTxt("Invalid number of output arguments.");
            else if (nrhs != 2)
               mexErrMsgTxt("Invalid number of input arguments.");
            else {
               if (pavifil->ps) {
                  AVIStreamClose(pavifil->ps);
                  pavifil->ps = NULL;
               }

               if (pavifil->psCompressed) {
                  AVIStreamClose(pavifil->psCompressed);
                  pavifil->psCompressed = NULL;
               }

               if (pavifil->pfile) {
                  AVIFileClose(pavifil->pfile);
                  pavifil->pfile = NULL;
               }
            }
         }
         else {
   /*****************************************************************
    *   Unrecognized command.
    *****************************************************************/
            mexErrMsgTxt("Unrecognized AVI command.");
         }
      }

      int pavih = (int) pavifil;
      plhs[0] = mxCreateDoubleMatrix(1,1,mxREAL);
      *mxGetPr(plhs[0])=(double)(pavih);
   }
}
