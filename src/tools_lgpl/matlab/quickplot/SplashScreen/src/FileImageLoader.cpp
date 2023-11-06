#include "StdAfx.h"
#include "FileImageLoader.h"
#include <sys/stat.h>

CFileImageLoader::CFileImageLoader(LPCTSTR lpszFileName)
{
	m_strFileName = lpszFileName;
}

CFileImageLoader::~CFileImageLoader(void)
{
}

IWICBitmapSource *CFileImageLoader::LoadImage() {
	IWICImagingFactory* imagingFactory = NULL;
	IWICBitmapDecoder* decoder = NULL;
	IWICBitmapSource *ipBitmap = NULL;
	struct _stat64i32 stFileInfo;

	// check exits
	int intStat = _wstat(m_strFileName.c_str(),&stFileInfo); 
	if(intStat != 0) {
		return NULL;
	}

	if (FAILED(CoCreateInstance(CLSID_WICImagingFactory, NULL, CLSCTX_INPROC_SERVER, IID_IWICImagingFactory, (LPVOID*) &imagingFactory))) {
		goto Return;
	}

	if (FAILED(imagingFactory->CreateDecoderFromFilename(m_strFileName.c_str(), NULL, GENERIC_READ, WICDecodeMetadataCacheOnDemand, &decoder))) {
		goto ReleaseDecoder;
	}

	ipBitmap = LoadImageFromDecoder(decoder);

ReleaseDecoder:
	if (decoder!=NULL) {
		decoder->Release();
	}
	imagingFactory->Release();

Return:
	return ipBitmap;
}
