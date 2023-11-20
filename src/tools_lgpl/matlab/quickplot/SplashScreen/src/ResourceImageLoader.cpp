#include "StdAfx.h"
#include "ResourceImageLoader.h"
#include "wincodec.h"

CResourceImageLoader::CResourceImageLoader(LPCTSTR lpName, LPCTSTR lpType)
{
	m_strName=lpName;
	m_strType=lpType;
}

CResourceImageLoader::~CResourceImageLoader(void)
{
}

IWICBitmapSource *CResourceImageLoader::LoadImage() {
	// load the PNG image data into a stream
    IStream * ipImageStream = CreateStreamOnResource(m_strName, m_strType.c_str());
    if (ipImageStream == NULL)
        goto Return;

    // load the bitmap with WIC
    IWICBitmapSource * ipBitmap = LoadBitmapFromStream(ipImageStream);

	ipImageStream->Release();

Return:
	return ipBitmap;
}
IStream *CResourceImageLoader::CreateStreamOnResource(LPCTSTR lpName, LPCTSTR lpType)
{
	// initialize return value
	IStream * ipStream = NULL;

	// find the resource
	HRSRC hrsrc = FindResource(NULL, lpName, lpType);
	if (hrsrc == NULL)
		goto Return;

	// load the resource
	DWORD dwResourceSize = SizeofResource(NULL, hrsrc);
	HGLOBAL hglbImage = LoadResource(NULL, hrsrc);
	if (hglbImage == NULL)
		goto Return;

	// lock the resource, getting a pointer to its data
	LPVOID pvSourceResourceData = LockResource(hglbImage);
	if (pvSourceResourceData == NULL)
		goto Return;

	// allocate memory to hold the resource data
	HGLOBAL hgblResourceData = GlobalAlloc(GMEM_MOVEABLE, dwResourceSize);
	if (hgblResourceData == NULL)
		goto Return;

	// get a pointer to the allocated memory
	LPVOID pvResourceData = GlobalLock(hgblResourceData);
	if (pvResourceData == NULL)
		goto FreeData;

	// copy the data from the resource to the new memory block
	CopyMemory(pvResourceData, pvSourceResourceData, dwResourceSize);
	GlobalUnlock(hgblResourceData);

	// create a stream on the HGLOBAL containing the data
	if (SUCCEEDED(CreateStreamOnHGlobal(hgblResourceData, TRUE, &ipStream)))
		goto Return;

FreeData:
	// couldn't create stream; free the memory
	GlobalFree(hgblResourceData);

Return:
	// no need to unlock or free the resource
	return ipStream;
}

// Loads a PNG image from the specified stream (using Windows Imaging Component).
IWICBitmapSource *CResourceImageLoader::LoadBitmapFromStream(IStream * ipImageStream)
{
	IWICImagingFactory* imagingFactory = NULL;
	IWICBitmapDecoder* decoder = NULL;
	IWICBitmapSource *ipBitmap = NULL;

	if (FAILED(CoCreateInstance(CLSID_WICImagingFactory, NULL, CLSCTX_INPROC_SERVER, IID_IWICImagingFactory, (LPVOID*) &imagingFactory))) {
		goto Return;
	}

	if (FAILED(imagingFactory->CreateDecoderFromStream(ipImageStream, NULL, WICDecodeMetadataCacheOnDemand, &decoder))) {
		goto ReleaseDecoder;
	}

	ipBitmap = LoadImageFromDecoder(decoder);

ReleaseDecoder:
	decoder->Release();
	imagingFactory->Release();

Return:
	return ipBitmap;
}
