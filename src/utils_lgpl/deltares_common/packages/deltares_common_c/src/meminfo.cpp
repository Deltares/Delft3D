#include "meminfo.h"
#if linux
#include <sys/sysctl.h>
#endif

#ifdef WIN32

unsigned __int64 MemInfo::GetTotalMemSize()
{
  MEMORYSTATUSEX status;
  status.dwLength = sizeof(status);
  GlobalMemoryStatusEx(&status);
  return unsigned (__int64 (status.ullTotalPhys));
}
#elif defined(_SC_PHYS_PAGES)
unsigned long long MemInfo::GetTotalMemSize()
{
long long pages = sysconf(_SC_PHYS_PAGES);
long long page_size = sysconf(_SC_PAGE_SIZE);
return (pages * page_size);
}

#else
unsigned long long MemInfo::GetTotalMemSize()
{
int mib[2];
size_t len;
long long totalphys64;

mib[0] = CTL_HW;
mib[1] = HW_MEMSIZE; /* gives a 64 bit int */
len = sizeof(totalphys64);
sysctl(mib, 2, &totalphys64, &len, NULL, 0);
return totalphys64;
}
#endif
