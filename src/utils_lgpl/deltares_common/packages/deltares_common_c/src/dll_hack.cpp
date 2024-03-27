#include <iostream>

#ifdef _WIN32
#   include <windows.h>
#   include <libloaderapi.h>
#   include <errhandlingapi.h>
#   define EXTERNAL __declspec(dllimport)
#elif __linux__
#   define _GNU_SOURCE
#   include <dlfcn.h>
#   define EXTERNAL
#endif // _WIN32

extern "C" {
    EXTERNAL int nc_def_var(int ncid, const char* name, int xtype, int ndims, const int* dimidsp, int* varidp);
}

static void dll_hack_imp();

extern "C" {
    void dll_hack() {
        dll_hack_imp();
    }
}

#ifdef _WIN32
void dll_hack_imp() {
    char path[MAX_PATH];
    HMODULE hm = NULL;

    if (GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
            GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
            (LPCSTR) &nc_def_var, &hm) == 0) {
        int ret = GetLastError();
        std::cerr << "GetModuleHandle failed, error = " << ret << std::endl;
        return;
    }
    if (GetModuleFileName(hm, path, sizeof(path)) == 0) {
        int ret = GetLastError();
        std::cerr << "GetModuleHandle failed, error = " << ret << std::endl;
        return;
    }

    std::cout << "NetCDF dll path is '" << path << "'.\n";
}
#elif __linux__
void dll_hack_imp() {
    Dl_info dl_info;
    dladdr((void*)nc_def_var, &dl_info);

    std::cout << "NetCDF so path is '" << dl_info.dli_fname << "'.\n";
}
#endif // _WIN32
