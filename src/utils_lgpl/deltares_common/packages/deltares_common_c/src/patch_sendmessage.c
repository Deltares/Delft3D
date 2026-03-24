/*
 * patch_sendmessage.c
 *
 * Patches the IAT (Import Address Table) of the main executable at runtime,
 * replacing SendMessageA with a wrapper that uses SendMessageTimeoutA
 * with a 5-second timeout. This prevents the GUI from deadlocking on
 * startup when another process (Outlook, Acrobat, etc.) has a hung
 * message queue that blocks the Win32 broadcast.
 *
 * Interacter is statically linked into dflowfm-cli.exe, so we patch
 * the executable's own IAT via GetModuleHandleA(NULL).
 *
 * Intentionally uses no imagehlp/dbghelp: the PE headers are parsed
 * directly so that no extra import library is required.
 */

#include <windows.h>
#include <stddef.h>

static LRESULT WINAPI SafeSendMessageA(HWND hWnd, UINT Msg,
   WPARAM wParam, LPARAM lParam)
{
   DWORD_PTR result = 0;
   /* SMTO_ABORTIFHUNG skips recipient windows whose thread is not pumping */
   SendMessageTimeoutA(hWnd, Msg, wParam, lParam,
      SMTO_ABORTIFHUNG | SMTO_NORMAL,
      5000, &result);
   return (LRESULT)result;
}

void patch_interacter_sendmessage(void)
{
   /* NULL = the executable itself, which contains the statically linked Interacter */
   HMODULE hMod = GetModuleHandleA(NULL);
   if (!hMod) return;

   /* Walk the PE headers manually - avoids imagehlp.lib dependency */
   PIMAGE_DOS_HEADER pDos = (PIMAGE_DOS_HEADER)hMod;
   PIMAGE_NT_HEADERS pNT = (PIMAGE_NT_HEADERS)((BYTE*)hMod + pDos->e_lfanew);
   DWORD importRVA = pNT->OptionalHeader
      .DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT]
      .VirtualAddress;
   if (!importRVA) return;

   PIMAGE_IMPORT_DESCRIPTOR pImport =
      (PIMAGE_IMPORT_DESCRIPTOR)((BYTE*)hMod + importRVA);

   for (; pImport->Name; pImport++) {
      const char* dllName = (const char*)((BYTE*)hMod + pImport->Name);
      if (_stricmp(dllName, "user32.dll") != 0) continue;

      PIMAGE_THUNK_DATA pThunk =
         (PIMAGE_THUNK_DATA)((BYTE*)hMod + pImport->FirstThunk);
      PIMAGE_THUNK_DATA pOrig =
         (PIMAGE_THUNK_DATA)((BYTE*)hMod + pImport->OriginalFirstThunk);

      for (; pThunk->u1.Function; pThunk++, pOrig++) {
         if (IMAGE_SNAP_BY_ORDINAL(pOrig->u1.Ordinal)) continue;

         PIMAGE_IMPORT_BY_NAME pName =
            (PIMAGE_IMPORT_BY_NAME)((BYTE*)hMod + pOrig->u1.AddressOfData);
         if (_stricmp((char*)pName->Name, "SendMessageA") != 0) continue;

         DWORD oldProt;
         VirtualProtect(&pThunk->u1.Function,
            sizeof(pThunk->u1.Function),
            PAGE_READWRITE, &oldProt);
         pThunk->u1.Function = (ULONG_PTR)SafeSendMessageA;
         VirtualProtect(&pThunk->u1.Function,
            sizeof(pThunk->u1.Function),
            oldProt, &oldProt);
         return;
      }
   }
}