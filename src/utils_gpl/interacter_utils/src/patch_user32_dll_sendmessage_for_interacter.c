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

 /* Converts a Relative Virtual Address (RVA) from the PE header into an
  * absolute pointer, by adding the module's load address as base. */
#define RVA_TO_PTR(base, rva, type) ((type)((BYTE*)(base) + (rva)))

  /* SafeSendMessageA - drop-in replacement for SendMessageA that uses a
   * 5-second timeout. Passed as the replacement function pointer when
   * patching the IAT.
   *   hWnd   : target window handle
   *   Msg    : message identifier
   *   wParam : message-specific first parameter
   *   lParam : message-specific second parameter */
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

/* replace_iat_entry - overwrites a single IAT slot with SafeSendMessageA.
 * The IAT lives in a read-only page, so VirtualProtect is used to make
 * the write legal and then restore the original protection afterwards.
 *   hMod   : base address of the module whose IAT is being patched
 *   pThunk : pointer to the IAT slot to overwrite */
static void replace_iat_entry(HMODULE hMod, PIMAGE_THUNK_DATA pThunk)
{
   /* The IAT slot is in a read-only page; temporarily make it writable */
   DWORD oldProt;
   VirtualProtect(&pThunk->u1.Function,
      sizeof(pThunk->u1.Function),
      PAGE_READWRITE, &oldProt);

   pThunk->u1.Function = (ULONG_PTR)SafeSendMessageA;

   VirtualProtect(&pThunk->u1.Function,
      sizeof(pThunk->u1.Function),
      oldProt, &oldProt);
}

/* patch_user32_dll_sendmessage_for_interacter - entry point called once at startup.
 * Walks the IAT of the main executable, finds the SendMessageA slot
 * in the user32.dll import section, and replaces it with SafeSendMessageA. */
void patch_user32_dll_sendmessage_for_interacter(void)
{
   /* NULL = the executable itself, which contains the statically linked Interacter */
   HMODULE hMod = GetModuleHandleA(NULL);
   if (!hMod) return;

   /* Walk the PE headers to find the import directory */
   PIMAGE_DOS_HEADER pDos = (PIMAGE_DOS_HEADER)hMod;
   PIMAGE_NT_HEADERS pNT = RVA_TO_PTR(hMod, pDos->e_lfanew, PIMAGE_NT_HEADERS);
   DWORD importRVA = pNT->OptionalHeader
      .DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT]
      .VirtualAddress;
   if (!importRVA) return;

   /* Each descriptor describes one imported DLL */
   PIMAGE_IMPORT_DESCRIPTOR pImport = RVA_TO_PTR(hMod, importRVA, PIMAGE_IMPORT_DESCRIPTOR);

   /* Scan import descriptors until we find the user32.dll entry */
   while (pImport->Name) {
      const char* dllName = RVA_TO_PTR(hMod, pImport->Name, const char*);
      if (_stricmp(dllName, "user32.dll") != 0) { pImport++; continue; }

      /* pThunk: the live IAT slots (patched addresses at runtime)
       * pOrig:  the original hint/name table (read-only, used for lookup) */
      PIMAGE_THUNK_DATA pThunk = RVA_TO_PTR(hMod, pImport->FirstThunk, PIMAGE_THUNK_DATA);
      PIMAGE_THUNK_DATA pOrig = RVA_TO_PTR(hMod, pImport->OriginalFirstThunk, PIMAGE_THUNK_DATA);

      /* Scan user32.dll imports by name until SendMessageA is found */
      for (; pThunk->u1.Function; pThunk++, pOrig++) {
         /* Skip entries imported by ordinal - they have no name to match */
         if (IMAGE_SNAP_BY_ORDINAL(pOrig->u1.Ordinal)) continue;

         PIMAGE_IMPORT_BY_NAME pName = RVA_TO_PTR(hMod, pOrig->u1.AddressOfData, PIMAGE_IMPORT_BY_NAME);
         if (_stricmp((char*)pName->Name, "SendMessageA") != 0) continue;

         replace_iat_entry(hMod, pThunk);
         return;
      }
      pImport++;
   }
}