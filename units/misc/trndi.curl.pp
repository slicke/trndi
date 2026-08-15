(*
 * Trndi
 * Medical and Non-Medical Usage Alert
 *
 * Copyright (c) Björn Lindh
 * GitHub: https://github.com/slicke/trndi
 *
 * This program is distributed under the terms of the GNU General Public License,
 * Version 3, as published by the Free Software Foundation. You may redistribute
 * and/or modify the software under the terms of this license.
 *
 * A copy of the GNU General Public License should have been provided with this
 * program. If not, see <http://www.gnu.org/licenses/gpl.html>.
 *
 * ================================== IMPORTANT ==================================
 * MEDICAL DISCLAIMER:
 * - This software is NOT a medical device and must NOT replace official continuous
 *   glucose monitoring (CGM) systems or any healthcare decision-making process.
 * - The data provided may be delayed, inaccurate, or unavailable.
 * - DO NOT make medical decisions based on this software.
 * - VERIFY all data using official devices and consult a healthcare professional for
 *   medical concerns or emergencies.
 *
 * LIABILITY LIMITATION:
 * - The software is provided "AS IS" and without any warranty—expressed or implied.
 * - Users assume all risks associated with its use. The developers disclaim all
 *   liability for any damage, injury, or harm, direct or incidental, arising
 *   from its use.
 *
 * INSTRUCTIONS TO DEVELOPERS & USERS:
 * - Any modifications to this file must include a prominent notice outlining what was
 *   changed and the date of modification (as per GNU GPL Section 5).
 * - Distribution of a modified version must include this header and comply with the
 *   license terms.
 *
 * BY USING THIS SOFTWARE, YOU AGREE TO THE TERMS AND DISCLAIMERS STATED HERE.
 *)

unit trndi.curl;

{$mode objfpc}{$H+}

{**
  Minimal libcurl "easy" API binding for Trndi.

  Replaces the unmaintained third-party libpascurl unit. Only the small
  surface actually used by @code(trndi.native.linux) (and its BSD subclass)
  is declared: easy-handle lifecycle, typed @code(curl_easy_setopt)
  overloads, @code(curl_easy_getinfo), header lists, and error strings.

  Constant values were taken from the upstream curl/curl.h numbering scheme
  (option value = type base + offset), which is a stable part of the libcurl
  ABI. Extend this unit as new options are needed rather than reintroducing
  a full-API binding.
}

interface

uses
ctypes;

const
  {$IFDEF WINDOWS}
CurlLib = 'libcurl.dll';
  {$ELSE}
  {$IFDEF DARWIN}
CurlLib = 'libcurl.dylib';
  {$ELSE}
CurlLib = 'libcurl.so';
  {$ENDIF}
  {$ENDIF}

type
  {** Opaque libcurl easy-session handle returned by @link(curl_easy_init). }
CURL = type Pointer;

  {** libcurl result code (C enum, int-sized). @code(CURLE_OK) means success. }
CURLcode = clong;

  {** Option selector for @link(curl_easy_setopt) (C enum, int-sized). }
CURLoption = clong;

  {** Info selector for @link(curl_easy_getinfo) (C enum, int-sized). }
CURLINFO = clong;

pcurl_slist = ^curl_slist;
  {** Singly-linked string list used for custom HTTP headers. }
curl_slist = record
  Data: pchar;
  Next: pcurl_slist;
end;

const
  // --- CURLcode values (subset) ---
CURLE_OK = 0;
CURLE_FAILED_INIT = 2;
CURLE_COULDNT_RESOLVE_HOST = 6;

  // --- curl_global_init flags ---
CURL_GLOBAL_SSL = 1 shl 0;
CURL_GLOBAL_WIN32 = 1 shl 1;
CURL_GLOBAL_ALL = CURL_GLOBAL_SSL or CURL_GLOBAL_WIN32;
CURL_GLOBAL_NOTHING = 0;
CURL_GLOBAL_DEFAULT = CURL_GLOBAL_ALL;

  // --- CURLoption value bases (curl.h numbering scheme) ---
CURLOPTTYPE_LONG = 0;
CURLOPTTYPE_OBJECTPOINT = 10000;
CURLOPTTYPE_FUNCTIONPOINT = 20000;
CURLOPTTYPE_STRINGPOINT = CURLOPTTYPE_OBJECTPOINT;
CURLOPTTYPE_SLISTPOINT = CURLOPTTYPE_OBJECTPOINT;
CURLOPTTYPE_CBPOINT = CURLOPTTYPE_OBJECTPOINT;

  // --- CURLoption values (subset) ---
CURLOPT_WRITEDATA = CURLOPTTYPE_CBPOINT + 1;
CURLOPT_URL = CURLOPTTYPE_STRINGPOINT + 2;
CURLOPT_PROXY = CURLOPTTYPE_STRINGPOINT + 4;
CURLOPT_WRITEFUNCTION = CURLOPTTYPE_FUNCTIONPOINT + 11;
CURLOPT_TIMEOUT = CURLOPTTYPE_LONG + 13;
CURLOPT_POSTFIELDS = CURLOPTTYPE_OBJECTPOINT + 15;
CURLOPT_USERAGENT = CURLOPTTYPE_STRINGPOINT + 18;
CURLOPT_COOKIE = CURLOPTTYPE_STRINGPOINT + 22;
CURLOPT_HTTPHEADER = CURLOPTTYPE_SLISTPOINT + 23;
CURLOPT_HEADERDATA = CURLOPTTYPE_CBPOINT + 29;
CURLOPT_VERBOSE = CURLOPTTYPE_LONG + 41;
CURLOPT_POST = CURLOPTTYPE_LONG + 47;
CURLOPT_FOLLOWLOCATION = CURLOPTTYPE_LONG + 52;
CURLOPT_PROXYPORT = CURLOPTTYPE_LONG + 59;
CURLOPT_POSTFIELDSIZE = CURLOPTTYPE_LONG + 60;
CURLOPT_SSL_VERIFYPEER = CURLOPTTYPE_LONG + 64;
CURLOPT_MAXREDIRS = CURLOPTTYPE_LONG + 68;
CURLOPT_CONNECTTIMEOUT = CURLOPTTYPE_LONG + 78;
CURLOPT_HEADERFUNCTION = CURLOPTTYPE_FUNCTIONPOINT + 79;
CURLOPT_SSL_VERIFYHOST = CURLOPTTYPE_LONG + 81;
CURLOPT_ACCEPT_ENCODING = CURLOPTTYPE_STRINGPOINT + 102;
CURLOPT_PROXYUSERNAME = CURLOPTTYPE_STRINGPOINT + 175;
CURLOPT_PROXYPASSWORD = CURLOPTTYPE_STRINGPOINT + 176;

  // --- CURLINFO value bases and values (subset) ---
CURLINFO_STRING = $100000;
CURLINFO_LONG = $200000;
CURLINFO_EFFECTIVE_URL = CURLINFO_STRING + 1;
CURLINFO_RESPONSE_CODE = CURLINFO_LONG + 2;
CURLINFO_REDIRECT_COUNT = CURLINFO_LONG + 20;

{**
  One-shot global libcurl initialisation; call once before any other libcurl
  use, ideally before threads start. Pass @link(CURL_GLOBAL_DEFAULT).
}
function curl_global_init(flags: clong): CURLcode;
cdecl; external CurlLib;

{** Creates an easy session handle, or @nil on failure. }
function curl_easy_init: CURL;
cdecl; external CurlLib;

{**
  Sets a long-typed option (timeouts, booleans, ports, sizes).
  Typed overloads replace C's variadic @code(curl_easy_setopt) so the
  compiler checks the argument against the option class.
}
function curl_easy_setopt(handle: CURL; option: CURLoption; Value: clong): CURLcode;
cdecl; overload; external CurlLib Name 'curl_easy_setopt';

{** Sets a string-typed option (URL, proxy, user agent, POST body, ...). }
function curl_easy_setopt(handle: CURL; option: CURLoption; Value: pchar): CURLcode;
cdecl; overload; external CurlLib Name 'curl_easy_setopt';

{** Sets a pointer-typed option (callbacks, userdata, header lists). }
function curl_easy_setopt(handle: CURL; option: CURLoption; Value: Pointer): CURLcode;
cdecl; overload; external CurlLib Name 'curl_easy_setopt';

{** Performs the transfer configured on @param(handle); blocks until done. }
function curl_easy_perform(handle: CURL): CURLcode;
cdecl; external CurlLib;

{**
  Retrieves post-transfer info. @param(Value) must point at storage matching
  the selector: @code(pclong) for @code(CURLINFO_LONG) selectors, @code(ppchar)
  for @code(CURLINFO_STRING) selectors (string storage is owned by libcurl).
}
function curl_easy_getinfo(handle: CURL; info: CURLINFO; Value: Pointer): CURLcode;
cdecl; external CurlLib;

{** Returns a static, human-readable description of @param(code). }
function curl_easy_strerror(code: CURLcode): pchar;
cdecl; external CurlLib;

{** Frees the session handle; the handle is invalid afterwards. }
procedure curl_easy_cleanup(handle: CURL);
cdecl; external CurlLib;

{**
  Appends a copy of @param(str) to string list @param(list) (@nil starts a
  new list). Returns the new list head, or @nil on failure — on failure the
  original list is left intact, so keep the old head until checked.
}
function curl_slist_append(list: Pcurl_slist; const str: pchar): Pcurl_slist;
cdecl; external CurlLib;

{** Frees an entire string list created by @link(curl_slist_append). }
procedure curl_slist_free_all(list: Pcurl_slist);
cdecl; external CurlLib;

implementation

end.
