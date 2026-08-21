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
 *
 * MODIFICATION NOTICE (GPLv3 Section 5):
 * - 2026-08-21: New unit. SafeThreadJoin and SafeThreadRelease moved here
 *   verbatim from trndi.native.base, so the base contract unit stays free of
 *   platform conditionals (these helpers carry the layer's only unavoidable
 *   Haiku {$IFDEF}s - they are free functions, so no virtual method can host
 *   the divergence).
 *)

{**
  @abstract(Cross-platform worker-thread teardown helpers.)

  Free functions shared by the native layer and its consumers for joining and
  releasing worker threads. They exist because FPC 3.2.2's TThread cannot be
  joined or freed safely on Haiku (see @link(SafeThreadJoin)); every other
  platform gets the plain WaitFor/Free path.
}
unit trndi.native.threading;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils;

{** Join a finished (or finishing) worker thread safely across platforms.

    On Haiku this must NOT use TThread.WaitFor: FPC 3.2.2's cthreads calls
    pthread_detach(pthread_self()) in CEndThread as every thread exits, and
    Haiku's libroot frees the pthread struct once a detached thread has
    exited — a subsequent pthread_join (which WaitFor does) then reads freed
    memory and GPFs inside wait_for_thread_etc. glibc tolerates the same
    join-after-detach, which is why this never crashed on Linux. Instead we
    poll TThread.Finished, which the thread epilogue sets after DoTerminate;
    past that point the thread no longer touches the object, and Free is
    safe (cthreads' CCloseThread is a no-op). When called on the main thread
    we pump CheckSynchronize so a worker blocked in Synchronize can drain. }
procedure SafeThreadJoin(T: TThread);

{** Release a worker thread the caller has already run to completion: joins it
    and frees it where that is safe, and detaches it where it is not. Callers
    set their own reference to nil afterwards - the thread's concrete class
    makes a @code(var) parameter impossible here. }
procedure SafeThreadRelease(T: TThread);

implementation

procedure SafeThreadJoin(T: TThread);
begin
{$IFDEF HAIKU}
  while not T.Finished do
    if GetCurrentThreadID = MainThreadID then
      CheckSynchronize(1)
    else
      Sleep(1);
{$ELSE}
  T.WaitFor;
{$ENDIF}
end;

procedure SafeThreadRelease(T: TThread);
begin
  if not Assigned(T) then
    Exit;
{$IFDEF HAIKU}
  // SafeThreadJoin above dodges TThread.WaitFor, but Free cannot: TThread
  // .Destroy calls WaitFor itself for any thread it has not reaped, so it walks
  // straight back into the same join-after-detach access violation. The object
  // therefore cannot be freed from here at all - hand it to the RTL instead.
  // A thread still running is freed by ThreadFunc when it exits; one that has
  // already finished leaks, because FreeOnTerminate is read only on that exit
  // path. Callers run this at shutdown, so it leaks once per worker at most,
  // and that beats taking the process down with it.
  T.FreeOnTerminate := true;
{$ELSE}
  T.WaitFor;
  T.Free;
{$ENDIF}
end;

end.
