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
 * - 2026-08-21: New unit. The identical main-thread marshalling bridge that
 *   trndi.native.linux, .mac, .win and .bsd each declared locally (TWakeBridge
 *   / TBSDWakeBridge) now lives here once. Platform units keep their own
 *   instances and teardown; only the class is shared. Not part of
 *   trndi.native.base because this depends on the LCL (Forms.Application),
 *   which base deliberately does not.
 *)

{**
  @abstract(Main-thread marshalling bridge for native event callbacks.)

  A tiny object that hands a native-event callback (wake-from-sleep,
  badge-click, ...) to the main thread via
  @code(Application.QueueAsyncCall), with @code(Pending) coalescing bursts —
  a second event arriving before the first fire has run is dropped rather
  than stacking another callback.

  Owners must remove queued calls before freeing an instance
  (@code(Application.RemoveAsyncCalls(bridge))) — a queued Fire must not
  outlive the bridge.
}
unit trndi.native.wakebridge;

{$mode objfpc}{$H+}

interface

uses
Classes, Forms, trndi.native.base;

type
  {** See the unit abstract. Fields are public by design — owners assign
      @code(Callback) directly, mirroring the original per-unit bridges. }
TTrndiWakeBridge = class
public
  Callback: TTrndiWakeCallback;
  Pending: boolean;
    {** Runs on the main thread; re-arms and invokes the callback. }
  procedure Fire(Data: PtrInt);
    {** Queue a Fire on the main thread unless one is already pending. }
  procedure Queue;
end;

implementation

procedure TTrndiWakeBridge.Fire(Data: PtrInt);
begin
  Pending := false;
  if Assigned(Callback) then
    try
      Callback();
    except
      // Never let a callback exception unwind into the message loop
    end;
end;

procedure TTrndiWakeBridge.Queue;
begin
  // Coalesce: re-arm only after the previous async fire completes.
  if not Pending then
  begin
    Pending := true;
    Application.QueueAsyncCall(@Fire, 0);
  end;
end;

end.
