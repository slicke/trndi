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
const
API_NS = 'NightScout';
API_NS3 = 'NightScout v3';
API_DEX_USA = 'Dexcom (USA)';
API_DEX_EU = 'Dexcom (Outside USA)';
API_DEX_NEW_USA = 'Dexcom New (USA)';
API_DEX_NEW_EU = 'Dexcom New (Outside USA)';
API_DEX_NEW_JP = 'Dexcom New (Japan)';
API_TANDEM_USA = 'Tandem t:connect (USA)';
API_TANDEM_EU = 'Tandem t:connect (EU)';
API_CARELINK_US = 'CareLink Follower (USA)';
API_CARELINK_EU = 'CareLink Follower (EU/Other)';
API_XDRIP = 'xDrip';

{$ifdef DEBUG}
API_D_DEBUG =  '* Debug Backend *';
API_D_MISSING = '* Debug Missing Backend *';
API_D_PERFECT = '* Debug Perfect Backend *';
API_D_CUSTOM = '* Debug Custom Backend *';
API_D_EDGE = '* Debug Edge Backend *';
API_D_FIRST = '* Debug First Reading Missing *';
API_D_SECOND = '* Debug Second Reading Missing *';
API_D_FIRSTX = '* Debug First X Readings Missing *';
API_D_INTERMITTENT = '* Debug Intermittent Missing *';
API_D_SLOW = '* Debug Slow Backend *';
API_D_LOWSOON = '* Debug Low Soon Backend *';
API_D_SENSOR_EXPIRY = '* Debug Sensor Expiry Backend *';
API_D_FAULTY = '* Debug Faulty Sensor Backend *';
API_D_LATE_MISSING = '* Debug Late Missing Backend *';

API_DEBUG: array of string = (API_D_DEBUG, API_D_MISSING, API_D_PERFECT, API_D_CUSTOM, API_D_EDGE, API_D_FIRST, API_D_SECOND, API_D_FIRSTX, API_D_INTERMITTENT, API_D_SLOW, API_D_LOWSOON, API_D_SENSOR_EXPIRY, API_D_FAULTY, API_D_LATE_MISSING);
{$endif}

MILLIS_PER_MINUTE = 60000; // Milliseconds in a minute
