unit trndi.defines.pp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

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

API_DEBUG: array of string = (API_D_DEBUG, API_D_MISSING, API_D_PERFECT, API_D_CUSTOM, API_D_EDGE, API_D_FIRST, API_D_SECOND, API_D_FIRSTX, API_D_INTERMITTENT);
{$endif}

MILLIS_PER_MINUTE = 60000; // Milliseconds in a minute

implementation

end.
