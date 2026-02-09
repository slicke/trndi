unit LCLType;

{$mode objfpc}{$H+}

interface

// Minimal stub of LCLType to satisfy headless builds.
// Add symbols as needed; keep tiny to avoid colliding with real LCL.

type
  TShiftStateEnum = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle);
  TShiftState = set of TShiftStateEnum;

  TMouseButton = (mbLeft, mbRight, mbMiddle);

// Virtual key codes (only a few used by the project)
const
  VK_S = 83; // 'S'
  VK_C = 67; // 'C'
  VK_X = 88; // 'X'

implementation

end.