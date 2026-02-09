unit LCLType;

{$mode objfpc}{$H+}

interface

// Minimal stub of LCLType to satisfy headless builds.
// Add symbols as needed; keep tiny to avoid colliding with real LCL.

uses Controls;

type
  // Reuse the shift state definitions from Controls to keep types compatible
  TShiftStateEnum = Controls.TShiftStateEnum;
  TShiftState = Controls.TShiftState;

  TMouseButton = Controls.TMouseButton;

// Virtual key codes (only a few used by the project)
const
  VK_S = 83; // 'S'
  VK_C = 67; // 'C'
  VK_X = 88; // 'X'
  VK_ESCAPE = 27; // Escape key
  crHandPoint = 12; // cursor constant used by umain

implementation

end.