unit dexcom_trend_tests;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry,
  trndi.types, trndi.api.dexcomNew;

type
  TDexcomTrendMappingTests = class(TTestCase)
  published
    procedure MapsNumericZeroBased;
    procedure MapsNumericOneBased;
    procedure MapsTextualStandard;
    procedure MapsTextualPyDexcomNames;
    procedure UnknownMapsToPlaceholder;
  end;

implementation

procedure TDexcomTrendMappingTests.MapsNumericZeroBased;
begin
  AssertEquals(Ord(TdDoubleUp), Ord(MapDexcomTrendToEnum('0')));
  AssertEquals(Ord(TdFlat), Ord(MapDexcomTrendToEnum('3')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('7')));
end;

procedure TDexcomTrendMappingTests.MapsNumericOneBased;
begin
  AssertEquals(Ord(TdSingleUp), Ord(MapDexcomTrendToEnum('1')));
  AssertEquals(Ord(TdFortyFiveDown), Ord(MapDexcomTrendToEnum('4')));
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('8')));
end;

procedure TDexcomTrendMappingTests.MapsTextualStandard;
begin
  AssertEquals(Ord(TdFlat), Ord(MapDexcomTrendToEnum('Flat')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('NOT COMPUTABLE')));
end;

procedure TDexcomTrendMappingTests.MapsTextualPyDexcomNames;
begin
  AssertEquals(Ord(TdDoubleUp), Ord(MapDexcomTrendToEnum('DoubleUp')));
  AssertEquals(Ord(TdSingleUp), Ord(MapDexcomTrendToEnum('SingleUp')));
  AssertEquals(Ord(TdFortyFiveUp), Ord(MapDexcomTrendToEnum('FortyFiveUp')));
  AssertEquals(Ord(TdFortyFiveDown), Ord(MapDexcomTrendToEnum('FortyFiveDown')));
  AssertEquals(Ord(TdDoubleDown), Ord(MapDexcomTrendToEnum('DoubleDown')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('NotComputable')));
  AssertEquals(Ord(TdNotComputable), Ord(MapDexcomTrendToEnum('RateOutOfRange')));
end;

procedure TDexcomTrendMappingTests.UnknownMapsToPlaceholder;
begin
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('Banana')));
  AssertEquals(Ord(TdPlaceholder), Ord(MapDexcomTrendToEnum('9')));
end;

initialization
  RegisterTest(TDexcomTrendMappingTests);

end.
