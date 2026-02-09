unit trndi_native_mock_test;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, trndi.native, SysUtils;

type
  TTrndiNativeMockTest = class(TTestCase)
  published
    procedure TestSettings;
    procedure TestHTTPExample;
    procedure TestProxyURLInvalid;
  end;

implementation

procedure TTrndiNativeMockTest.TestSettings;
var
  n: TTrndiNativeBase;
begin
  n := TrndiNative.Create;
  try
    n.SetSetting('unittest.testkey', 'unittest-value');
    AssertEquals('unittest-value', n.GetSetting('unittest.testkey', ''));
    n.DeleteSetting('unittest.testkey');
    AssertEquals('', n.GetSetting('unittest.testkey', ''));
  finally
    n.Free;
  end;
end;

procedure TTrndiNativeMockTest.TestHTTPExample;
var
  res: string;
  ok: boolean;
begin
  // Allow skipping network tests by setting TRNDI_OFFLINE_TESTS=1 in environment
  if GetEnvironmentVariable('TRNDI_OFFLINE_TESTS') = '1' then
    Exit;

  ok := TrndiNative.getURL('http://example.com', res);
  AssertTrue(ok);
  AssertTrue(Pos('Example Domain', res) > 0);
end;

procedure TTrndiNativeMockTest.TestProxyURLInvalid;
var
  res: string;
  ok: boolean;
begin
  ok := TrndiNative.TestProxyURL('http://example.com', '127.0.0.1:9', '9', '', '', res);
  // Expect false when proxy is not reachable
  AssertFalse(ok);
end;

initialization
  RegisterTest(TTrndiNativeMockTest);

end.
