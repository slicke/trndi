unit uconf;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ExtCtrls,StdCtrls,SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TfConf = class(TForm)
    Bevel1:TBevel;
    lLicense:TButton;
    cbSys:TComboBox;
    Image1:TImage;
    Label1:TLabel;
    eAddr:TLabeledEdit;
    ePass:TLabeledEdit;
    lAck:TButton;
    lTitle:TLabel;
    lVer:TLabel;
    procedure lAckClick(Sender:TObject);
    procedure lLicenseClick(Sender:TObject);
  private

  public

  end;

var
  fConf: TfConf;

implementation

{$R *.lfm}

procedure TfConf.lAckClick(Sender:TObject);
begin
ShowMessage('Trndi makes use of the following 3rd party libraries:'#10+
            'macOS native code libraries by Phil Hess'#10+
            'WinStyles library by Espectr0'#10#10+
            'Extensions use JavaScript engine QuickJS by Fabrice Bellard and Charlie Gordo'#10+
            'integration of said engine is made possible with mORMot2 by Synopse Informatique - Arnaud Bouchez'#10#10+
            'Built in Object Pascal, using the Lazarus component library (LCL) and FreePascal.');
end;

procedure TfConf.lLicenseClick(Sender:TObject);
begin
     ShowMessage('Trndi - CGM viewer'#10'A re-imagination of TrayTrend by Björn Lindh'#10#10+
     'Copyright (C) 2017-2025 Björn Lindh'#10#10+

     'This program is free software: you can redistribute it and/or modify it'#10+
     'under the terms of the GNU General Public License version 3 as published'#10+
     'by the Free Software Foundation.'#10#10+

     'For more information, refer to the accompanying license file or visit:'#10+
     'https://www.gnu.org/licenses/gpl-3.0'#10#10+
     'Trndi is hobby project, pleae verify all data with an officially apprived'#10'medical app before acting on any shown data!');
end;

end.

