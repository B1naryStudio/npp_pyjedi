unit frmAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormAbout = class(TForm)
    btnOk: TButton;
    mmoAbout: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

resourcestring
  sAboutStr = 'PyJedy Autocomplete plugin v%s'#13#10 +
    'Created by Lebedinskiy Artyom'#13#10 +
    '';

implementation

{$R *.dfm}

procedure TFormAbout.FormCreate(Sender: TObject);
var
  Size: DWORD;
  FixedVI: PVSFixedFileInfo;
  VersionWords: array[0..3] of Word;
  ver: string;
  Buf: PChar;
  Len: Cardinal;
begin
  Size := GetFileVersionInfoSize(PChar(Application.ExeName), Size);
  Buf := AllocMem(Size);
  try
    GetFileVersionInfo(PChar(Application.ExeName), 0, Size, Buf);
    VerQueryValue(Buf, '\', pointer(FixedVI), Len);
    Move(FixedVI.dwFileVersionMS, VersionWords, 8);
    ver := Format('%d.%d.%d.%d',
      [VersionWords[1], VersionWords[0], VersionWords[3], VersionWords[2]]);
  finally
    FreeMem(Buf, Size);
  end;
  mmoAbout.Text := Format(sAboutStr, [ver]);
end;

end.
