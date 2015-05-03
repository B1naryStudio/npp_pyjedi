unit frmOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormOptions = class(TForm)
    chkDropWord: TCheckBox;
    lblImgSIze: TLabel;
    cbbImagesSize: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblCallTipAutoTimeout: TLabel;
    edtHintTimeout: TEdit;
    btnDefault: TButton;
    procedure btnDefaultClick(Sender: TObject);
  private
    procedure LoadOptions;
    procedure CommitOptions;
    { Private declarations }
  public
    { Public declarations }
  end;

function ShowOptions: Boolean;

implementation
uses
  PyJediOptions;

const
  cDisabledHintTimeout = 10000000;

{$R *.dfm}

function ShowOptions: Boolean;
begin
  Result := false;
  with TFormOptions.Create(nil) do
  try
    LoadOptions;
    if ShowModal = mrOk then
    begin
      CommitOptions;
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TFormOptions.btnDefaultClick(Sender: TObject);
begin
  PJOptions := __defaultOptions;
  LoadOptions;
end;

procedure TFormOptions.CommitOptions;
begin
  PJOptions.DropRestOfWord := chkDropWord.Checked;
  if cbbImagesSize.ItemIndex >= 0 then
    PJOptions.ImagesSize := StrToIntDef(cbbImagesSize.Text, 16)
  else
    PJOptions.ImagesSize := 16;
  PJOptions.CallTipAutoTimeout := StrToIntDef(edtHintTimeout.Text, cDisabledHintTimeout);
  if PJOptions.CallTipAutoTimeout < 0 then
    PJOptions.CallTipAutoTimeout := cDisabledHintTimeout;
end;

procedure TFormOptions.LoadOptions;
begin
  chkDropWord.Checked := PJOptions.DropRestOfWord;
  cbbImagesSize.ItemIndex := cbbImagesSize.Items.IndexOf(
  IntToStr(PJOptions.ImagesSize));
  if PJOptions.CallTipAutoTimeout = cDisabledHintTimeout then
    edtHintTimeout.Text := '-1'
  else
    edtHintTimeout.Text := IntToStr(PJOptions.CallTipAutoTimeout);
end;

end.
