unit frmUsagesList;

interface

uses
  Dialogs, nppp_forms, StdCtrls, Classes, Controls, Vcl.ComCtrls,
  JediUtils;

type
  TFormUsagesList = class(TNppDockingForm)
    lvList: TListView;
    procedure lvListData(Sender: TObject; Item: TListItem);
  private
    FList: TArray<TJediDefinition>;
    procedure lvUsagesListDblClick(Sender: TObject);
    procedure UpdateTexts;
  public
    procedure SetUsagesList(list: TArray<TJediDefinition>);
  end;

implementation
uses
  System.SysUtils, nppp_baseplugin, PyJediPlugin, System.StrUtils;

{$R *.dfm}

function GetFileLine(const fn: string; line: integer): string;
var
  fs: TFileStream;
  sr: TStreamReader;
begin
  fs := TFileStream.Create(fn, fmOpenRead);
  sr := TStreamReader.Create(fs);
  try
    repeat
      sr.ReadLine;
      Dec(line);
    until line = 0;
    Result := sr.ReadLine;
  finally
    sr.Free;
    fs.Free;
  end;
end;

procedure TFormUsagesList.lvListData(Sender: TObject; Item: TListItem);
const
  cPositionFmt = '%d,%d';
begin
  with FList[Item.Index] do
  begin
    Item.Caption := Trim(text);
    Item.SubItems.Add(Format(cPositionFmt, [line + 1, column]));
    Item.SubItems.Add(typeStr);
    Item.SubItems.Add(full_name);
    Item.SubItems.Add(module_path);
  end;
end;

procedure TFormUsagesList.lvUsagesListDblClick(Sender: TObject);
begin
  if lvList.ItemIndex < 0 then
    Exit;
  with FList[lvList.ItemIndex] do
    (FNPPPlugin as TPyJediPlugin).GoToPos(module_path, line, column);
end;

procedure TFormUsagesList.SetUsagesList(list: TArray<TJediDefinition>);
resourcestring
  sFormCaption = 'PyJedy usages of "%s"';
begin
  FList := list;
  UpdateTexts;
  Self.Caption := Format(sFormCaption, [FNPPPlugin.NppBaseFuncs.getCurrentWord]);
  lvList.OnDblClick := lvUsagesListDblClick;
  lvList.Items.Count := Length(FList);
  lvList.Repaint;
end;

procedure TFormUsagesList.UpdateTexts;
var
  i: integer;
begin
  for i := 0 to Length(FList) - 1 do
    with FList[i] do
      if FileExists(module_path)
        and not SameText(module_path, FNPPPlugin.NppBaseFuncs.getFullCurrentPath) then
        text := GetFileLine(module_path, line)
      else
      begin
        text := FNPPPlugin.ScintillaFuncs.getTextRange(
          FNPPPlugin.ScintillaFuncs.getLineStartPosition(line),
          FNPPPlugin.ScintillaFuncs.getLineEndPosition(line)
        );
      end;
end;

end.
