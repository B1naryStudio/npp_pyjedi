{
    PyJedi Autocomplete plugin for Notepad++

    Copyright (C) 2015 Lebedinskiy Artyom

    This plugin is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This Example Plugin is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License.
    If not, see <http://www.gnu.org/licenses/>.
}

unit PyJediPlugin;

interface

uses
  nppp_baseplugin, nppp_types, frmUsagesList, PythonEngine, JediUtils;

type
  TUsesParams = record
    AutoSortOrder: TSciAutoListSortOrder;
    DropRestOfWord: Boolean;
  end;

  TPyJediPlugin = class(TNppBasePlugin)
  private
    FFrmUsagesList: TFormUsagesList;
    FPyEngine: TPythonEngine;
    FActive: boolean;
    FUsesParams: TUsesParams;
    FUsagesDocIndex: Integer;
    FUsagesView: TNppView;
    const
      cAutoListSeparator: byte = Ord(' ');
      cSettingsFile = 'PyJedi\pyjedy.settings';
    procedure RegisterAutocompleteImages;
    procedure InitNPP;
    procedure LoadOldParams;
    procedure ApplySettings;
    function FilterParam(const param: string): Boolean;
    function FormatCallArgs(calls: TArray<TPJCallSignature>): string;
    function FormatAutocompleteList(const completions: TArray<TJediDefinition>): string;
    function GetFormUsagesList: TFormUsagesList;
    procedure ShowAutocompleteList(script: Variant; position: NativeInt);
    procedure HighlightCallTip(calls: TArray<TPJCallSignature>);
    procedure CheckLanguage;
  protected
    procedure DoSciCharAdded(ch: Integer); override;
    procedure DoNppnLanguageChanged; override;
    procedure DoNppnBufferActivated; override;
    procedure DoSciDwellStart(position, x, y: integer); override;
    procedure DoSciDwellEnd; override;
    procedure DoSciUpdateUI(updated: TSciUpdateUIFlag); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetInfo(NppData: TNppData); override;
    procedure GoToPos(const fn: string; line, column: NativeInt);
    //plugin functions
    procedure GoToDefinition;
    procedure ShowCallArgs;
    procedure ShowUsages;
    procedure ShowOptions;
    procedure About;
    procedure Autocomplete;
  end;

implementation

uses
  Windows, SysUtils, Classes,
  nppp_scifuncs, PluginCIntf, IconsTable,
  Vcl.Imaging.pngimage, frmAbout, Vcl.Forms, PyJediOptions, frmOptions;

{ TPyJediPlugin }

procedure TPyJediPlugin.DoNppnBufferActivated;
begin
  inherited;
  CheckLanguage;
end;

procedure TPyJediPlugin.DoNppnLanguageChanged;
begin
  inherited;
  CheckLanguage;
end;

procedure TPyJediPlugin.DoSciCharAdded(ch: Integer);
var
  script: Variant;
  source: string;
  params: TArray<TPJCallSignature>;
  p: Integer;
begin
  inherited;
  if not FActive then
    Exit;
  if ScintillaFuncs.autoIsListShowing then
    Exit;
  if ch in [13, 10] then
    Exit;

  p := ScintillaFuncs.getCurrentPosition;
  source := ScintillaFuncs.getText(p);
  script := JediMakeScript(source, NppBaseFuncs.getFullCurrentPath);
  params := JediGetCallArgsList(script);
  if Length(params) > 0 then
  begin
    if not ScintillaFuncs.callTipIsShowing then
      ScintillaFuncs.callTipShow(p, FormatCallArgs(params));
    HighlightCallTip(params);
  end
  else
    ShowAutocompleteList(script, p);
end;

procedure TPyJediPlugin.DoSciDwellEnd;
begin
  inherited;
  ScintillaFuncs.callTipCancel;
end;

procedure TPyJediPlugin.DoSciDwellStart(position, x, y: integer);
const
  cHintFmt = '%s: %s'#13#10'%s'#13#10#13#10'%s';
var
  hint, source: string;
  defs: TArray<TJediDefinition>;
  lineEnd, lineStart, line: NativeInt;
begin
  inherited;
  if position < 0 then
    Exit;
  line := ScintillaFuncs.getLineFromPosition(position);
  lineStart := ScintillaFuncs.getLineStartPosition(line);
  lineEnd := ScintillaFuncs.getLineEndPosition(line);
  source := ScintillaFuncs.getText(lineEnd);
  defs := JediGetDefinitionsList(JediMakeScript(source,
    NppBaseFuncs.getFullCurrentPath, line, position - lineStart), false);
  if Length(defs) = 0 then
    Exit;
  with defs[0] do
  begin
    hint := Format(cHintFmt, [full_name, typeStr, docstring, module_path]);
    ScintillaFuncs.callTipShow(position, hint);
    ScintillaFuncs.callTipHighLight(0, Pos(#13, hint));
  end;
end;

procedure TPyJediPlugin.DoSciUpdateUI(updated: TSciUpdateUIFlag);
var
  params: TArray<TPJCallSignature>;
begin
  inherited;
  if updated <> uuiSelection then
    Exit;
  if not ScintillaFuncs.callTipIsShowing then
    Exit;
  params := JediGetCallArgsList(
    JediMakeScript(
      ScintillaFuncs.getText(ScintillaFuncs.getCurrentPosition),
      NppBaseFuncs.getFullCurrentPath
    )
  );
  if Length(params) > 0 then
    HighlightCallTip(params)
  else
    ScintillaFuncs.callTipCancel;
end;

function TPyJediPlugin.FilterParam(const param: string): Boolean;
begin
  Result := (param = 'self') or (param = '...') or (Pos('*', param) > 0);
end;

function TPyJediPlugin.FormatAutocompleteList(const completions: TArray<TJediDefinition>): string;
var
  imgIndex: Integer;
  c: TJediDefinition;
  sb: TStringBuilder;
begin
  if Length(completions) = 0 then
    Exit;
  sb := TStringBuilder.Create;
  try
    for c in completions do
    begin
      imgIndex := SearchCompletionDef(c.typeStr);
      if imgIndex >= 0 then
        sb.Append(c.name + '?' + IntToStr(imgIndex))
      else
        sb.Append(c.name);
      sb.Append(Char(cAutoListSeparator));
    end;
    if sb.Length > 0 then
      sb.Remove(sb.Length - 1, 1);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TPyJediPlugin.FormatCallArgs(calls: TArray<TPJCallSignature>): string;
var
  i, j: integer;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    for i := 0 to Length(calls) - 1 do
    begin
      for j := 0 to Length(calls[i].params) - 1 do
        if not FilterParam(calls[i].params[j]) then
          sb.Append(calls[i].params[j] + ', ');
      if Length(calls) > 1 then
        sb.Append(Chr(i + 1));
    end;
    Result := sb.ToString(0, sb.Length - 2);
  finally
    sb.Free;
  end;
end;

procedure TPyJediPlugin.ShowAutocompleteList(script: Variant; position: NativeInt);
var
  completions: string;
  wordStart: NativeInt;
begin
  completions := FormatAutocompleteList(
    JediGetAutocompleteList(script)
  );
  if completions = '' then
    Exit;

  wordStart := ScintillaFuncs.getWordStartPosition(position, true);
  ScintillaFuncs.autoShowList(position - wordStart, completions);
end;

procedure TPyJediPlugin.ShowCallArgs;
var
  source: string;
  p: NativeInt;
  params: TArray<TPJCallSignature>;
  hint: string;
begin
  if not FActive then
    Exit;
  p := ScintillaFuncs.getCurrentPosition;
  source := ScintillaFuncs.getText(p);
  params := JediGetCallArgsList(JediMakeScript(source, NppBaseFuncs.getFullCurrentPath));
  if Length(params) = 0 then
    Exit;
  hint := FormatCallArgs(params);
  if hint = '' then
    Exit;
  ScintillaFuncs.callTipShow(p, hint);
  HighlightCallTip(params);
end;

procedure TPyJediPlugin.GoToDefinition;
var
  defs: TArray<TJediDefinition>;
  p, wordEnd: NativeInt;
  source: string;
begin
  if not FActive then
    Exit;
  p := ScintillaFuncs.getCurrentPosition;
  wordEnd := ScintillaFuncs.getWordEndPosition(p, true);
  source := ScintillaFuncs.getText(wordEnd);
  defs := JediGetDefinitionsList(JediMakeScript(source, NppBaseFuncs.getFullCurrentPath));
  if Length(defs) = 0 then
    Exit;
  with defs[Length(defs) - 1] do
    GoToPos(module_path, line - 1, column);
end;

procedure TPyJediPlugin.ShowUsages;
var
  usages: TArray<TJediDefinition>;
  source: string;
  line, column: NativeInt;
begin
  if not FActive then
    Exit;
  ScintillaFuncs.setCursor(scpWait);
  try
    FUsagesView := NppBaseFuncs.getCurrentView;
    FUsagesDocIndex := NppBaseFuncs.getCurrentDocIndex(FUsagesView);
    source := ScintillaFuncs.getText;
    line := NppBaseFuncs.getCurrentLine;;
    column := NppBaseFuncs.getCurrentColumn;
    usages := JediGetUsagesList(JediMakeScript(source,
      NppBaseFuncs.getFullCurrentPath, line, column));
    with GetFormUsagesList do
    begin
      SetUsagesList(usages);
      if not Showing then
        Show;
    end;
  finally
    ScintillaFuncs.setCursor(scpNormal);
  end;
end;

procedure TPyJediPlugin.CheckLanguage;
begin
  if (NppBaseFuncs.getCurrentLangType <> L_PYTHON) and FActive then
  begin
    FActive := false;
    LoadOldParams;
  end;
  if (NppBaseFuncs.getCurrentLangType = L_PYTHON) and not FActive and JediAvailable then
  begin
    FActive := true;
    InitNPP;
  end;
end;

constructor TPyJediPlugin.Create;
resourcestring
  sCompletions = 'Completions';
  sGoToDef = 'Go to definition';
  sUsages = 'Find usages';
  sCallArgs = 'Call arguments';
  sOptions = 'Options...';
  sAbout = 'About...';
begin
  inherited;
  self.PluginName := 'PyJedi Autocomplete';

  self.AddFuncItem(sCompletions, _CFuncAutocomplete,
    TShortcutKey.Create(False, False, true, VK_SPACE));

  self.AddFuncItem(sGoToDef, _CFuncGoToDefinition,
    TShortcutKey.Create(False, False, true, VK_F12));

  self.AddFuncItem(sUsages, _CFuncUsages,
    TShortcutKey.Create(True, False, true, VK_F12));

  self.AddFuncItem(sCallArgs, _CFuncCallArguments,
    TShortcutKey.Create(False, True, true, VK_SPACE));

  Self.AddFuncItem('-', nil);
  self.AddFuncItem(sOptions, _CFuncShowOptions);
  self.AddFuncItem(sAbout, _CFuncAbout);

  FPyEngine := TPythonEngine.Create(nil);
  FPyEngine.LoadDll;
end;

destructor TPyJediPlugin.Destroy;
begin
  PJOptions.SaveToFile(GetPluginsConfigDir + cSettingsFile);
  if Assigned(FFrmUsagesList) then
    FFrmUsagesList.Free;
  FPyEngine.Free;
  inherited;
end;

procedure TPyJediPlugin.SetInfo(NppData: TNppData);
begin
  inherited;
  PJOptions.LoadFromFile(GetPluginsConfigDir + cSettingsFile);
  FUsesParams.AutoSortOrder := ScintillaFuncs.autoGetSortOrder;
  FUsesParams.DropRestOfWord := ScintillaFuncs.autoGetDropRestOfWord;
  FActive := NppBaseFuncs.getCurrentLangType = L_PYTHON;
  if not FActive then
    Exit;
  InitNPP;
end;

procedure TPyJediPlugin.ApplySettings;
begin
  ScintillaFuncs.autoSetDropRestOfWord(PJOptions.DropRestOfWord);
  RegisterAutocompleteImages;
end;

procedure TPyJediPlugin.Autocomplete;
var
  source: string;
  p: NativeInt;
begin
  if not FActive then
    Exit;
  p := ScintillaFuncs.getCurrentPosition;
  source := ScintillaFuncs.getText(p);
  ShowAutocompleteList(JediMakeScript(source, NppBaseFuncs.getFullCurrentPath), p);
end;

procedure TPyJediPlugin.InitNPP;
begin
  ScintillaFuncs.autoSetListSeparator(cAutoListSeparator);
  ScintillaFuncs.autoSetSortOrder(asoCustom);
  ScintillaFuncs.callTipSetPosition(true);
  ScintillaFuncs.callTipSetHighLightFG(RGB(0, 0, 255));
  ScintillaFuncs.callTipSetAutoTimeout(PJOptions.CallTipAutoTimeout);
  ApplySettings;
end;

procedure TPyJediPlugin.LoadOldParams;
begin
  with FUsesParams do
  begin
    ScintillaFuncs.autoSetSortOrder(AutoSortOrder);
    ScintillaFuncs.autoSetDropRestOfWord(DropRestOfWord);
  end;
  ScintillaFuncs.autoClearImages;
end;

procedure TPyJediPlugin.GoToPos(const fn: string; line, column: NativeInt);
var
  gotoLineStart: NativeInt;
begin
  if FileExists(fn) then
    NppBaseFuncs.openFile(fn)
  else
    NppBaseFuncs.activateDoc(FUsagesView, FUsagesDocIndex);
  gotoLineStart := ScintillaFuncs.getLineStartPosition(line);
  ScintillaFuncs.goToPosition(gotoLineStart + column);
  ScintillaFuncs.ScintillaSetFocus;
end;

procedure TPyJediPlugin.HighlightCallTip(calls: TArray<TPJCallSignature>);
var
  i, startParam: integer;
begin
  if Length(calls) = 0 then
    Exit;
  if (calls[0].paramIndex < 0) or (calls[0].paramIndex > Length(calls[0].params)) then
    Exit;
  startParam := 0;
  for i := 0 to calls[0].paramIndex - 1 do
    if not FilterParam(calls[0].params[i]) then
      Inc(startParam, Length(calls[0].params[i]) + 2);

  ScintillaFuncs.callTipHighLight(startParam,
    startParam + Length(calls[0].params[calls[0].paramIndex]));
end;

procedure TPyJediPlugin.RegisterAutocompleteImages;
var
  i: Integer;
  png: TPngImage;
begin
  ScintillaFuncs.autoClearImages;
  png := TPngImage.Create();
  try
    for i := Low(cCompletionDefs) to High(cCompletionDefs) do
    begin
      png.LoadFromResourceName(HInstance, Format(cImgResFmt,
        [cCompletionDefs[i].iconResName, PJOptions.ImagesSize]));
      ScintillaFuncs.autoRegisterRGBAImage(png, i);
    end;
  finally
    png.Free;
  end;
end;

procedure TPyJediPlugin.ShowOptions;
begin
  if frmOptions.ShowOptions then
    ApplySettings;
end;

function TPyJediPlugin.GetFormUsagesList: TFormUsagesList;
begin
  if not Assigned(FFrmUsagesList) then
  begin
    FFrmUsagesList := TFormUsagesList.Create(self, 1);
    FFrmUsagesList.TileMode := TTileMode.tbHorizontal;
  end;
  Result := FFrmUsagesList;
end;

procedure TPyJediPlugin.About;
begin
  with TFormAbout.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

initialization
  FNPPPlugin := TPyJediPlugin.Create as TNppBasePlugin;

end.
