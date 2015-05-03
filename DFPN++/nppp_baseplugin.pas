{
    Delphi Foundation for creating plugins for Notepad++
    (Short: DFPN++)

    Copyright (C) 2009 Bastian Blumentritt

    This file is part of DFPN++.

    DFPN++ is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    DFPN++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with DFPN++.  If not, see <http://www.gnu.org/licenses/>.
}

unit nppp_baseplugin;

interface

uses
  Windows,Messages,SysUtils,
  Dialogs,Classes,Forms,
  nppp_types, nppp_consts, nppp_basefuncs, nppp_menufuncs, nppp_scifuncs;

type
  TNppBasePlugin = class(TObject)
  private
    FuncArray: array of _TFuncItem;
    FBaseFuncs: TNPPBaseFunctions;
    FMenuFuncs: TNPPMenuFunctions;
    FScintillaFuncs: TNPPSciFunctions;
    FDuringStartup: Boolean;
  protected
    PluginName: nppString;
    function GetPluginsConfigDir: String;
    function AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD):
      Integer; overload;
    function AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD;
      ShortcutKey: TShortcutKey): Integer; overload;

    // hooks
    procedure DoNppnToolbarModification; virtual;
    procedure DoNppnShutdown; virtual;
    procedure DoNppnStartedUp; virtual;
    procedure DoSciCharAdded(ch: Integer); virtual;
    procedure DoNppnLanguageChanged; virtual;
    procedure DoNppnFileOpened; virtual;
    procedure DoNppnBufferActivated; virtual;
    procedure DoSciDwellStart(position, x, y: integer); virtual;
    procedure DoSciDwellEnd; virtual;
    procedure DoSciUpdateUI(updated: TSciUpdateUIFlag); virtual;

    property duringStartup: Boolean read FDuringStartup;
  public
    NppData: TNppData;
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function CmdIdFromDlgId(DlgId: Integer): Integer;
    procedure beginUpdate;
    procedure endUpdate;

    // needed for DLL export
    procedure SetInfo(NppData: TNppData); virtual;
    function GetName: nppPChar;
    function GetFuncsArray(var FuncsCount: Integer): Pointer;
    procedure BeNotified(sn: PSCNotification);
    procedure MessageProc(var Msg: TMessage); virtual;

    // functions usable by plugins are in these classes
    property NppBaseFuncs: TNPPBaseFunctions read FBaseFuncs;
    property NppMenuFuncs: TNPPMenuFunctions read FMenuFuncs;
    property ScintillaFuncs: TNPPSciFunctions read FScintillaFuncs;
  end;

var
  FNPPPlugin: TNppBasePlugin;

implementation

uses
  ShlObj, ActiveX, Winapi.KnownFolders;

{ TNppPlugin }

procedure TNppBasePlugin.BeforeDestruction;
begin
{ This is hacking for trouble...
  We need to unset the Application handler so that the forms
  don't get berserk and start throwing OS error 1004.
  This happens because the main NPP HWND is already lost when the
  DLL_PROCESS_DETACH gets called, and the form tries to allocate a new
  handler for sending the "close" windows message...
}
  Application.Handle := 0;
  Application.Terminate;
  inherited;
end;

procedure TNppBasePlugin.beginUpdate;
begin
  FScintillaFuncs.beginUpdate;
end;

constructor TNppBasePlugin.Create;
begin
  inherited;
  FDuringStartup := True;
  FBaseFuncs := TNPPBaseFunctions.Create;
  FMenuFuncs := TNPPMenuFunctions.Create;
  FScintillaFuncs := TNPPSciFunctions.Create;
end;

destructor TNppBasePlugin.Destroy;
var
  i: Integer;
begin
  FScintillaFuncs.Free;
  FMenuFuncs.Free;
  FBaseFuncs.Free;

  for i:=0 to Length(self.FuncArray)-1 do
  begin
    if (self.FuncArray[i].ShortcutKey <> nil) then
    begin
      Dispose(self.FuncArray[i].ShortcutKey);
    end;
  end;
  inherited;
end;

function TNppBasePlugin.AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD): Integer;
var
  i: Integer;
begin
  i := Length(self.FuncArray);
  SetLength(Self.FuncArray,i+1);
{$IFDEF NPPUNICODE}
  StringToWideChar(Name, self.FuncArray[i].ItemName, MAXFUNCNAMELENGTH);
{$ELSE}
  StrCopy(self.FuncArray[i].ItemName, PChar(Name));
{$ENDIF}
  self.FuncArray[i].Func := Func;
  self.FuncArray[i].ShortcutKey := nil;
  Result := i;
end;

function TNppBasePlugin.AddFuncItem(Name: nppString; Func: PFUNCPLUGINCMD;
  ShortcutKey: TShortcutKey): Integer;
var
  i: Integer;
begin
  i := self.AddFuncItem(Name, Func);
  New(self.FuncArray[i].ShortcutKey);
  self.FuncArray[i].ShortcutKey.IsCtrl := ShortcutKey.IsCtrl;
  self.FuncArray[i].ShortcutKey.IsAlt := ShortcutKey.IsAlt;
  self.FuncArray[i].ShortcutKey.IsShift := ShortcutKey.IsShift;
  self.FuncArray[i].ShortcutKey.Key := ShortcutKey.Key; // need widechar ??
  Result := i;
end;

function TNppBasePlugin.GetFuncsArray(var FuncsCount: Integer): Pointer;
begin
  FuncsCount := Length(self.FuncArray);
  Result := self.FuncArray;
end;

function TNppBasePlugin.GetName: nppPChar;
begin
  Result := nppPChar(self.PluginName);
end;

function TNppBasePlugin.GetPluginsConfigDir: String;
var
  PFolder: PChar;
begin
  SHGetKnownFolderPath(FOLDERID_RoamingAppData, 0, 0, PFolder);
  Result := PFolder;
  CoTaskMemFree(PFolder);
  Result := Result + '\Notepad++\plugins\config\';
  ForceDirectories(Result);
end;

procedure TNppBasePlugin.BeNotified(sn: PSCNotification);
begin
  if HWND(sn^.nmhdr.hwndFrom) = self.NppData.NppHandle then
    case sn^.nmhdr.code of
      NPPN_TBMODIFICATION:
        self.DoNppnToolbarModification;
      NPPN_SHUTDOWN:
        self.DoNppnShutdown;
      NPPN_READY:
      begin
        FDuringStartup := False;
        self.DoNppnStartedUp;
      end;
      NPPN_LANGCHANGED:
        self.DoNppnLanguageChanged;
      NPPN_FILEOPENED:
        self.DoNppnFileOpened;
      NPPN_BUFFERACTIVATED:
        Self.DoNppnBufferActivated;
    end
  else
  if HWND(sn^.nmhdr.hwndFrom) = Self.NppData.ScintillaMainHandle then
    with sn^ do
    begin
      case nmhdr.code of
        SCN_CHARADDED:
          Self.DoSciCharAdded(ch);
        SCN_DWELLSTART:
          Self.DoSciDwellStart(position, x, y);
        SCN_DWELLEND:
          self.DoSciDwellEnd;
        SCN_UPDATEUI:
          self.DoSciUpdateUI(TSciUpdateUIFlag(updated));
      end;
    end;
end;

procedure TNppBasePlugin.MessageProc(var Msg: TMessage);
var
  hm: HMENU;
  i: integer;
begin
  if (Msg.Msg = WM_CREATE) then
  begin
    // Change '-' to separator items
    hm := GetMenu(self.NppData.NppHandle);
    for i:=0 to Length(self.FuncArray)-1 do
      if (self.FuncArray[i].ItemName[0] = '-') then
        ModifyMenu(hm, self.FuncArray[i].CmdID, MF_BYCOMMAND or MF_SEPARATOR, 0, nil);
  end;
  Dispatch(Msg);
end;

procedure TNppBasePlugin.SetInfo(NppData: TNppData);
begin
  self.NppData := NppData;
  Application.Handle := NppData.NppHandle;

  FBaseFuncs.setNPPHandle(NppData.NppHandle);
  FMenuFuncs.setNPPHandle(NppData.NppHandle);
  FScintillaFuncs.setNPPHandle(NppData.NppHandle);
  FScintillaFuncs.setSci1Handle(NppData.ScintillaMainHandle);
  FScintillaFuncs.setSci2Handle(NppData.ScintillaSecondHandle);
end;

function TNppBasePlugin.CmdIdFromDlgId(DlgId: Integer): Integer;
begin
  Result := self.FuncArray[DlgId].CmdId;
end;

procedure TNppBasePlugin.endUpdate;
begin
  FScintillaFuncs.endUpdate;
end;

// function templates

// called right before Notepad++ is about to be shutdowned.
procedure TNppBasePlugin.DoNppnBufferActivated;
begin

end;

procedure TNppBasePlugin.DoNppnFileOpened;
begin

end;

procedure TNppBasePlugin.DoNppnLanguageChanged;
begin

end;

procedure TNppBasePlugin.DoNppnShutdown;
begin
  // override this
end;

// called after all the procedures of launchment of notepad++ are done.
procedure TNppBasePlugin.DoNppnStartedUp;
begin
  // override this
end;

// called when toolbar icons can be registered.
procedure TNppBasePlugin.DoNppnToolbarModification;
begin
  // override this
end;

//called when user types
procedure TNppBasePlugin.DoSciCharAdded(ch: Integer);
begin

end;

procedure TNppBasePlugin.DoSciDwellEnd;
begin

end;

procedure TNppBasePlugin.DoSciDwellStart;
begin

end;

procedure TNppBasePlugin.DoSciUpdateUI(updated: TSciUpdateUIFlag);
begin

end;

end.
