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

unit nppp_forms;

interface

uses
  Forms, Controls, Messages, Classes,
  nppp_baseplugin, nppp_types, nppp_consts;

type
  TNppForm = class(TForm)
  private
    { Private declarations }
  protected
    procedure RegisterForm();
    procedure UnregisterForm();
    procedure DoClose(var Action: TCloseAction); override;
  public
    { Public declarations }
    Npp: TNppBasePlugin;
    DefaultCloseAction: TCloseAction;
    constructor Create(NppParent: TNppBasePlugin); reintroduce; overload;
    constructor Create(AOwner: TNppForm); reintroduce; overload;
    destructor Destroy; override;
    function WantChildKey(Child: TControl; var Message: TMessage): Boolean; override;
  end;

  TNppDockingForm = class(TNppForm)
  private
    { Private declarations }
    FDlgId: Integer;
    FOnDock: TNotifyEvent;
    FOnFloat: TNotifyEvent;
    procedure RemoveControlParent(control: TControl);
  protected
    { Protected declarations }
    ToolbarData: TToolbarData;
    NppDefaultDockingMask: Cardinal;
    // @todo: change caption and stuff....
    procedure OnWM_NOTIFY(var msg: TWMNotify); message WM_NOTIFY;
    property OnDock: TNotifyEvent read FOnDock write FOnDock;
    property OnFloat: TNotifyEvent read FOnFloat write FOnFloat;
  public
    { Public declarations }
    CmdId: Integer;
    constructor Create(NppParent: TNppBasePlugin); overload;
    constructor Create(AOwner: TNppForm); overload;
    constructor Create(NppParent: TNppBasePlugin; DlgId: Integer); overload; virtual;
    constructor Create(AOwner: TNppForm; DlgId: Integer); overload;  virtual;
    procedure Show;
    procedure Hide;
    procedure RegisterDockingForm(MaskStyle: Cardinal = DWS_DF_CONT_LEFT);
    procedure UpdateDisplayInfo; overload;
    procedure UpdateDisplayInfo(Info: String); overload;
    property DlgID: Integer read FDlgid;
  published
    { Published declarations }
  end;

implementation

uses
  Windows, SysUtils;

{ TNppForm }

constructor TNppForm.Create(NppParent: TNppBasePlugin);
begin
  self.Npp := NppParent;
  self.DefaultCloseAction := caNone;
  inherited Create(nil);
  // We figure right now this does more damage than good.
  // So let the main transalte and dispatch do it's thing instead of isdialogmessage
  self.RegisterForm();
end;

constructor TNppForm.Create(AOwner: TNppForm);
begin
  self.Npp := AOwner.Npp;
  self.DefaultCloseAction := caNone;
  inherited Create(Aowner);
  //self.RegisterForm();
end;

destructor TNppForm.Destroy;
begin
  if (self.HandleAllocated) then
  begin
    self.UnregisterForm();
  end;
  inherited;
end;

procedure TNppForm.RegisterForm();
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_MODELESSDIALOG, MODELESSDIALOGADD, self.Handle);
end;

procedure TNppForm.UnregisterForm();
begin
  if (not self.HandleAllocated) then exit;
  SendMessage(self.Npp.NppData.NppHandle, NPPM_MODELESSDIALOG, MODELESSDIALOGREMOVE, self.Handle);
end;

procedure TNppForm.DoClose(var Action: TCloseAction);
begin
  if (self.DefaultCloseAction <> caNone) then Action := self.DefaultCloseAction;
  inherited;
end;

// This is going to help us solve the problems we are having because of N++ handling our messages
function TNppForm.WantChildKey(Child: TControl; var Message: TMessage): Boolean;
begin
  Result := Child.Perform(CN_BASE + Message.Msg, Message.WParam, Message.LParam) <> 0;
end;

{ TNppDockingForm }

// I don't know how else to hide a constructor.
constructor TNppDockingForm.Create(NppParent: TNppBasePlugin);
begin
  MessageBox(0, 'Do not use this constructor', 'Plugin Framework error', MB_OK);
  Halt(1);
end;
constructor TNppDockingForm.Create(AOwner: TNppForm);
begin
  MessageBox(0, 'Do not use this constructor', 'Plugin Framework error', MB_OK);
  Halt(1);
end;

constructor TNppDockingForm.Create(NppParent: TNppBasePlugin; DlgId: Integer);
begin
  inherited Create(NppParent);
  self.FDlgId := DlgId;
  self.CmdId := self.Npp.CmdIdFromDlgId(DlgId);
  self.RegisterDockingForm(self.NppDefaultDockingMask);
  self.RemoveControlParent(self);
end;

constructor TNppDockingForm.Create(AOwner: TNppForm; DlgId: Integer);
begin
  inherited Create(AOwner);
  self.FDlgId := DlgId;
  self.RegisterDockingForm(self.NppDefaultDockingMask);
  self.RemoveControlParent(self);
end;

procedure TNppDockingForm.OnWM_NOTIFY(var msg: TWMNotify);
begin
  if (self.Npp.NppData.NppHandle <> msg.NMHdr.hwndFrom) then
  begin
    inherited;
    exit;
  end;
  msg.Result := 0;

  if (msg.NMHdr.code = DMN_CLOSE) then
  begin
    self.DoHide;
  end;
  if ((msg.NMHdr.code and $ffff) = DMN_FLOAT) then
  begin
    // msg.NMHdr.code shr 16 - container
    if Assigned(FOnFloat) then FOnFloat(Self);
  end;
  if ((msg.NMHdr.code and $ffff) = DMN_DOCK) then
  begin
    // msg.NMHdr.code shr 16 - container
    if Assigned(FOnDock) then FOnDock(Self);
  end;
 inherited;
end;

procedure TNppDockingForm.RegisterDockingForm(MaskStyle: Cardinal = DWS_DF_CONT_LEFT);
begin
  self.HandleNeeded;
  //self.Visible := true;

  FillChar(self.ToolbarData,sizeof(TToolbarData),0);

  if (not self.Icon.Empty) then
  begin
    self.ToolbarData.IconTab := self.Icon.Handle;
    self.ToolbarData.Mask := self.ToolbarData.Mask or DWS_ICONTAB;
  end;

  self.ToolbarData.ClientHandle := self.Handle;

  self.ToolbarData.DlgId := self.FDlgId;
  self.ToolbarData.Mask := MaskStyle;

  self.ToolbarData.Mask := self.ToolbarData.Mask or DWS_ADDINFO;

  GetMem(self.ToolbarData.Title, 500*sizeof(nppPChar));
  GetMem(self.ToolbarData.ModuleName, 1000*sizeof(nppPChar));
  GetMem(self.ToolbarData.AdditionalInfo, 1000*sizeof(nppPChar));

{$IFDEF NPPUNICODE}
  StringToWideChar(self.Caption, self.ToolbarData.Title, 500);
  GetModuleFileNameW(HInstance, self.ToolbarData.ModuleName, 1000);
  StringToWideChar(ExtractFileName(self.ToolbarData.ModuleName), self.ToolbarData.ModuleName, 1000);
  StringToWideChar('', self.ToolbarData.AdditionalInfo, 1);
  SendMessageW(self.Npp.NppData.NppHandle, NPPM_DMMREGASDCKDLG, 0, Integer(@self.ToolbarData));
{$ELSE}
  StrCopy(self.ToolbarData.Title, PChar(self.Caption));
  GetModuleFileNameA(HInstance, self.ToolbarData.ModuleName, 1000);
  StrLCopy(self.ToolbarData.ModuleName, PChar(ExtractFileName(self.ToolbarData.ModuleName)), 1000);
  StrCopy(self.ToolbarData.AdditionalInfo, PChar(''));
  SendMessageA(self.Npp.NppData.NppHandle, NPPM_DMMREGASDCKDLG, 0, Integer(@self.ToolbarData));
{$ENDIF}

  self.Visible := true;
end;

procedure TNppDockingForm.Show;
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_DMMSHOW, 0, LPARAM(self.Handle));
  inherited;
  self.DoShow;
end;

procedure TNppDockingForm.Hide;
begin
  SendMessage(self.Npp.NppData.NppHandle, NPPM_DMMHIDE, 0, LPARAM(self.Handle));
  self.DoHide;
end;

// This hack prevents the Win Dialog default procedure from an endless loop while
// looking for the prevoius component, while in a floating state.
// I still don't know why the pointer climbs up to the docking dialog that holds this one
// but this works for now.
procedure TNppDockingForm.RemoveControlParent(control: TControl);
var
  wincontrol: TWinControl;
  i, r: integer;
begin
  if (control is TWinControl) then
  begin
    wincontrol := control as TWinControl;
    wincontrol.HandleNeeded;
    r := Windows.GetWindowLong(wincontrol.Handle, GWL_EXSTYLE);
    if (r and WS_EX_CONTROLPARENT = WS_EX_CONTROLPARENT) then
    begin
      Windows.SetWindowLong(wincontrol.Handle, GWL_EXSTYLE, r and not WS_EX_CONTROLPARENT);
    end;
  end;
  for i:=control.ComponentCount-1 downto 0 do
  begin
    if (control.Components[i] is TControl) then self.RemoveControlParent(control.Components[i] as TControl);
  end;
end;

procedure TNppDockingForm.UpdateDisplayInfo;
begin
  self.UpdateDisplayInfo('');
end;

procedure TNppDockingForm.UpdateDisplayInfo(Info: String);
begin
{$IFDEF NPPUNICODE}
  StringToWideChar(Info, self.ToolbarData.AdditionalInfo, 1000);
  SendMessageW(self.Npp.NppData.NppHandle, NPPM_DMMUPDATEDISPINFO, 0, self.Handle);
{$ELSE}
  StrLCopy(self.ToolbarData.AdditionalInfo, PChar(Info), 1000);
  SendMessageA(self.Npp.NppData.NppHandle, NPPM_DMMUPDATEDISPINFO, 0, self.Handle);
{$ENDIF}
end;

end.
