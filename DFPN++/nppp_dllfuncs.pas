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

unit nppp_dllfuncs;

interface

uses
  Types, Windows, Messages,
  nppp_types;

procedure DLLEntryPoint(dwReason: DWord);
procedure setInfo(NppData: TNppData); cdecl; export;
function getName: nppPchar; cdecl; export;
function getFuncsArray(var nFuncs:integer):Pointer;cdecl; export;
procedure beNotified(sn: PSCNotification); cdecl; export;
function messageProc(msg: Integer; _wParam: WPARAM; _lParam: LPARAM): LRESULT; cdecl; export;
{$IFDEF NPPUNICODE}
function isUnicode : Boolean; cdecl; export;
{$ENDIF}

implementation

uses
  nppp_baseplugin;

//{$Include 'lib\NppPluginInclude.pas'}
procedure DLLEntryPoint(dwReason: DWord);
begin
  case dwReason of
    DLL_PROCESS_ATTACH: begin
      // create the main object
      //Npp := TDbgpNppPlugin.Create;
    end;
    DLL_PROCESS_DETACH: begin
      if (Assigned(FNPPPlugin)) then
        FNPPPlugin.Destroy;
    end;
  end;
end;

procedure setInfo(NppData: TNppData); cdecl; export;
begin
  FNPPPlugin.SetInfo(NppData);
end;

function getName: nppPchar; cdecl; export;
begin
  Result := FNPPPlugin.GetName;
end;

function getFuncsArray(var nFuncs:integer):Pointer;cdecl; export;
begin
  Result := FNPPPlugin.GetFuncsArray(nFuncs);
end;

procedure beNotified(sn: PSCNotification); cdecl; export;
begin
  FNPPPlugin.BeNotified(sn);
end;

function messageProc(msg: Integer; _wParam: WPARAM; _lParam: LPARAM): LRESULT; cdecl; export;
var xmsg:TMessage;
begin
  xmsg.Msg := msg;
  xmsg.WParam := _wParam;
  xmsg.LParam := _lParam;
  xmsg.Result := 0;
  FNPPPlugin.MessageProc(xmsg);
  Result := xmsg.Result;
end;

{$IFDEF NPPUNICODE}
function isUnicode : Boolean; cdecl; export;
begin
  Result := true;
end;
{$ENDIF}

end.
