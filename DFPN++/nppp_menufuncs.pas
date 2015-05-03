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

unit nppp_menufuncs;

interface

uses
  Windows,
  nppp_types;

type
  TNPPMenuFunctions = class(TObject)
  private
    FNPPHandle: HWND;
  protected
  public
    constructor Create;
    procedure setNPPHandle(pHandle: HWND);
    procedure FileNew;
  end;

implementation

uses
  nppp_consts;

{ TNPPBaseFunctions }

constructor TNPPMenuFunctions.Create;
begin
  inherited;
  FNPPHandle := 0;
end;

procedure TNPPMenuFunctions.setNPPHandle(pHandle: HWND);
begin
  FNPPHandle := pHandle;
end;

procedure TNPPMenuFunctions.FileNew;
begin
  SendMessage(FNPPHandle, NPPM_MENUCOMMAND, 0, LPARAM(IDM_FILE_NEW));
end;

end.

