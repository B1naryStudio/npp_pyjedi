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

library pyjedi;

{$R *.dres}

uses
  Windows,
  PyJediPlugin in 'PyJediPlugin.pas',
  nppp_dllfuncs,
  frmUsagesList in 'frmUsagesList.pas' {FormUsagesList},
  PluginCIntf in 'PluginCIntf.pas',
  IconsTable in 'IconsTable.pas',
  JediUtils in 'JediUtils.pas',
  frmAbout in 'frmAbout.pas' {FormAbout},
  frmOptions in 'frmOptions.pas' {FormOptions},
  PyJediOptions in 'PyJediOptions.pas';

{$R *.res}

exports
  setInfo, getName, getFuncsArray, beNotified, messageProc;
{$IFDEF NPPUNICODE}
exports
  isUnicode;
{$ENDIF}

begin
  DllProc := @DLLEntryPoint;
  DLLEntryPoint(DLL_PROCESS_ATTACH);
end.

