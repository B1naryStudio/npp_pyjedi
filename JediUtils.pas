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
unit JediUtils;

interface
type
  TJediDefinition = record
    //Name of variable/function/class/module
    name: string;
    //The type of the definition.
    typeStr: string;
    module_name: string;
    //Return a document string for this completion object.
    docstring: string;
    //A textual description of the object.
    description: string;
    //Dot-separated path of this object.
    full_name: string;
    line, column: Integer;
    builtin: Boolean;
    module_path: string;
    //in Completion only
    name_with_symbols: string;
    //it is not from Jedi, using in usages list
    text: string;
  end;
  PJediDefinition = ^TJediDefinition;

  TPJCallSignature = record
    params: TArray<string>;
    paramIndex: Integer;
  end;

function JediMakeScript(const source, source_path: string;
  Aline: integer = -1; Acolumn: Integer = -1): Variant;

function JediGetAutocompleteList(script: Variant): TArray<TJediDefinition>;
function JediGetDefinitionsList(script: Variant; tryLiter: boolean = true): TArray<TJediDefinition>;
function JediGetUsagesList(script: Variant): TArray<TJediDefinition>;
function JediGetCallArgsList(script: Variant): TArray<TPJCallSignature>;

implementation
uses
  System.SysUtils, VarPyth, System.Variants, PythonEngine;

var __jedi: Variant;
function Jedi: Variant;
begin
  if __jedi = Unassigned then
    __jedi := Import('jedi');
  Result := __jedi;
end;

function JediMakeScript(const source, source_path: string;
  Aline: integer = -1; Acolumn: Integer = -1): Variant;
var
  s: Variant;
begin
  Result := None;
  if source = '' then
    Exit;
    (*script := jedi.Interpreter(source, VarPythonEval('[locals()]'),
    line := line + 1, column := column);*)
  s := source;
  try
    if (Aline < 0) or (Acolumn < 0) then
      Result := Jedi.Script(s, source_path := source_path)
    else
      Result := Jedi.Script(s, Aline + 1, Acolumn, source_path := source_path);
  except

  end;
end;

procedure ConvertDefinition(pyDef: Variant; nativeDef: PJediDefinition);
begin
  with nativeDef^ do
  begin
    name := pyDef.name;
    typeStr := pyDef.type;
    module_name := pyDef.module_name;
    docstring := pyDef.docstring();
    description := pyDef.description;
    full_name := pyDef.full_name;
    if not VarIsNone(pyDef.line) then
      line := pyDef.line - 1;
    if not VarIsNone(pyDef.column) then
      column := pyDef.column;
    builtin := pyDef.in_builtin_module;
    module_path := pyDef.module_path;
  end;
end;

function JediGetAutocompleteList(script: Variant): TArray<TJediDefinition>;
var
  completions, c: Variant;
  i: integer;
begin
  if VarIsNone(script) then
    Exit;
  completions := script.completions();
  SetLength(Result, integer(completions.Length));
  for i := 0 to Length(Result) - 1 do
  begin
    c := completions.GetItem(i);
    ConvertDefinition(c, @Result[i]);
    Result[i].name_with_symbols := c.name_with_symbols;
  end;
end;

function JediGetDefinitionsList(script: Variant; tryLiter: boolean = true): TArray<TJediDefinition>;
var
  definitions, d: Variant;
  i: Integer;
  flagGoDeeper: boolean;
begin
  if VarIsNone(script) then
    Exit;
  flagGoDeeper := true;
  if tryLiter then
  begin
    definitions := script.goto_assignments();
    SetLength(Result, integer(definitions.Length));
    for i := 0 to Length(Result) - 1 do
    begin
      d := definitions.GetItem(i);
      ConvertDefinition(d, @Result[i]);
      if Result[i].typeStr <> 'import' then
        flagGoDeeper := False;
    end;
  end;
  if flagGoDeeper then
  begin
    try
      definitions := script.goto_definitions();
    except
      Exit;
    end;
    SetLength(Result, integer(definitions.Length));
    for i := 0 to Length(Result) - 1 do
    begin
      d := definitions.GetItem(i);
      ConvertDefinition(d, @Result[i]);
    end;
  end;
end;

function JediGetUsagesList(script: Variant): TArray<TJediDefinition>;
var
  usages, u: Variant;
  i: Integer;
begin
  if VarIsNone(script) then
    Exit;
  try
    usages := script.usages();
  except
    on E: EPyException do
      if (E.EName = 'NotFoundError') then
        Exit;
  end;
  SetLength(Result, integer(usages.Length));
  for i := 0 to Length(Result) - 1 do
  begin
    u := usages.GetItem(i);
    ConvertDefinition(u, @Result[i]);
  end;
end;

function JediGetCallArgsList(script: Variant): TArray<TPJCallSignature>;
var
  call_signs, c, params: Variant;
  i, j: Integer;
begin
  if VarIsNone(script) then
    Exit;
  call_signs := script.call_signatures();
  if call_signs.Length = 0 then
    Exit;
  SetLength(Result, integer(call_signs.Length));
  for i := 0 to Length(Result) - 1 do
  begin
    c := call_signs.GetItem(i);
    if VarIsNone(c.index) then
      Result[i].paramIndex := -1
    else
      Result[i].paramIndex := c.index;
    params := c.params;
    SetLength(Result[i].params, integer(params.Length));
    for j := 0 to Length(Result[i].params) - 1 do
      Result[i].params[j] := Trim(params.GetItem(j).get_code());
  end;
end;

initialization
  __jedi := Unassigned;

end.
