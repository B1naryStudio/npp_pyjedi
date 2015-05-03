unit IconsTable;

interface
uses
  System.SysUtils;

function SearchCompletionDef(const name: string): Integer;

type
  TPJCompletionDef = record
    typeName: string;
    iconResName: string
  end;

const
  cImgResFmt = '%s_%d';
  cCompletionDefs: array [0..5] of TPJCompletionDef = (
    (typeName: 'function'; iconResName: 'img_function'),
    (typeName: 'instance'; iconResName: 'img_instance'),
    (typeName: 'module'; iconResName: 'img_module'),
    (typeName: 'statement'; iconResName: 'img_statement'),
    (typeName: 'class'; iconResName: 'img_class'),
    (typeName: 'keyword'; iconResName: 'img_keyword')
  );

implementation

function SearchCompletionDef(const name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(cCompletionDefs) to High(cCompletionDefs) do
    if name = cCompletionDefs[i].typeName then
    begin
      Result := i;
      Exit;
    end;
end;

end.
