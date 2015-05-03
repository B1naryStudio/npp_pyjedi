unit PyJediOptions;

interface

type
  TPJOptions = record
    DropRestOfWord: Boolean;
    ImagesSize: integer;
    CallTipAutoTimeout: Integer;
    procedure LoadFromFile(const fn: string);
    procedure SaveToFile(const fn: string);
  end;

const
  __defaultOptions: TPJOptions = (
    DropRestOfWord: true;
    ImagesSize: 16;
    CallTipAutoTimeout: 500;
  );

var
  PJOptions: TPJOptions;

implementation
uses
  superobject, System.Classes, System.SysUtils;

{ TPJOptions }

procedure TPJOptions.LoadFromFile(const fn: string);
var
  json: ISuperObject;
  ctx: TSuperRttiContext;
begin
  if not FileExists(fn) then
    Exit;
  with TStringStream.Create do
  try
    LoadFromFile(fn);
    json := SO(DataString);
  finally
    Free;
  end;
  ctx := TSuperRttiContext.Create;
  try
    Self := ctx.AsType<TPJOptions>(json);
  finally
    ctx.Free;
  end;
end;

procedure TPJOptions.SaveToFile(const fn: string);
var
  json: ISuperObject;
  ctx: TSuperRttiContext;
begin
  ForceDirectories(ExtractFilePath(fn));
  ctx := TSuperRttiContext.Create;
  try
    json := ctx.AsJson<TPJOptions>(Self);
  finally
    ctx.Free;
  end;
  with TStringStream.Create(json.AsJSon(true)) do
  try
    SaveToFile(fn);
  finally
    Free;
  end;
end;

initialization
  PJOptions := __defaultOptions;

end.
