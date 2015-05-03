unit PluginCIntf;

interface

procedure _CFuncAutocomplete; cdecl;
procedure _CFuncShowOptions; cdecl;
procedure _CFuncAbout; cdecl;
procedure _CFuncGoToDefinition; cdecl;
procedure _CFuncUsages; cdecl;
procedure _CFuncCallArguments; cdecl;

implementation
uses
  nppp_baseplugin, PyJediPlugin;

procedure _CFuncAutocomplete; cdecl;
begin
  (FNPPPlugin as TPyJediPlugin).Autocomplete();
end;

procedure _CFuncShowOptions; cdecl;
begin
  (FNPPPlugin as TPyJediPlugin).ShowOptions;
end;

procedure _CFuncUsages; cdecl;
begin
  (FNPPPlugin as TPyJediPlugin).ShowUsages;
end;

procedure _CFuncAbout; cdecl;
begin
  (FNPPPlugin as TPyJediPlugin).About;
end;

procedure _CFuncGoToDefinition; cdecl;
begin
  (FNPPPlugin as TPyJediPlugin).GoToDefinition;
end;

procedure _CFuncCallArguments; cdecl;
begin
  (FNPPPlugin as TPyJediPlugin).ShowCallArgs;
end;

end.
