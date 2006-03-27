unit DUSystem;

interface

uses Windows,ShellAPI;


procedure RunControlPanelApplet(AppletFileName: string);

function FunctionDetect(LibName, FuncName: string; var LibPointer: Pointer): Boolean;

var BlockInput:function (Block: BOOL): BOOL; stdcall;


implementation

procedure RunControlPanelApplet(AppletFileName: string);
begin
 ShellExecute(GetDesktopWindow, 'open', PChar('rundll32.exe'),
   PChar('shell32.dll,Control_RunDLL '+AppletFileName), nil, SW_SHOWNORMAL);
end;

function FunctionDetect(LibName, FuncName: string; var LibPointer: Pointer): Boolean;
var
  LibHandle: THandle;
begin
  Result     := False;
  LibPointer := nil;
  if LoadLibrary(PChar(LibName)) = 0 then Exit;
  LibHandle := GetModuleHandle(PChar(LibName));
  if LibHandle <> 0 then
  begin
    LibPointer := GetProcAddress(LibHandle, PChar(FuncName));
    if LibPointer <> nil then Result := True;
  end;
end;

initialization
  FunctionDetect(user32, 'BlockInput', @BlockInput)
end.
 