unit DUStartup;

interface

uses windows, Registry;

type TStartType = (stRunCU,stRunOnce,stRunLM);


function IsAutostart(Key:string; StartType:TStartType):boolean;
function ReadAutostart(Key:string; StartType:TStartType):string;

procedure WriteAutostart(Key:string; StartType:TStartType; FileName:String);
procedure DeleteAutostart(Key:string; StartType:TStartType);

implementation

function GetReg(S:TStartType):TRegistry;
begin
  Result:= TRegistry.Create;
    case s of
      stRunCU: begin
                 Result.RootKey:=HKEY_CURRENT_USER;
                 Result.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',false);
               end;
      stRunLM: begin
                 Result.RootKey:=HKEY_CURRENT_USER;
                 Result.OpenKey('\Software\Microsoft\Windows\CurrentVersion\RunOnce',false);
               end;
      stRunOnce: begin
                 Result.RootKey:=HKEY_LOCAL_MACHINE;
                 Result.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Run',false);
               end;
    end;
end;

function IsAutostart(Key:string; StartType:TStartType):boolean;
begin
  Result:= ReadAutostart(Key,StartType)>'';
end;

function ReadAutostart(Key:string; StartType:TStartType):string;
var r:TRegistry;
begin
  r:= GetReg(StartType);
  try
    try
      result:= r.ReadString(Key);
    except
      Result:= '';
    end;
  finally
    r.Free;
  end;
end;


procedure WriteAutostart(Key:string; StartType:TStartType; FileName:String);
var r:TRegistry;
begin
  r:= GetReg(StartType);
  try
    if FileName>'' then
      r.WriteString(Key,FileName)
    else
      r.DeleteValue(Key);
  finally
    r.Free;
  end;
end;

procedure DeleteAutostart(Key:string; StartType:TStartType);
begin
  WriteAutostart(Key,StartType,'');
end;


end.
