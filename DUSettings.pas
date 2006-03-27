unit DUSettings;

interface

uses Classes, IniFiles, SysUtils, DUStrings, DUNumbers;

type
  TGlobalSettings = class
  private
    FIniStyle: boolean;
    FFileName: String;
    FDelimiter: char;
    function GetBool(const Name: string): boolean;
    function GetFloat(const Name: string): Double;
    function GetInteger(const Name: string): int64;
    function GetString(const Name: string): string;
    procedure SetBool(const Name: string; const Value: boolean);
    procedure SetFloat(const Name: string; const Value: Double);
    procedure SetInteger(const Name: string; const Value: int64);
    procedure SetString(const Name, Value: string);

  protected
    OrigFile, Settings:TStringList;
    function GetRaw(Name:string):string;
    procedure SetRaw(Name,Value:String);
  public
    constructor Create;
    destructor Destroy; override;

    property FileName : String read FFileName write FFileName;
    property IniStyle : boolean read FIniStyle write FIniStyle;
    property Delimiter : char read FDelimiter write FDelimiter;
    procedure ClearSettings;
    procedure LoadSettings;
    procedure SaveSettings;

    property PrefString [const Name: string]: string  read GetString  write SetString;   default;
    property PrefInteger[const Name: string]: int64   read GetInteger write SetInteger;
    property PrefBoolean[const Name: string]: boolean read GetBool    write SetBool;
    property PrefFloat  [const Name: string]: Double  read GetFloat   write SetFloat;
  end;


var GlobalSettings:TGlobalSettings;

implementation

const CommentChars = ['#',';','/','!'];
      Quote        = '"';
      BoolStrs     :array[boolean]of string= ('false','true');
      DeciPoint    = '.';


{ TGlobalSettings }

constructor TGlobalSettings.Create;
begin
  inherited;
  Settings:= TStringList.Create;
  Settings.Sorted:= true;
  OrigFile:= TStringList.Create;
  FDelimiter:= '.';
  FIniStyle:= false;
end;

destructor TGlobalSettings.Destroy;
begin
  Settings.Free;
  OrigFile.Free;
  inherited;
end;

procedure TGlobalSettings.ClearSettings;
begin
  Settings.Clear;
end;

procedure TGlobalSettings.LoadSettings;
var i:integer;
    s,sect:String;
begin
  if not FileExists(FFileName) then exit;
  OrigFile.LoadFromFile(FFileName);
  if not FIniStyle then begin
    for i:= 0 to OrigFile.Count-1 do begin
      s:= OrigFile[i];
      if (s>'') and not(s[1] in CommentChars) and (pos('=',s)>0) then
        SetRaw(CopyTo(S,'='),CopyFrom(S,'='));
    end;
  end else begin
    sect:= '';
    for i:= 0 to OrigFile.Count-1 do begin
      s:= OrigFile[i];
      if (s>'') and not(s[1] in CommentChars) then begin
        if (S[1] = '[') and (S[Length(S)] = ']') then
          sect:= Copy(S, 2, Length(S) - 2)
        else
        if pos('=',s)>0 then SetRaw(Sect+FDelimiter+CopyTo(S,'='),CopyFrom(S,'='));
      end;
    end;
  end;
end;

procedure TGlobalSettings.SaveSettings;
var i:integer;
begin
  if not FIniStyle then begin
    if FileExists(FFileName) then
      OrigFile.LoadFromFile(FFileName);
    for i:= 0 to Settings.Count-1 do begin
      if OrigFile.IndexOfName(Settings.Names[i])>=0 then
        OrigFile.Values[Settings.Names[i]]:= Settings.Values[Settings.Names[i]]
      else
        OrigFile.Add(Settings[i]);
    end;
    OrigFile.SaveToFile(FFileName);
  end else begin
    with TIniFile.Create(FFileName) do try
      for i:= 0 to Settings.Count-1 do
        WriteString(CopyTo(Settings[i],FDelimiter),
                    CopySubst(Settings[i],FDelimiter,'='),
                    Settings.Values[Settings.Names[i]]);
    finally
      Free;
    end;
  end;
end;

function TGlobalSettings.GetBool(const Name: string): boolean;
var S:String;
begin
  S:= GetRaw(Name);
  Result:= (CompareText(S,'false')<>0) and (S>'') and (S<>'0');
end;

function TGlobalSettings.GetFloat(const Name: string): Double;
var S:String;
begin
  S:= GetRaw(Name);
  Result:= StringToFloat(S);
end;

function TGlobalSettings.GetInteger(const Name: string): int64;
begin
  Result:= StrToInt64(GetRaw(Name));
end;

function TGlobalSettings.GetRaw(Name: string): string;
begin
  Result:= Settings.Values[Name];
end;

function TGlobalSettings.GetString(const Name: string): string;
begin
  Result:= GetRaw(Name);
  Result:= ExtractQuotedString(Result,Quote);
end;

procedure TGlobalSettings.SetBool(const Name: string;
  const Value: boolean);
begin
  SetRaw(Name,BoolStrs[Value]);
end;

procedure TGlobalSettings.SetFloat(const Name: string;
  const Value: Double);
begin
  SetRaw(Name,FloatToString(Value,DeciPoint));
end;

procedure TGlobalSettings.SetInteger(const Name: string;
  const Value: int64);
begin
  SetRaw(Name,IntToStr(Value));
end;

procedure TGlobalSettings.SetRaw(Name, Value: String);
begin
  Settings.Sorted:= false;
  if Settings.IndexOfName(Name)>=0 then
    Settings.Values[Name]:= Value
  else Settings.Add(Name+'='+Value);
  Settings.Sorted:= true;
end;

procedure TGlobalSettings.SetString(const Name, Value: string);
begin
  SetRaw(Name,QuotedString(Value,Quote));
end;


initialization
  GlobalSettings:= TGlobalSettings.Create;
finalization
  GlobalSettings.Free;
  GlobalSettings:= nil;
end.