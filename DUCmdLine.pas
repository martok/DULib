{-----------------------------------------------------------------------------
 Unit Name: DUCmdLine
 Author:    Sebastian Huetter
 Date:      2009-01-22
 Purpose:   Provides some utilities for basic line applications.

 History:   2009-01-22 initial release

------------------------------------------------------------------------------
 Copyright Notice: If you copy or modify this file, this block
   _must_ be preserved! If you are using this code in your projects,
   I would like to see a word of thanks in the About-Box or a similar place.
-----------------------------------------------------------------------------}

unit DUCmdLine;

interface

uses DUStrings;

type
  TParamDescr = record
    LongName, ShortName: string;
    Long: string;
    Short: char;
  end;
  TParamSwitchState = (ssNone, ssOn, ssOff);

(*
Expected formats:
  Values
     -v Text
     --value=Text
  Switches
     -s+ -s- -s
     --switch=on --switch=off --switch
     on,1,yes,true  OR  off,0,no,false
  OptionSets(do we have x?)
     -o:abc
     --option:abc
*)

//function _PD(LongName: string; ShortName: string = ''): TParamDescr; overload;
function _PD(Long: string; Short: char = #0): TParamDescr; overload;
function _PD(Short: char = #0): TParamDescr; overload;
function ParamPresent(Param: TParamDescr): boolean;
function ParamValue(Param: TParamDescr): string;
function ParamGetOptSet(Param: TParamDescr): string;
function ParamOptSet(Param: TParamDescr; Option: char): boolean;
function ParamSwitch(Param: TParamDescr): TParamSwitchState;

var
  ParamSwitchOn: char = '+';
  ParamSwitchOff: char = '-';
  ParamSwitchOnLong: TStringArray;
  ParamSwitchOffLong: TStringArray;
  ParamOptDelim: char = ':';
  ParamValueDelim: char = '=';
  ParamShortIntro: char = '-';
  ParamLongIntro: string = '--';
  ParamLongNegate: string = 'no';

function QualifiedFileName(const FN: string): string;
function RequoteParams(First, Last: integer; Quote: char = '"'): string;

type
  TSwitchStyle = (ssPrefix, ssSuffix);
  TParamError = (perrParameterExpectedNext, perrUnexpectedEnd);
  TParamErrorHandler = procedure (errCode: TParamError; Ref1,Ref2: string);
  TParamParser = class
  private
    FErrorHandler: TParamErrorHandler;
    FSwitchStyle: TSwitchStyle;
  protected
    argv: TStringArray;
    argc: integer;
    LastOpt: integer;
    procedure InvokeError(errCode: TParamError; Ref1,Ref2: string);
    function IsShortParam(S:string): boolean;
    function IsLongParam(S:string): boolean;
    function LongParamMatch(P,M:string): boolean;
  public
    constructor Create;
    property ErrorHandler: TParamErrorHandler read FErrorHandler write FErrorHandler;
    procedure GetAllValues(PD: TParamDescr; var Values: TStringArray);
    function GetLastValue(PD: TParamDescr): string;
    function GetFirstValue(PD: TParamDescr): string;
    function GetSwitchState(PD: TParamDescr; Default: boolean): boolean;
    property SwitchStyle : TSwitchStyle read FSwitchStyle write FSwitchStyle;
    function ParamExists(PD: TParamDescr): boolean;
    function Count: integer;
    function AsString(const Prm: integer): string;
  end;

var
  Params: TParamParser;

function ErrorCodeToString(Code: TParamError): string;
procedure DefaultHandlerConsole(errCode: TParamError; Ref1,Ref2: string);


implementation

uses SysUtils;

var
  ParamCache: TStringArray;

(*function _PD(LongName: string; ShortName: string = ''): TParamDescr;
begin
  Result.LongName:= LongName;
  Result.ShortName:= ShortName;
end;  *)

function _PD(Long: string; Short: char = #0): TParamDescr; overload;
begin
  Result.Long:= Long;
  Result.Short:= Short;
end;

function _PD(Short: char = #0): TParamDescr; overload;
begin
  Result.Long:= '';
  Result.Short:= Short;
end;



function FindParamD(var Param: TParamDescr; var FoundLong: boolean; StartWhere: integer = 0): integer;
  function IsKey(P, M: string; short: boolean): boolean;
  begin
    Result:= (P = M) or
      (StartsWith(P, M) and (
      (short and (length(P) = length(M) + 1) and (P[length(P)] in [ParamSwitchOn, ParamSwitchOff])) or
      (P[length(M) + 1] in [ParamOptDelim, ParamValueDelim])
      ));
  end;

var
  i: integer;
begin
  for i:= StartWhere to high(ParamCache) do begin
    if (Param.LongName > '') and
      IsKey(ParamCache[i], Param.LongName, false) then begin
      FoundLong:= true;
      Result:= i;
      exit;
    end;
    if (Param.ShortName > '') and
      IsKey(ParamCache[i], Param.ShortName, true) then begin
      FoundLong:= false;
      Result:= i;
      exit;
    end;
  end;
  Result:= -1;
end;

function ParamPresent(Param: TParamDescr): boolean;
var
  dummy: Boolean;
begin
  Result:= FindParamD(Param, dummy) <> -1;
end;

function ParamValue(Param: TParamDescr): string;
var
  k: integer;
  ll: boolean;
begin
  k:= FindParamD(Param, ll);
  Result:= '';
  if k < 0 then
    exit;
  if ll then
    Result:= CopyFrom(ParamCache[k], ParamValueDelim)
  else if k < high(ParamCache) then
    Result:= ParamCache[k + 1];
end;

function ParamGetOptSet(Param: TParamDescr): string;
var
  k: integer;
  ll: boolean;
begin
  k:= FindParamD(Param, ll);
  Result:= '';
  if k < 0 then
    exit;
  Result:= CopyFrom(ParamCache[k], ParamOptDelim);
end;

function ParamOptSet(Param: TParamDescr; Option: char): boolean;
var
  s: string;
begin
  s:= ParamGetOptSet(Param);
  Result:= Pos(Option, s) > 0;
end;

function ParamSwitch(Param: TParamDescr): TParamSwitchState;
var
  k: integer;
  ll: boolean;
  s: string;
begin
  Result:= ssNone;
  k:= FindParamD(Param, ll);
  while k >= 0 do begin
    if ll then begin
      s:= CopyFrom(ParamCache[k], ParamValueDelim);
      if StrAFind(ParamSwitchOnLong, s) >= 0 then
        Result:= ssOn
      else if StrAFind(ParamSwitchOffLong, s) >= 0 then
        Result:= ssOff;
    end
    else begin
      s:= copy(ParamCache[k], length(Param.ShortName) + 1, MaxInt);

      if s = ParamSwitchOn then
        Result:= ssOn
      else if s = ParamSwitchOff then
        Result:= ssOff;
    end;

    k:= FindParamD(Param, ll, k + 1);
  end;
end;

procedure FillParamCache;
var
  i: integer;
begin
  for i:= 1 to ParamCount do
    StrAAdd(ParamCache, ParamStr(i));
end;

function QualifiedFileName(const FN: string): string;
begin
  if (Pos(':', FN) = 2) or              //local drive
    (Pos('\\', FN) = 1) then            //UNC name
    Result:= FN
  else
    Result:= ExpandFileName(FN);
end;

function RequoteParams(First, Last: integer; Quote: char = '"'): string;
var
  i: integer;
  p: string;
begin
  Result:= '';
  for i:= First to Last do begin
    p:= ParamStr(i);
    if i > First then
      Result:= Result + ' ';
    if pos(' ', p) > 0 then
      Result:= Result + Quote + p + Quote
    else
      Result:= Result + p;
  end;
end;


function ErrorCodeToString(Code: TParamError): string;
begin
  case Code of
    perrParameterExpectedNext: Result:= 'Expected value for "%0:s", but found "%1:s"';
    perrUnexpectedEnd: Result:= 'Unexpected end after Parameter %0:s';
  else
    Result:= '';
  end;
end;

procedure DefaultHandlerConsole(errCode: TParamError; Ref1,Ref2: string);
begin
  Writeln('Parameter Error: ',Format(ErrorCodeToString(errCode),[Ref1, Ref2]));
end;

{ TParamParser }

constructor TParamParser.Create;
var i: integer;
    p: pchar;
    a: string;
  function next(ptr: pchar):char;
  begin
    Result:= pchar(ptr+1)^;
  end;
begin
  inherited;
  FSwitchStyle:= ssSuffix;
  argc:= 0;
  StrAClear(argv);
  p:= CmdLine;
  while p^<>#0 do begin
    a:= '';
    if p^='''' then begin
      inc(p);
      repeat
        a:= a + p^;
        inc(p);
      until (p^=#0) or ((p^='''') and (next(p) in [' ',#0]));

      if p^<>#0 then
        inc(p);
    end else
    if p^='"' then begin
      inc(p);
      repeat
        a:= a + p^;
        inc(p);
      until (p^=#0) or ((p^='"') and (next(p) in [' ',#0]));

      if p^<>#0 then
        inc(p);
    end else begin
      while (p^<>#0) and (p^<>' ') do begin
        a:= a + p^;
        inc(p);
      end;
    end;
    while (p^<>#0) and (p^=' ') do
      inc(p);
    StrAAdd(argv, a);
  end;

  argc:= length(argv);
  LastOpt:= argc+1;
  for i:= 0 to argc-1 do 
    if argv[i]='--' then
      LastOpt:= i;
end;

function TParamParser.IsLongParam(S: string): boolean;
begin
  Result:= StartsWith(S,ParamLongIntro);
end;

function TParamParser.IsShortParam(S: string): boolean;
begin
  Result:= not IsLongParam(S) and StartsWith(S,ParamShortIntro);
end;

function TParamParser.LongParamMatch(P, M: string): boolean;
begin
  Result:= (CopyTo(P, ParamValueDelim, length(ParamLongIntro)+1)=M) and (M>'');
end;

procedure TParamParser.GetAllValues(PD: TParamDescr; var Values: TStringArray);
var i:integer;
    p:string;
begin
  StrAClear(Values);
  i:= 0;
  while i<argc do begin
    p:= argv[i];
    if length(p)>=2 then begin
      if (i<LastOpt) and
        (PD.Short>#0) and
        IsShortParam(p[1]) and (p[length(p)]=PD.Short) then begin
          if i+1<argc then
            if argv[i+1][1]<>ParamShortIntro then begin
              StrAAdd(Values, argv[i+1]);
              inc(i);
            end else
              InvokeError(perrParameterExpectedNext, PD.Short, argv[i+1])
          else
            InvokeError(perrUnexpectedEnd, IntToStr(i+1), '');
      end else
      if IsLongParam(p) and LongParamMatch(P,PD.Long) then begin
        StrAAdd(Values, CopyFrom(p,ParamValueDelim));
      end;
    end;
    inc(i);
  end;
end;

function TParamParser.GetFirstValue(PD: TParamDescr): string;
var B:TStringArray;
begin
  GetAllValues(PD, B);
  if length(B)>0 then
    Result:= B[0]
  else
    Result:= '';
end;

function TParamParser.GetLastValue(PD: TParamDescr): string;
var B:TStringArray;
begin
  GetAllValues(PD, B);
  if length(B)>0 then
    Result:= B[high(B)]
  else
    Result:= '';
end;

procedure TParamParser.InvokeError(errCode: TParamError; Ref1,
  Ref2: string);
begin
  if Assigned(FErrorHandler) then
    FErrorHandler(errCode, Ref1, Ref2);
end;

function TParamParser.GetSwitchState(PD: TParamDescr;
  Default: boolean): boolean;
var i,k:integer;
    p,s:string;
begin
  Result:= default;
  if FSwitchStyle = ssSuffix then begin
    for i:= argc-1 downto 0 do begin
      p:= argv[i];
      if length(p)<2 then continue;
      if (PD.Short>#0) and (i<LastOpt) and
         IsShortParam(P) then begin
        k:= Pos(PD.Short, p);
        if (k>0) then begin
          if k = length(p) then
            p:= p+' ';
          case p[k+1] of
            '+': result:= true;
            '-': result:= false;
            else result:= not Default;
          end;
          exit;
        end;
      end;
      if IsLongParam(p) and LongParamMatch(p,PD.Long) then begin
        s:= CopyFrom(p,ParamValueDelim);
        if StrAFind(ParamSwitchOnLong, s) >= 0 then
          Result:= true
        else if StrAFind(ParamSwitchOffLong, s) >= 0 then
          Result:= false;
      end;
    end;
  end else begin
    for i:= argc-1 downto 0 do begin
      p:= argv[i];
      if (PD.Short>#0) and (i<LastOpt) then begin
        k:= 0;
        if p=ParamSwitchOn+PD.Short then Result:= true else
        if p=ParamSwitchOff+PD.Short then Result:= false else
          k:= 1;
        if k=0 then exit;
      end;
      if (PD.Long>'') and IsLongParam(p) then begin
        k:= 0;
        if p=ParamLongIntro+PD.Long then Result:= true else
        if p=ParamLongIntro+ParamLongNegate+PD.Long then Result:= false else
          k:= 1;
        if k=0 then exit;
      end;
    end;
  end
end;

function TParamParser.ParamExists(PD: TParamDescr): boolean;
var i,k:integer;
    p:string;
begin
  Result:= false;
  for i:= argc-1 downto 0 do begin
    p:= argv[i];
    if length(p)<2 then continue;
    if (i<LastOpt) and
       IsShortParam(P) then begin
      k:= Pos(PD.Short, p);
      if k>0 then begin
        Result:= true;
        exit;
      end;
    end else
    if IsLongParam(p) and LongParamMatch(p,PD.Long) then begin
      Result:= true;
      exit;
    end;
  end;
end;

function TParamParser.Count: integer;
begin
  Result:= argc;
end;

function TParamParser.AsString(const Prm: integer): string;
begin
  Result:= argv[Prm];
end;

initialization
  StrAClear(ParamCache);
  FillParamCache;

  ParamSwitchOnLong:= SplitString('1,on,yes,true', ',');
  ParamSwitchOffLong:= SplitString('0,off,no,false', ',');

  Params:= TParamParser.Create;
finalization
  Params.Free;
  StrAClear(ParamCache);
end.

