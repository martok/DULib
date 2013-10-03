{-----------------------------------------------------------------------------
 Unit Name: DUDbgCon
 Author:    Sebastian Hütter
 Date:      2006-09-02
 Purpose:   Creates a console window for debug output

 History:   2006-09-02 initial release
 
------------------------------------------------------------------------------
 Copyright Notice: If you copy or modify this file, this block
   _must_ be preserved! If you are using this code in your projects,
   I would like to see a word of thanks in the About-Box or a similar place.
-----------------------------------------------------------------------------}
unit DUDbgCon;


interface

uses SysUtils, Windows, Classes, TypInfo;

function ShowConsole(Txt:string):boolean; overload;
function ShowConsole(Txt:integer):boolean;overload;
function ShowConsole(Txt:extended):boolean;overload;
function ShowConsole(Txt:int64):boolean;overload;
function ShowConsole(Txt:boolean):boolean;overload;
function ShowConsole(Txt: array of Const):boolean;overload;
function ShowConsole(Txt: TStrings):boolean;overload;
function ShowConsoleRTTI(Which:TComponent):boolean;

function TimeMeasure:int64;
function TimeDelta(First,Second:int64):double; // ms
function TimeToMSec(ATime:int64):double;
function TimeStop(First:int64):double;         // ms

const
  RTTIFormatstr:string = 'RTTI for ''%s'', type %s:';
  PropFormatStr:string = '  %s = %s';

implementation

function ShowConsole(Txt:string):boolean;
begin
  result := false;
  if System.IsConsole then
    begin
     WriteLn(txt);
     Flush(Output);
     result := true;
    end;
end;

function ShowConsole(Txt:integer):boolean;overload;
begin
  result := ShowConsole(inttostr(txt));
end;

function ShowConsole(Txt:extended):boolean;overload;
begin
  result := ShowConsole(floattostr(txt));
end;

function ShowConsole(Txt:int64):boolean;overload;
begin
  result := ShowConsole(inttostr(txt));
end;

function ShowConsole(Txt:boolean):boolean;overload;
 const a:array [boolean] of string = ('false','true');
begin
  result := ShowConsole( a[txt] );
end;

function ShowConsole(Txt: array of Const):boolean;overload;
var
  I: Integer;
  S: String;
begin
  S := '';
  for I := 0 to High(Txt) do 
    with Txt[I] do
      case VType of
        vtInteger:    S := S + IntToStr(VInteger);
        vtBoolean:    S := S + BoolToStr(VBoolean,true);
        vtChar:       S := S + VChar;
        vtExtended:   S := S + FloatToStr(VExtended^);

        vtString:     S := S + VString^;
        vtPChar:      S := S + VPChar;
        vtObject:     S := S + VObject.ClassName;
        vtClass:      S := S + VClass.ClassName;
        vtAnsiString: S := S + string(VAnsiString);
        vtCurrency:   S := S + CurrToStr(VCurrency^);
        vtVariant:    S := S + string(VVariant^);
        vtInt64:      S := S + IntToStr(VInt64^);
    end;
  Result:= ShowConsole(S);
end;

function ShowConsole(Txt: TStrings):boolean;overload;
var i:integer;
begin
  Result:= true;
  for i:= 0 to Txt.Count-1 do
    Result:= Result and ShowConsole(Txt[i]);
end;


function GetObjAsString(Instance: TObject): String;
begin
  if Instance=nil then Result:='nil'
  else if Instance is TComponent then
    Result:=TComponent(Instance).Name
  else Result:=Instance.ClassName;
end;

function GetPropValAsString(Instance: TComponent; PropInfo: PPropInfo): String;
begin
  case PropInfo.PropType^.Kind of
    tkInteger: Result:=IntToStr(GetOrdProp(Instance, PropInfo));
    tkChar, tkWChar:
      Result:=Char(GetOrdProp(Instance, PropInfo));
    tkEnumeration:
      Result:=GetEnumName(PropInfo^.PropType^, GetOrdProp(Instance, PropInfo));
    tkFloat:
      Result:=FloatToStr(GetFloatProp(Instance, PropInfo));
    tkString, tkLString, tkWString:
      Result:=''''+GetStrProp(Instance, PropInfo)+'''';
    tkSet: Result:=SetToString(PropInfo,GetOrdProp(Instance, PropInfo),true);
    tkClass: Result:=GetObjAsString(TObject(GetOrdProp(Instance, PropInfo)));
    tkVariant: Result:=GetVariantProp(Instance, PropInfo);
  end;
end;

function ListProperties(Instance: TComponent; List:TStrings):boolean;
var
  i, count: Integer;
  TypeInfo: PTypeInfo;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  List.Clear;
  Result:= true;

  if Instance<>nil then try
    list.Add(format(RTTIFormatstr,[Instance.Name,Instance.ClassName]));

    TypeInfo:=Instance.ClassInfo;

    count:=GetPropList(TypeInfo, tkProperties, nil);
    GetMem(PropList, count * SizeOf(Pointer));
    try
      GetPropList(TypeInfo, tkProperties, PropList);
      for i:=0 to count-1 do begin
        PropInfo:=PropList^[i];
        {Speichern der Eigenschaftsnamen mit Zeiger auf dessen
         PropInfo-Struktur in der Liste}
        List.Add(format(PropFormatStr,[PropInfo.Name,GetPropValAsString(Instance, PropInfo)]));
      end;
    List.Add('');
    finally
      FreeMem(PropList);
    end;
  except
    on E: Exception do begin
      ShowConsole('Fehler bei der Anzeige der Eigenschaften: '+E.Message);
      Result:= false;
    end;
  end;
end;

function ShowConsoleRTTI(Which:TComponent):boolean;
var sl:TStringList;
begin
  sl:= TStringList.Create;
  try
    Result:= ListProperties(Which,sl);
    if Result then
      Result:= ShowConsole(sl);
  finally
    sl.Free;
  end;
end;

var PerfFreq:int64;

function TimeMeasure:int64;
begin
  QueryPerformanceCounter(Result);
end;

function TimeDelta(First,Second:int64):double;
begin
  Result:= (Second-First)/PerfFreq*1000;
end;

function TimeToMSec(ATime:int64):double;
begin
  Result:= ATime/PerfFreq*1000;
end;

function TimeStop(First:int64):double;
begin
  Result:= TimeDelta(First,TimeMeasure);
end;

initialization
  QueryPerformanceFrequency(PerfFreq);
  if AllocConsole then begin
    TTextRec(Input).Handle:= GetStdHandle(STD_INPUT_HANDLE);
    TTextRec(Output).Handle:= GetStdHandle(STD_OUTPUT_HANDLE);
    TTextRec(ErrOutput).Handle:= GetStdHandle(STD_ERROR_HANDLE);
    System.IsConsole:= true;
  end;

finalization
  FreeConsole;

end.
