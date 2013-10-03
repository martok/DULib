{-----------------------------------------------------------------------------
 Unit Name: DUParams
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   Simple acces to command line parameters in the form paramname=paramvalue

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}
unit DUParams;

interface
uses SysUtils, Classes, Windows;

type TParamList = class(TStringList)
     protected
       function Get(Index: Integer): string; override;
     end;

var  ParamList:TParamList;
     ParamAlias:TStringList;

procedure RebuildParamList;
function GetPrmValue(Index:integer):string; overload;
function GetPrmValue(Name:string):string;   overload;
function ParamExists(Name:string):boolean;
function CatParams(First,Last:integer; Quote:boolean=false):string;

implementation

{ TParamList }

function TParamList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= Count) then Result:= '' else
  Result := inherited Get(Index);
end;

function GetPrmValue(Index:integer):string;
begin
  Result:= ParamList.ValueFromIndex[Index];
end;

function GetPrmValue(Name:string):string;
begin
  Result:= ParamList.Values[Name];
end;

function ParamExists(Name:string):boolean;
begin
  Result:= (ParamList.IndexOfName(Name)>-1) or
           (ParamList.IndexOf(Name)>-1);
end;

function CatParams(First,Last:integer; Quote:boolean=false):string;
var i:integer;
begin
  if Quote then begin
    Result:= '"'+ParamStr(First)+'"';
    for i:= First+1 to Last do
      Result:= Result+' "'+ParamStr(i)+'"';
  end else begin
    Result:= ParamStr(First);
    for i:= First+1 to Last do
      Result:= Result+' '+ParamStr(i);
  end;
end;

procedure BuildParamList;
var i:integer;
    s,n,v:string;
begin
  for i:= 1 to ParamCount do begin
    s:= ParamStr(i);
    n:= copy(s,1,pos('=',s)-1);
    v:= copy(s,pos('=',s)+1,maxint);
    if n='' then s:= v else
      s:= n+'='+v;
    ParamList.Add(s);
  end;
end;

procedure RebuildParamList;
begin
  ParamList.Clear;
  BuildParamList;
end;




initialization
  ParamList:= TParamList.Create;
  ParamAlias:= TStringList.Create;
  BuildParamList;
finalization
  ParamList.Free;
  ParamAlias.Free;

end.
 