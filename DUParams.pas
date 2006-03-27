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

implementation

{ TParamList }

function TParamList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= Count) then Result:= '' else
  Result := inherited Get(Index);
end;

function GetPrmValue(Index:integer):string;
begin
  Result:= GetPrmValue(ParamList.Names[Index]);
end;

function GetPrmValue(Name:string):string;
begin
  Result:= ParamList.Values[Name];
end;

function ParamExists(Name:string):boolean;
begin
  Result:= ParamList.IndexOf(Name)>-1;
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
 