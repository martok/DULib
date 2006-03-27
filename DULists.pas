unit DULists;

interface

uses Classes, Math;

type
  TIntegerArray = array of Integer;
  TSortedIntList = class(TList)
  private
    FOnChange: TNotifyEvent;
    function GetInteger(Index: integer): integer;
    procedure SetInteger(Index: integer; Value: integer);
  public
    function FirstFree(Start: integer):integer;
    function MaxVal:integer;
    function MinVal:integer;
    function AsArray: TIntegerArray;
    procedure Add(Value: integer); reintroduce;
    procedure Insert(Index, Value: integer); reintroduce;
    function Find(Value: integer; var Index: Integer): Boolean;
    procedure Sort;
    property Integers[index: integer]: integer read GetInteger write SetInteger; default;
    property OnChange :TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

function TSortedIntList.GetInteger(Index: integer): integer;
begin
  result:=Integer(Items[index]);
end;

procedure TSortedIntList.SetInteger(Index: integer; Value: integer);
begin
  Items[index]:=Pointer(Value);
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TSortedIntList.Insert(Index, Value: integer);
begin
  inherited Insert(Index, Pointer(Value));
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TSortedIntList.Add(Value: integer);
var
  Index: integer;
begin
  if not Find(Value, Index) then
    Insert(Index, Value);
end;

function TSortedIntList.Find(Value: integer; var Index: Integer):
Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Integers[I] - Value;
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then
        Result := True;
    end;
  end;
  Index := L;
end;


function SortProc(Item1, Item2: Pointer): integer;
begin
  Result:= 0;
  if Integer(Item1)<Integer(Item2) then Result:= -1 else
  if Integer(Item1)=Integer(Item2) then Result:=  0 else
  if Integer(Item1)>Integer(Item2) then Result:= +1;
end;

procedure TSortedIntList.Sort;
begin
  inherited Sort(SortProc);
end;

function TSortedIntList.FirstFree(Start: integer):integer;
var i,x:integer;
begin
  for i:= Start to MaxVal do begin
    x:= 0;
    if not Find(i,x) then begin
      Result:= i;
      exit;
    end;
  end;
  Result:= Succ(MaxVal);
end;

function TSortedIntList.MaxVal: integer;
begin
  Result:= MaxIntValue(AsArray);
end;

function TSortedIntList.MinVal: integer;
begin
  Result:= MinIntValue(AsArray);
end;

function TSortedIntList.AsArray: TIntegerArray;
var i:integer;
begin
  SetLength(Result,Count);
  for i:= 0 to Count-1 do begin
    Result[i]:= Integers[i];
  end;
end;

end.
