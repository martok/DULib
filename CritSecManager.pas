unit CritSecManager;

interface

uses
  Classes, SyncObjs, SysUtils;

type
  TCritcalSecData = class(TObject)
  private
    FName: string;
    FSection: TCriticalSection;
    FActive: boolean;
  public
    property Name    : string read FName write FName;
    property Active  : boolean read FActive;
    property Section : TCriticalSection read FSection write FSection;
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;

  TCritSecManager = class(TComponent)
  private
    { Private-Deklarationen }
    FCritSecs: TList;
    FAutoAdd: boolean;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    property AutoAdd:boolean read FAutoAdd write FAutoAdd default true;

    procedure AddCritSec(Name:string);
    procedure DeleteCritSec(Name: string);
    procedure DeleteAll;

    procedure Enter(Name:string);
    procedure Leave(Name:string);
    procedure LeaveAll;
    function Find(Name:string):integer;
  end;

var CritSec:TCritSecManager;

implementation

{ TCrtitcalSecData }

constructor TCritcalSecData.Create;
begin
  inherited Create;
  FSection:= TCriticalSection.Create;
end;

destructor TCritcalSecData.Destroy;
begin
  FSection.Free;
  inherited Destroy;
end;


procedure TCritcalSecData.Enter;
begin
  FSection.Enter;
  FActive:= true;
end;

procedure TCritcalSecData.Leave;
begin
  FSection.Leave;
  FActive:= false;
end;

{ TCritSecManager }


constructor TCritSecManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCritSecs:= TList.Create;
  FAutoAdd:= true;
end;


destructor TCritSecManager.Destroy;
begin
  LeaveAll;
  DeleteAll;
  FCritSecs.Free;
end;

function TCritSecManager.Find(Name: string): integer;
var i:integer;
begin
  Name:= UpperCase(Name);
  Result:= -1;
  for i:=0 to FCritSecs.Count - 1 do begin
    if (UpperCase(TCritcalSecData(FCritSecs[i]).Name)=Name) then begin
      Result:= i;
      break;
    end;
  end;
end;

procedure TCritSecManager.AddCritSec(Name: string);
var csd:TCritcalSecData;
begin
  if Find(Name)>-1 then exit;
  csd:= TCritcalSecData.Create;
  csd.Name:= UpperCase(Name);
  FCritSecs.Add(csd);
end;

procedure TCritSecManager.DeleteCritSec(Name: string);
var csd:TCritcalSecData;
    i:integer;
begin
  i:= Find(Name);
  if i=-1 then exit;
  Leave(Name);
  csd:= TCritcalSecData(FCritSecs[i]);
  csd.Free;
  FCritSecs.Delete(i);
end;

procedure TCritSecManager.Enter(Name: string);
var csd:TCritcalSecData;
    i:integer;
begin
  i:= Find(Name);
  if i=-1 then
    if FAutoAdd then AddCritSec(Name)
    else exit;
  i:= Find(Name);
  csd:= TCritcalSecData(FCritSecs[i]);
  csd.Enter;
end;

procedure TCritSecManager.Leave(Name: string);
var csd:TCritcalSecData;
begin
  if Find(Name)=-1 then exit;
  csd:= TCritcalSecData(FCritSecs[Find(Name)]);
  csd.Leave;
end;

procedure TCritSecManager.LeaveAll;
var i:integer;
begin
  for i:=0 to FCritSecs.Count - 1 do
    TCritcalSecData(FCritSecs[i]).Leave;
end;

procedure TCritSecManager.DeleteAll;
var i:integer;
begin
  for i:=0 to FCritSecs.Count - 1 do
    DeleteCritSec(TCritcalSecData(FCritSecs[i]).Name);
end;


initialization
  CritSec:= TCritSecManager.Create(nil);

finalization
  CritSec.LeaveAll;
  CritSec.Free;
  CritSec:= nil;

end.
 