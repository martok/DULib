{-----------------------------------------------------------------------------
 Unit Name: DUHint
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   Show custom Hint windows

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}
unit DUHint;

interface

uses Windows,Controls,Forms,Graphics,Classes;

type
  TCustomHint = class
  private
    FHintWnd:THintWindow;
    FText: string;
    FData: Pointer;
    procedure SetText(const Value: string);
    function GetCanvas: TCanvas;
  public
    constructor Create(AOwner:TComponent);
    destructor Destroy;  override;

    procedure ShowHint(Rect:TRect);              overload;
    procedure ShowHint(X, Y, MaxWidth: integer); overload;
    procedure ShowHint(XY: TPoint; MaxWidth: integer); overload;
    procedure HideHint;
    function GetRect(MaxWidth:integer):TRect;
    property Text : string read FText write SetText;
    property Data : Pointer read FData write FData;
    property Canvas : TCanvas read GetCanvas;
  end;

implementation

{ TCustomHint }

constructor TCustomHint.Create(AOwner:TComponent);
begin
  inherited Create;
  FHintWnd:= HintWindowClass.Create(AOwner);
  FHintWnd.Color:= Application.HintColor;
end;

destructor TCustomHint.Destroy;
begin
  FHintWnd.Free;
  inherited;
end;


function TCustomHint.GetCanvas: TCanvas;
begin
  Result:= FHintWnd.Canvas;
end;

function TCustomHint.GetRect(MaxWidth:integer): TRect;
begin
  Result:= FHintWnd.CalcHintRect(MaxWidth,Text,nil);
end;

procedure TCustomHint.HideHint;
begin
  FHintWnd.ReleaseHandle;
end;

procedure TCustomHint.SetText(const Value: string);
begin
  FText := Value;
  if FHintWnd.HandleAllocated then
    FHintWnd.ActivateHint(FHintWnd.BoundsRect,Value);
end;

procedure TCustomHint.ShowHint(Rect: TRect);
begin
  FHintWnd.ActivateHint(Rect,FText);
end;

procedure TCustomHint.ShowHint(X, Y, MaxWidth: integer);
var r:TRect;
begin
  r:= GetRect(MaxWidth);
  r.TopLeft:= Point(X,Y);
  inc(r.Bottom,Y);
  inc(r.Right,X);
  ShowHint(r);
end;

procedure TCustomHint.ShowHint(XY:TPoint; MaxWidth: integer);
begin
  ShowHint(XY.X,XY.Y,MaxWidth);
end;

end.
 