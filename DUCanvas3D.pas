{-----------------------------------------------------------------------------
 Unit Name: DUCanvas3D
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   3 Dimensional Canvas class. Uses simple projection.

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}
unit DUCanvas3D;

interface

uses Windows, Consts, Classes,Graphics, math;

type
  TPoint3d = record X,Y,Z:integer; end;

  TSurface = record LF,RF,RR,LR:TPoint3d; end;

  TCuboid = record
              BLF,BRF,BRR,BLR,
              TLF,TRF,TRR,TLR:TPoint3d;
            end;

  TColorRec = record
                case boolean of
                  true: (Col:TColor);
                  false: (B,G,R:byte);
              end;

  TCanvas3D = class
  private
    FQ: Single;
    FAngle: Single;
    FHeight: integer;
    FWidth: integer;
    function GetBrush: TBrush;
    function GetFont: TFont;
    function GetPen: TPen;
  protected
    FCanvas:TCanvas;

    function ProjectPoint(X,Y,Z:integer):TPoint;    overload;
    function ProjectPoint(P: TPoint3d): TPoint;  overload;
    function ColorFactor(S: array of TPoint3d): single;
  public
    constructor Create(ACanvas:TCanvas);

    property Angle : Single read FAngle write FAngle;
    property Q : Single read FQ write FQ;
    property Width : integer read FWidth write FWidth;
    property Height : integer read FHeight write FHeight;

    procedure LineTo(P:TPoint3d);   overload;
    procedure MoveTo(P:TPoint3d);   overload;
    procedure LineTo(X, Y, Z: integer);  overload;
    procedure MoveTo(X, Y, Z: integer);  overload;
    procedure Polygon(Points: array of TPoint3d);
    procedure FillRect(const Rect: TRect);
    procedure FrameRect(const Rect: TRect);

    procedure FillCuboid(const Cuboid: TCuboid; Open:boolean=false);
    procedure FrameCuboid(const Cuboid: TCuboid; Open:boolean);
    procedure FillSurface(const Surface:TSurface);
  published
    property Brush: TBrush read GetBrush;
    property Font: TFont read GetFont;
    property Pen: TPen read GetPen;
  end;


function Point3d(X,Y,Z:integer):TPoint3d;
function CuboidBounds(X,Y,Z, CX,CY,CZ:integer):TCuboid;
function Surface(LF,RF,RR,LR:TPoint3d):TSurface;

implementation

function Sin(X:double):double;
begin
  Result:= System.Sin(X* Pi / 180);
end;

function Cos(X:double):double;
begin
  Result:= System.Cos(X* Pi / 180);
end;

function ProjectPointAQ(X, Y, Z, A,Q:double): TPoint;
begin
  Result.x:= round(X+cos(A)*Z*Q);
  Result.y:= round(Y+Sin(A)*Z*Q);
end;

function Volume(Cuboid:TCuboid):double;
begin
  with Cuboid do
    Result:= (BRF.x-BLF.X)*
             (TLF.y-BLF.y)*
             (BLR.z-BLF.z);
end;

function PolygonArea(Points: array of TPoint):double;
var i,j:integer;
    area:double;
begin
   area:= 0;
   //--- Compute
   for i:=0 to high(Points) do begin
      j := (i + 1) mod length(Points);
      area := area + Points[i].x * Points[j].y;
      area := area - Points[i].y * Points[j].x;
   end;

   area:= area / 2;
   Result:= abs(area);
end;

function Point3d(X,Y,Z:integer):TPoint3d;
begin
  Result.X:= X;
  Result.Y:= Y;
  Result.Z:= Z;
end;

function CuboidBounds(X,Y,Z, CX,CY,CZ:integer):TCuboid;
begin
  with Result do begin
    BLF:=Point3d(X   ,Y,Z);
    BRF:=Point3d(X+CX,Y,Z);
    BRR:=Point3d(X+Cx,Y,Z+CZ);
    BLR:=Point3d(X   ,Y,Z+CZ);

    TLF:=Point3d(X   ,Y+CY,Z);
    TRF:=Point3d(X+CX,Y+CY,Z);
    TRR:=Point3d(X+Cx,Y+CY,Z+CZ);
    TLR:=Point3d(X   ,Y+CY,Z+CZ);
  end;
end;

function Surface(LF,RF,RR,LR:TPoint3d):TSurface;
begin
  Result.LF:= LF;
  Result.RF:= RF;
  Result.RR:= RR;
  Result.LR:= LR;
end;

{ TCanvas3D }

constructor TCanvas3D.Create(ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas:= ACanvas;
  FQ:= 0.5;
  FAngle:= 45;
end;

//==============================================================================
//                       Functions Accessing Canvas                                                       

function TCanvas3D.ProjectPoint(X, Y, Z: integer): TPoint;
begin
  Result:= ProjectPointAQ(x,y,z,FAngle,FQ);
  Result.y:= Height-Result.y-2;
end;

function TCanvas3D.ProjectPoint(P:TPoint3d): TPoint;
begin
  Result:= ProjectPoint(P.X,P.Y,P.z);
end;

procedure TCanvas3D.LineTo(X, Y, Z: integer);
var p:TPoint;
begin
  p:= ProjectPoint(X,Y,Z);
  FCanvas.LineTo(p.x,p.y);
end;

procedure TCanvas3D.MoveTo(X, Y, Z: integer);
var p:TPoint;
begin
  p:= ProjectPoint(X,Y,Z);
  FCanvas.MoveTo(p.x,p.y);
end;

procedure TCanvas3D.Polygon(Points: array of TPoint3d);
var p:array of TPoint;
    i:integer;
begin
  SetLength(p,length(Points));
  for i:= 0 to high(Points) do
    p[i]:= ProjectPoint(Points[i].X,Points[i].Y,Points[i].Z);
  FCanvas.Polygon(p);
end;

//==============================================================================

procedure TCanvas3D.LineTo(P: TPoint3d);
begin
  LineTo(P.X,P.Y,P.Z);
end;

procedure TCanvas3D.MoveTo(P: TPoint3d);
begin
  MoveTo(P.X,P.Y,P.Z);
end;




function TCanvas3D.GetBrush: TBrush;
begin
  Result:= FCanvas.Brush;
end;

function TCanvas3D.GetFont: TFont;
begin
  Result:= FCanvas.Font;
end;

function TCanvas3D.GetPen: TPen;
begin
  Result:= FCanvas.Pen;
end;

procedure TCanvas3D.FillRect(const Rect: TRect);
begin
  FCanvas.FillRect(rect);
end;

procedure TCanvas3D.FrameRect(const Rect: TRect);
begin
  FCanvas.FrameRect(rect);
end;

function TCanvas3D.ColorFactor(S: array of TPoint3d): single;
var f,A1,A2:double;
    p1,p2:array of TPoint;
    i:integer;
begin
  SetLength(p1,length(S));
  SetLength(p2,length(S));
  for i:= 0 to High(s) do begin
    p1[i]:= ProjectPoint  (S[i].X,S[i].Y,S[i].Z);
    p2[i]:= ProjectPointAQ(S[i].X,S[i].Y,S[i].Z,5,0.1);
    P2[i].y:= Height-P2[i].y-2;
  end;
  A1:= PolygonArea(p1);
  A2:= PolygonArea(p2);
  if A1<A2 then
    f:= A1/A2 else
  if A2<A1 then
    f:= A2/A1 else
  if A1=0 then
    f:= 0 else
    f:=1;
    
  if f > 1.0 then f := 1.0;
  if f <= 0.2 then f := 0.2;
  Result:= f;
end;

procedure TCanvas3D.FillSurface(const Surface: TSurface);
var c:TColorRec;
    save:TColor;
    s:TBrushStyle;
    f:double;
begin
  save:= Brush.Color;
  s:= Brush.Style;
  try
    with Surface do begin
      if Brush.Style<>bsClear then begin
        f:= ColorFactor([LF,RF,RR,LR]);
        c.Col:= Brush.Color;
        c.B:= trunc(c.B * f);
        c.G:= trunc(c.G * f);
        c.R:= trunc(c.R * f);
        Brush.Color:= c.col;
        Brush.Style:= s;
      end;
      Polygon([LF,RF,RR,LR]);
    end;
  finally
    Brush.Color:= save;
    Brush.Style:= s;
  end;
end;

procedure TCanvas3D.FillCuboid(const Cuboid: TCuboid; Open:boolean=false);
begin
  if Volume(Cuboid)=0 then exit;
  with Cuboid do begin
    FillSurface(Surface(BLF,BRF,BRR,BLR));
    FillSurface(Surface(BLF,BLR,TLR,TLF));
    FillSurface(Surface(BLR,BRR,TRR,TLR));
    if not Open then begin
      FillSurface(Surface(BLF,BRF,TRF,TLF));
      FillSurface(Surface(BRF,BRR,TRR,TRF));
      FillSurface(Surface(TLF,TRF,TRR,TLR));
    end;
  end;
end;

procedure TCanvas3D.FrameCuboid(const Cuboid: TCuboid; Open:boolean);
var OldStyle:TBrushStyle;
begin
  OldStyle:= Brush.Style;
  Brush.Style:= bsClear;
  FillCuboid(Cuboid, Open);
  Brush.Style:= OldStyle;
end;




end.
