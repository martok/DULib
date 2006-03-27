unit DUAngles;

interface

function NA(a:double):double;
function Degr(X: Extended): Extended;
function Rad(X: Extended): Extended;
procedure GetXY(const Theta,Radius:double; var x,y:integer);

implementation

uses Math;

function NA(a:double):double;
begin
  if a>360 then result:= a-360 else
  if a<0   then result:= a+360 else
  result:= a;
end;

function Degr(X: Extended): Extended;
begin
  Result:= RadToDeg(NA(X));
end;

function Rad(X: Extended): Extended;
begin
  Result:= DegToRad(NA(X));
end;

procedure GetXY(const Theta,Radius:double; var x,y:integer);
var t:double;
begin
  t:= Rad(Theta);
  y:= Round(-Radius*cos(t));
  x:= round(-Radius*sin(t));
end;

end.
