unit DUHugeNumber;

interface

uses SysUtils,Math;

type
  TZiffer = type Cardinal;

const
//  hnBase = 65000;        //max sqrt(high(Ziffer))
  hnBase = 10;        //max sqrt(high(Ziffer))

type
  TNumber = packed array of TZiffer;
  TZahl = record
            IntPart,
            FracPart:TNumber;
            Sign:TValueSign;
          end;

  THugeNumber = class
  private
    function GetInt64: Int64;
    procedure SetInt64(const Value: Int64);
    function GetDouble: Double;
    procedure SetDouble(const Value: Double);
    function GetString: string;
    procedure SetString(const Value: string);
  public
    Val:TZahl;
    constructor Create;
    destructor Destroy; override;
    property AsInt64:Int64 read GetInt64 Write SetInt64;
    property AsDouble:Double read GetDouble write SetDouble;
    property AsString:string read GetString write SetString;
  end;

function HugeNumberNew:THugeNumber;

procedure HN_Init(var Z:TZahl);
function HN_ToInt64(Z:TZahl):int64;
function HN_ToDouble(Z:TZahl):Double;
function HN_ToString(Z:TZahl):String;
procedure HN_FromInt64(var Z:TZahl; Val:int64);
procedure HN_FromDouble(var Z:TZahl; Val:Double);
procedure HN_FromString(var Z:TZahl; Val:String);

procedure HN_Add(var A:TZahl; B:TZahl);
procedure HN_Multiply(var A:TZahl; B:TZahl);

implementation

////////////////////////////////
////
////   Math Stuff
////
////////////////////////////////
const
  Epsilon = 1E-15 * 1000;

function Power(Base:double; Exp:integer):double;
begin
  Result:= Math.IntPower(Base,Exp);
end;

// IntPower should return Int
// Also make the Compiler Check for Exp<0 (use Power() for this)
function IntPower(Base:double; Exp:Cardinal):int64;
begin
  Result:= trunc(Math.IntPower(Base,Exp));
end;






////////////////////////////////
////
////   Utility functions
////
////////////////////////////////
procedure HN_ChangeLength(var N:TNumber; NewLength:integer);
var i,L:integer;
begin
  l:= length(n);
  SetLength(N,NewLength);
  if NewLength>l then
    for i:= l to NewLength-1 do
      N[i]:= 0;
end;

procedure HN_Optimize(var Z:TZahl);
  procedure Opt(var a:TNumber);
  var i:integer;
  begin
    for i:= high(a) downto 0 do
      if a[i]<>0 then begin
        SetLength(a,i+1);
        break;
      end;
  end;
begin
  Opt(Z.FracPart);
  Opt(Z.intPart);
end;

procedure HN_FlipNum(num:TNumber; out new:TNumber);
var i:integer;
begin
  SetLength(new,length(Num));
  for i:= 0 to high(num) do
    new[i]:= num[high(num)-i];
end;

procedure HN_BaseConv(Feld1:TZahl;
                      out Feld2 :TZahl;
                      Bas1,Bas2   :Cardinal);
var i,j,l:integer;
    z:TZiffer;
    a,r:TZahl;
begin
  if Bas1=bas2 then begin Feld2:=Feld1; exit end;
  HN_Init(r);
  for i:= High(Feld1.FracPart) downto 0 do begin
    z:= Feld1.FracPart[i]*IntPower(Bas1,i-1)  ;

    j:= 0;
    l:= 0;
    while z>0 do begin
      if j>=l then begin
        inc(l,5);
        SetLength(a.IntPart,l);
      end;
      a.IntPart[j]:= z mod Bas2;
      z:=z div Bas2;
      inc(j);
    end;
    SetLength(a.IntPart,j);

    HN_add(r,a);
  end;
  Feld2.FracPart:= r.FracPart;

  HN_Init(r);
  for i:= 0 to High(Feld1.IntPart) do begin
    z:= Feld1.IntPart[i]*IntPower(Bas1,i)  ;

    j:= 0;
    l:= 0;
    while z>0 do begin
      if j>=l then begin
        inc(l,5);
        SetLength(a.IntPart,l);
      end;
      a.IntPart[j]:= z mod Bas2;
      z:=z div Bas2;
      inc(j);
    end;
    SetLength(a.IntPart,j);

    HN_add(r,a);
  end;
  Feld2.IntPart:= r.IntPart;
end;


////////////////////////////////
////
////   Procedural HN handling
////
////////////////////////////////
function HugeNumberNew:THugeNumber;
begin
  Result:= THugeNumber.Create;
end;

procedure HN_Init(var Z:TZahl);
begin
  with z do begin
    SetLength(IntPart,0);
    SetLength(FracPart,0);
    Sign:= +1;
  end;
end;

function HN_ToInt64(Z:TZahl):int64;
var i:integer;
begin
  Result:= 0;
  with Z do begin
    for i:= 0 to high(IntPart) do
      inc(Result,IntPart[i]*IntPower(hnBase,i));
    Result:= Result*Sign;
  end;
end;

function HN_ToDouble(Z:TZahl):Double;
var i:integer;
begin
  Result:= 0;
  with Z do begin
    for i:= 0 to high(IntPart) do
      Result:= Result+IntPart[i]*IntPower(hnBase,i);
    for i:= 0 to high(FracPart) do
      Result:= Result+FracPart[i]*Power(hnBase,-(i+1));
    Result:= Result*Sign;
  end;
end;

function HN_ToString(Z:TZahl):String;
var Z2:TZahl;
    i:integer;
begin
  HN_BaseConv(Z,Z2,hnBase,10);
  Z2.Sign:= Z.Sign;
  HN_Optimize(Z2);
  with Z2 do begin
    if Sign<0 then
      Result:= '-'
    else
      Result:= '';
    for i:= high(IntPart) downto 0 do
      Result:= Result+chr(IntPart[i]+ord('0'));
    if length(FracPart)>0 then begin
      Result:= Result+DecimalSeparator;
      for i:= 0 to high(FracPart) do
        Result:= Result+chr(FracPart[i]+ord('0'));
    end;
  end;
end;

procedure HN_FromInt64(var Z:TZahl; Val:int64);
var v:int64;
    i,l:integer;
begin
  with Z do begin
    SetLength(FracPart,0);
    Sign:= Math.Sign(Val);
    v:= val*Sign;
    i:= 0;
    l:= 0;
    while v>0 do begin
      if i>=l then begin
        inc(l,5);
        SetLength(IntPart,l);
      end;
      IntPart[i]:= v mod hnBase;
      v:=v div hnBase;
      inc(i);
    end;
    SetLength(IntPart,i);
  end;
end;

procedure HN_FromDouble(var Z:TZahl; Val:Double);
var v,f:double;
    i,l:integer;
begin
  with Z do begin
    Sign:= Math.Sign(Val);
    v:= val*Sign;
    f:= frac(V);
    f:= RoundTo(f,-14);  //truncate periodics after maximum precision
    HN_FromInt64(Z,Trunc(v));

    i:= 0;
    l:= 0;
    while (f>epsilon) and (i<15) do begin //max 15 stellen, mehr hat Double nicht!
      if i>=l then begin
        inc(l,5);
        SetLength(FracPart,l);
      end;
      f:= f*hnBase;
      FracPart[i]:= trunc(f);
      f:= f-FracPart[i];
      inc(i);
    end;
    SetLength(FracPart,i);
  end;
end;

procedure HN_FromString(var Z:TZahl; Val:String);
begin

end;

procedure HN_Add(var A:TZahl; B:TZahl);
var i,m_s:integer;
    sum,carry:TZiffer;
    Af,Bf,Ai,Bi:integer;
begin
  Af:= high(A.FracPart);
  Bf:= high(B.FracPart);
  Ai:= high(A.IntPart);
  Bi:= high(B.IntPart);


  //make all the same length:
  m_S:= Max(Af,Bf);
  HN_ChangeLength(A.FracPart,m_s+1);
  HN_ChangeLength(B.FracPart,m_s+1);
  carry:= 0;
  for i:= m_s downto 0 do begin
    sum:= A.FracPart[i]+B.FracPart[i]+carry;
    carry:= 0;
    If Sum >= hnBase then Begin
      dec(Sum,hnBase);
      inc(carry)
    end;
    A.FracPart[i] := Sum;
  end;

  //make all the same length:
  m_S:= Max(Ai,Bi);
  HN_ChangeLength(A.IntPart,m_s+1);
  HN_ChangeLength(B.IntPart,m_s+1);
  for i:= 0 to m_s do begin
    sum:= A.IntPart[i]+B.IntPart[i]+carry;
    carry:= 0;
    If Sum >= hnBase then Begin
      Sum := Sum-hnBase;
      inc(carry)
    end;
    A.IntPart[i] := Sum;
  end;
  If carry = 1 then begin
    SetLength(A.IntPart,m_s+2);
    A.IntPart[m_s+1] := 1;
  end;

  HN_Optimize(A);
end;

function HN_RemoveDecimals(A,B:TZahl; out X,Y:TNumber):integer;
var
  Af,Bf,Ai,Bi:integer;
  tmp:TNumber;

  procedure CopyNum(deci,l:integer; i:TZahl; out j:TNumber);
  begin
    HN_ChangeLength(j,l+deci);
    Move(i.intpart[0],j[deci],l*Sizeof(Tziffer));
    HN_ChangeLength(i.FracPart,deci);
    HN_FlipNum(i.FracPart,tmp);
    Move(tmp[0],j[0],deci*Sizeof(TZiffer));
  end;

begin
  Af:= length(A.FracPart);
  Bf:= length(B.FracPart);
  Ai:= length(A.IntPart);
  Bi:= length(B.IntPart);

  Result:= Max(Af,Bf);

  CopyNum(Result,Ai,A,X);
  CopyNum(Result,Bi,B,Y);
end;

procedure HN_Multiply(var A:TZahl; B:TZahl);
var i,m_s, deci,j,st:integer;
    sum:TZiffer;
    Af,Bf,Ai,Bi:integer;
    tmp,nr1,nr2:TNumber;
begin
  Af:= length(A.FracPart);
  Bf:= length(B.FracPart);
  Ai:= length(A.IntPart);
  Bi:= length(B.IntPart);


  deci:= HN_RemoveDecimals(A,B,nr1,nr2);

  // Variablen initialisieren
  sum := 0;
  st:= 0;
  // Längste Zahl bestimmen
  m_S:= Max(Af+Ai,Bf+Bi);
  // Mit Nullen auffüllen
  HN_ChangeLength(nr1,m_s);
  HN_ChangeLength(nr2,m_s);

  HN_ChangeLength(tmp,m_s*m_s);
  // Von Rechts zur Mitte
  for i := 0 to m_s-1 do
  begin
    for j := 0 to i do
      sum := sum + nr1[j] * nr2[i-j];
    tmp[st]:= sum mod hnBase;
    inc(st);
    sum := sum div hnBase;
  end;
  // Von der Mitte Nach links
  for i := m_s - 2 downto 0 do
  begin
    for j := 0 to i do
      sum := sum + nr1[m_s - 1 - j] * nr2[m_S - 1 - i + j];
    tmp[st]:= sum mod hnBase;
    inc(st);
    sum := sum div hnBase;
  end;
  // Letzten Rest hinzufügen
  tmp[st]:= sum;

  deci:= deci shl 1;   // * 2
  HN_ChangeLength(A.IntPart,length(tmp)-deci);
  Move(tmp[deci],A.Intpart[0],length(A.IntPart)*Sizeof(Tziffer));

  HN_ChangeLength(A.FracPart,deci);
  Move(tmp[0],A.Fracpart[0],length(A.FracPart)*Sizeof(Tziffer));
  HN_FlipNum(a.FracPart,tmp);
  Move(tmp[0],A.Fracpart[0],length(A.FracPart)*Sizeof(Tziffer));

  HN_Optimize(A);
end;


////////////////////////////////
////
////   THugeNumber class
////
////////////////////////////////

constructor THugeNumber.Create;
begin
  inherited;
  HN_Init(val);
end;

destructor THugeNumber.Destroy;
begin
  HN_Init(Val);
  inherited;
end;

function THugeNumber.GetInt64: Int64;
begin
  Result:= HN_ToInt64(Val);
end;

procedure THugeNumber.SetInt64(const Value: Int64);
begin
  HN_FromInt64(Val,Value);
end;

function THugeNumber.GetDouble: Double;
begin
  Result:= HN_ToDouble(Val);
end;

procedure THugeNumber.SetDouble(const Value: Double);
begin
  HN_FromDouble(Val,Value);
end;

function THugeNumber.GetString: string;
begin
  Result:= HN_ToString(Val);
end;

procedure THugeNumber.SetString(const Value: string);
begin
  HN_FromString(Val,Value);
end;

end.
