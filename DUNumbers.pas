unit DUNumbers;

interface

uses SysUtils, SysConst, Math;

function StringToFloat(S:String):Double;
function FloatToString(V:Double; DS:Char='~'):String;

Function Fakultaet(Zahl : integer): Integer;
Function Quersumme(i : integer): integer;
Function IsPrim(zahl : Integer): boolean;
function RoundToPointPosition(Number : Extended; PointPos : Byte): Extended;
function ArithRound(x: Extended): Int64;
function nteWurzel(n, radikand: integer): real;

function DezToX(dez, bas: Cardinal): String;
function DecToRoman ( iDecimal: longint ): string;
function ZahlToString(z: LongInt): string;
function ZahlToStringEnglish(z: LongInt): string;

implementation

resourcestring SBadBase = 'Unzulässige Zahlenbasis:';

procedure ConvertErrorFmt(const Ident: string; const Args: array of const);
begin
  raise EConvertError.CreateFmt(Ident, Args);
end;


function StringToFloat(S:String):Double;
var dss:char;
    tmp:Extended;
begin
  dss:= DecimalSeparator;
  try
    DecimalSeparator:= '.';
    if not TextToFloat(PChar(S), tmp, fvExtended) then begin
      DecimalSeparator:= ',';
      if not TextToFloat(PChar(S), tmp, fvExtended) then
        ConvertErrorFmt(SInvalidFloat, [S]);
    end;
    Result:= tmp;
  finally
    DecimalSeparator:= dss;
  end;
end;

function FloatToString(V:Double; DS:Char='~'):String;
var dss:char;
begin
  dss:= DecimalSeparator;
  try
    if DS<>'~' then DecimalSeparator:= DS;
    Result:= Sysutils.FloatToStr(V);
  finally
    DecimalSeparator:= dss;
  end;
end;

Function Fakultaet(Zahl : integer): Integer;
begin
  If Zahl = 0 then
    result := 1
  else
    result := Zahl * Fakultaet(Zahl - 1);
end;

Function Quersumme(i : integer): integer;
var
p: pchar;
begin
 result := 0;
 p := Pchar(IntToStr(i));
 while (p^ <> #0) do
 begin
   result := result + strtoint(p^);
   Inc(p);
 end;
end;

Function IsPrim(zahl : Integer): boolean;
var
i: integer;
begin
  result := true;
  If zahl = 1 then
  begin
    result := false;
    exit;
  end;
  For i := 2 to Trunc(sqrt(zahl))+1 do
  begin
    If ((zahl mod i) = 0) then
    begin
      result := false;
      exit;
    end;
  end;
end;

function RoundToPointPosition(Number : Extended; PointPos : Byte): Extended;
Var
  Multi:Integer;
begin
  Multi:=Trunc(IntPower(10,PointPos));
  Result := Round(Number * Multi)/Multi;
end;

function ArithRound(x: Extended): Int64;
begin
  if frac(x) < 0.5 then result := floor(x)
  else result := ceil(x);
end;

function nteWurzel(n, radikand: integer): real;
begin
  if radikand>=0 then
    result:=power(radikand, 1/n)
  else result:=-1;
end;

function DezToX(dez, bas: Cardinal): String;
const
zahlen: array[0..15] of Char =
  ('0', '1', '2', '3', '4', '5', '6', '7',
   '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  If (Bas>16) OR (bas<2) Then Begin
     raise EInvalidArgument.Create(sBadBase+IntToStr(bas));
  End;
  if dez <> 0 then
    Result := DezToX(dez div bas, bas) + zahlen[dez mod bas]
  else
    Result := '';
end;

function DecToRoman ( iDecimal: longint ): string;
const
  aRomans: array [ 1..13 ] of string = ( 'I', 'IV', 'V',
   'IX', 'X', 'XL','L', 'XC', 'C', 'CD', 'D', 'CM', 'M' );
  aArabics: array [ 1..13 ] of integer = ( 1, 4, 5, 
   9, 10, 40, 50, 90, 100, 400, 500, 900, 1000 );
var
   i: integer;
begin
     for i := 13 downto 1 do
         while ( iDecimal >= aArabics [ i ] ) do begin
               iDecimal := iDecimal - aArabics [ i ];
               result := result + aRomans [ i ];
         end;
end;

{
  Autor:   Johannes Tränkle
  email:   Johannes@traenkle.org
  (c) 1998 by Johannes Tränkle
}
function ZahlToString(z: LongInt): string;
  function ZahlBis19(za:LongInt): string;
  begin
    case za of
      0: result:='null';
      1: result:='ein';
      2: result:='zwei';
      3: result:='drei';
      4: result:='vier';
      5: result:='fünf';
      6: result:='sechs';
      7: result:='sieben';
      8: result:='acht';
      9: result:='neun';
      10: result:='zehn';
      11: result:='elf';
      12: result:='zwölf';
      13: result:='dreizehn';
      14: result:='vierzehn';
      15: result:='fünfzehn';
      16: result:='sechzehn';
      17: result:='siebzehn';
      18: result:='achtzehn';
      19: result:='neunzehn';
    end;
  end;

  function ZahlBis99(za: LongInt): string;
  begin
    case za of
      1..19: result:=ZahlBis19(za);
      20: result:='zwanzig';
      21..29: result:=ZahlBis19(za-(za div 10)*10)+'undzwanzig';
      30: result:='dreissig';
      31..39: result:=ZahlBis19(za-(za div 10)*10)+'unddreisig';
      40: result:='vierzig';
      41..49: result:=ZahlBis19(za-(za div 10)*10)+'undvierzig';
      50:result:='fünfzig';
      51..59: result:=ZahlBis19(za-(za div 10)*10)+'undfünfzig';
      60:result:='sechzig';
      61..69: result:=ZahlBis19(za-(za div 10)*10)+'undsechzig';
      70: result:='siebzig';
      71..79: result:=ZahlBis19(za-(za div 10)*10)+'undsiebzig';
      80: result:='achzig';
      81..89: result:=ZahlBis19(za-(za div 10)*10)+'undachzig';
      90: result:='neunzig';
      91..99: result:=ZahlBis19(za-(za div 10)*10)+'undneunzig';
    end;
  end;

  function ZahlBis999(za: LongInt): string;
  begin
    case za of
      0..99: result:=zahlbis99(za);
      100: result:='einhundert';
      101..199: result:='einhundert und '+Zahlbis99(za-100);
      200: result:='zweihundert';
      201..299: result:='zweihundert und '+Zahlbis99(za-200);
      300: result:='dreihundert';
      301..399: result:='dreihundert und '+Zahlbis99(za-300);
      400: result:='vierhundert';
      401..499: result:='vierhundert und '+Zahlbis99(za-400);
      500: result:='fünfhundert';
      501..599: result:='fünfhundert und '+Zahlbis99(za-500);
      600: result:='sechshundert';
      601..699: result:='sechshundert und '+Zahlbis99(za-600);
      700: result:='siebenhundert';
      701..799: result:='siebenhundert und '+Zahlbis99(za-700);
      800: result:='achthundert';
      801..899: result:='achthundert und '+Zahlbis99(za-800);
      900: result:='neunhundert';
      901..999: result:='neunhundert und '+Zahlbis99(za-900);
    end;
  end;

var za:longInt;
begin
  result:='';
  if z<0 then
  begin
    result:='minus ';
    z:=abs(z);
  end;
  if z=1 then
  begin
    Result:=result+'eins';
    exit;
  end;
  za:=z;
  if za div 1000000000 > 0 then
  begin
    if za div 1000000000=1 then
      result:='eine Milliarde '
    else
      result:=Zahlbis999(za div 1000000000)+' Milliarden ';
      za:=za - (za div 1000000000)*1000000000;
  end;
  if za div 1000000 > 0 then
  begin
    if za div 1000000=1 then
      result:=result +'eine Million '
    else
      result:=result+Zahlbis999(za div 1000000)+' Millionen ';
    za:=za - (za div 1000000)*1000000;
  end;
  if za div 1000 > 0 then
  begin
    if za div 1000=1 then
      result:=result +'ein Tausend '
    else
      result:=result+Zahlbis999(za div 1000)+' Tausend ';
    za:=za - (za div 1000)*1000;
  end;
  if za > 0 then
    result:=result+Zahlbis999(za)+' ';
end;

{---------------------------------------------------}

function ZahlToStringEnglish(z: LongInt): string;
  function ZahlBis19(za: LongInt): string;
  begin
    case za of
      0: result:='zero';
      1: result:='one';
      2: result:='two';
      3: result:='three';
      4: result:='four';
      5: result:='five';
      6: result:='six';
      7: result:='seven';
      8: result:='eight';
      9: result:='nine';
      10: result:='ten';
      11: result:='eleven';
      12: result:='twelve';
      13: result:='thirdteen';
      14: result:='fourteen';
      15: result:='fifteen';
      16: result:='sixteen';
      17: result:='seventeen';
      18: result:='eightteen';
      19: result:='nineteen';
    end;
  end;

  function ZahlBis99(za: LongInt): string;
  begin
    case za of
      1..19: result:=ZahlBis19(za);
      20: result:='twenty';
      21..29: result:='twenty-'+ZahlBis19(za-(za div 10)*10);
      30: result:='thirty';
      31..39: result:='thirty-'+ZahlBis19(za-(za div 10)*10);
      40: result:='forty';
      41..49: result:='forty-'+ZahlBis19(za-(za div 10)*10);
      50: result:='fifty';
      51..59: result:='fifty-'+ZahlBis19(za-(za div 10)*10);
      60 :result:='sixty';
      61..69: result:='sixty-'+ZahlBis19(za-(za div 10)*10);
      70: result:='seventy';
      71..79: result:='seventy-'+ZahlBis19(za-(za div 10)*10);
      80: result:='eighty';
      81..89: result:='eighty-'+ZahlBis19(za-(za div 10)*10);
      90: result:='ninety';
      91..99: result:='ninety-'+ZahlBis19(za-(za div 10)*10);
    end;
  end;

  function ZahlBis999(za: LongInt): string;
  begin
    case za of
      0..99: result:=zahlbis99(za);
      100: result:='one hundred';
      101..199: result:='one hundred and '+Zahlbis99(za-100);
      200: result:='two hundred';
      201..299: result:='two hundred and '+Zahlbis99(za-200);
      300: result:='three hundred';
      301..399: result:='three hundred '+Zahlbis99(za-300);
      400: result:='four hundred';
      401..499: result:='four hundred '+Zahlbis99(za-400);
      500: result:='five hundred';
      501..599: result:='five hundred and '+Zahlbis99(za-500);
      600: result:='six hundred';
      601..699: result:='six hundred and '+Zahlbis99(za-600);
      700: result:='seven hundred';
      701..799: result:='seven hundred and '+Zahlbis99(za-700);
      800: result:='eight hundred';
      801..899: result:='eight hundred and '+Zahlbis99(za-800);
      900: result:='nine hundred';
      901..999: result:='nine hundred and '+Zahlbis99(za-900);
    end;
  end;

var za:longInt;
begin
  result:='';
  if z<0 then
  begin
    result:='minus ';
    z:=abs(z);
  end;
  if z=1 then
  begin
    Result:=result+'one';
    exit;
  end;
  za:=z;
  if za div 1000000000 > 0 then
  begin
    if za div 1000000000=1 then
      result:='one billion '
    else
      result:=Zahlbis999(za div 1000000000)+' billion ';
    za:=za - (za div 1000000000)*1000000000;
  end;
  if za div 1000000 > 0 then
  begin
    if za div 1000000=1 then
      result:=result +'one million '
    else
      result:=result+Zahlbis999(za div 1000000)+' million ';
    za:=za - (za div 1000000)*1000000;
  end;
  if za div 1000 > 0 then
  begin
    if za div 1000=1 then
      result:=result +'one thousend '
    else
      result:=result+Zahlbis999(za div 1000)+' thousend ';
    za:=za - (za div 1000)*1000;
  end;
  if za > 0 then
    result:=result+Zahlbis999(za)+' ';
end;


end.
