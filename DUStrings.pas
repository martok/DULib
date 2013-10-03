{-----------------------------------------------------------------------------
 Unit Name: DUStrings
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   String processing routines like Fetchtoke, Splitstring, div. Copy routines
            String comparison routines
 History:   2006-08-01 initial release
            2006-08-29 Added PosNth, Extract, PosEx(from StrUtils.pas)
-----------------------------------------------------------------------------}
unit DUStrings;

interface

uses Sysutils;

type
  TStringArray = array of ShortString;
  TCharSet = set of char;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
function PosNth(const SubStr, S: string; N: integer): Integer;
function PosRightNth(const SubStr, S: string; N: integer): Integer;
function Cut(var s: string; index, count: Integer): string;
function CopyTo(S, Subst: string; Start: integer = 1): string;
function CopyFrom(S, Subst: string; Count: integer = MaxInt): string;
function CopySubst(S, SubStart, SubEnd: string): string;
function CopyEx(S: string; I, Count: integer): string;
function Extract(S, Start, EndText: string; num: integer; IgnoreCase: boolean): string;

function SplitString(s: string; size: integer): TStringArray; overload;
function SplitString(s: string; delim: char; SkipDelims: boolean = false): TStringArray; overload;
function SplitStringQuoted(s: string; delim, quote: char): TStringArray;
function StrAGetVal(P: TStringArray; Name: string): string;
function StrAFind(P: TStringArray; Elem: string; IgnoreCase: boolean = true): integer;
function StrANew(Proto: array of string): TStringArray;
procedure StrAClear(var A: TStringArray);
procedure StrADelete(var A: TStringArray; Index: integer);
procedure StrAAdd(var A: TStringArray; S: string);
procedure StrAInsert(var A: TStringArray; S: string; Index: integer);
procedure StrATrim(var A: TStringArray);
procedure StrAReverse(var A: TStringArray);
function StrAPos(const A: TStringArray; Needle: string; Partial, IgnoreCase: boolean): integer;
function ConcatString(parts: TStringArray; Sep: string = ''): string;

function FetchToken(var s: string; delim: Char; SkipDelims: boolean = true): string;

function KillString(Substr, Str: string): string;
function KillChars(S: string; Chars: TCharSet): string;

function QuotedString(const S: string; Quote: Char): string;
function ExtractQuotedString(const S: string; Quote: Char): string;

function RepeatString(S: string; Times: integer): string;
function RepeatChar(C: Char; Times: integer): string;
function CountChar(C: Char; S: string): integer;
function StringFillRight(const S: string; Len: integer; Fill: char = ' '): string;
procedure DeleteRight(var S: string; Num: integer);
function ReverseString(const S: string): string;

{ IfThen will return the true string if the value passed in is true, else
  it will return the false string }

function IfThen(AValue: Boolean; const ATrue: string;
  AFalse: string = ''): string; overload;

function CaseStr(Value: string; Args: array of string; Sensitive: Boolean): Integer;

function CompareText(S1, S2: string; IgnoreCase: boolean = true): integer;
function IsSameText(S1, S2: string; IgnoreCase: boolean = true): boolean;
function StartsWith(S1, S2: string; IgnoreCase: boolean = true): boolean;
function EndsWith(S1, S2: string; IgnoreCase: boolean = true): boolean;

function ConvertSpecials(S: string): string;
// always with & an ;, eg. Str:= '&amp;'
function IsHTMLEntity(Str: string): boolean;
function StripTags(Input: string): string;
function NiceCase(const Str: string): string;

const
  Small_Letters = ['a'..'z'];
  Capitals = ['A'..'Z'];
  Letters = Small_Letters + Capitals;
  Numbers = ['0'..'9'];
  AlphaNum = Letters + Numbers;

implementation

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result:= Pos(SubStr, S)
  else begin
    I:= Offset;
    LenSubStr:= Length(SubStr);
    Len:= Length(S) - LenSubStr + 1;
    while I <= Len do begin
      if S[I] = SubStr[1] then begin
        X:= 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then begin
          Result:= I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result:= 0;
  end;
end;

function PosNth(const SubStr, S: string; N: integer): Integer;
var
  i, f: integer;
begin
  Result:= 0;
  if N < 1 then
    exit
  else if N = 1 then
    Result:= pos(SubStr, S)
  else begin
    f:= 0;
    for i:= 1 to length(s) - Length(SubStr) do
      if copy(S, i, length(SubStr)) = SubStr then begin
        inc(f);
        if f = N then
          break;
      end;
    if f = N then
      Result:= i;
  end;
end;

function PosRightNth(const SubStr, S: string; N: integer): Integer;
var
  i, f: integer;
begin
  Result:= 0;
  if N < 1 then
    exit
  else begin
    f:= 0;
    for i:= length(s) - Length(SubStr) + 1 downto 1 do
      if copy(S, i, length(SubStr)) = SubStr then begin
        inc(f);
        if f = N then
          break;
      end;
    if f = N then
      Result:= length(s) - i + 1;
  end;
end;

function Cut(var s: string; index, count: Integer): string;
begin
  Result:= Copy(s, index, count);
  Delete(s, index, count);
end;

function CopyTo(S, Subst: string; Start: integer = 1): string;
begin
  if Pos(Subst, S) > 0 then
    Result:= copy(S, Start, posex(Subst, S, Start) - Start)
  else
    Result:= copy(S, Start, Maxint);
end;

function CopyFrom(S, Subst: string; Count: integer = MaxInt): string;
var
  i: integer;
begin
  i:= pos(Subst, S);
  if i > 0 then
    Result:= copy(S, i + length(Subst), Count)
  else
    Result:= '';
end;

function CopySubst(S, SubStart, SubEnd: string): string;
begin
  Result:= CopyFrom(S, SubStart);
  Result:= CopyTo(Result, SubEnd);
end;

function CopyEx(S: string; I, Count: integer): string;
begin
  if I < 0 then
    Result:= Copy(S, length(S) + I + 1, Count)
  else begin
    if Count < 0 then
      Result:= Copy(S, I, length(S) + Count)
    else
      Result:= copy(S, I, Count);
  end;
end;

function Extract(S, Start, EndText: string; num: integer; IgnoreCase: boolean): string;
var
  f, j: integer;
begin
  if num < 1 then
    exit;
  if IgnoreCase then begin
    s:= LowerCase(S);
    Start:= LowerCase(Start);
    EndText:= LowerCase(EndText);
  end;
  f:= PosNth(Start, S, num);
  j:= f + length(Start);
  if f > 0 then
    Result:= copy(S, j, posex(EndText, S, j) - j);
end;

function SplitString(s: string; size: integer): TStringArray;
var
  c: integer;
begin
  StrAClear(Result);
  c:= 0;
  while Length(s) > size do begin
    SetLength(result, c + 1);
    Result[c]:= Cut(s, 1, size);
    inc(c);
  end;
  SetLength(result, c + 1);
  Result[c]:= Cut(s, 1, size);
end;

function SplitString(s: string; delim: char; SkipDelims: boolean = false): TStringArray; overload;
var
  a: string;
begin
  StrAClear(Result);
  while s > '' do begin
    a:= fetchToken(s, delim, SkipDelims);
    StrAAdd(Result, a);
  end;
end;

function SplitStringQuoted(s: string; delim, quote: char): TStringArray;
var
  i: integer;
  a: string;
  inq: boolean;
begin
  StrAClear(Result);
  inq:= false;
  for i:= 1 to length(s) do begin
    if inq then begin
      if s[i] = quote then
        inq:= false
      else
        a:= a + s[i];
    end else begin
      if s[i] = quote then
        inq:= true
      else if s[i] = delim then begin
        StrAAdd(Result, a);
        a:= '';
      end else
        a:= a + s[i];
    end;
  end;
  if a > '' then
    StrAAdd(Result, a);
end;

function StrAGetVal(P: TStringArray; Name: string): string;
var
  i: integer;
begin
  for i:= 0 to high(p) do
    if CopyTo(p[i], '=') = Name then begin
      Result:= CopyFrom(p[i], '=');
      exit
    end;
end;

function StrAFind(P: TStringArray; Elem: string; IgnoreCase: boolean): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to high(p) do
    if IsSameText(P[i], Elem, IgnoreCase) then begin
      Result:= i;
      exit;
    end;
end;

function StrANew(Proto: array of string): TStringArray;
var
  i: integer;
begin
  SetLength(Result, length(Proto));
  for i:= 0 to high(Proto) do
    Result[i]:= Proto[i];
end;

procedure StrAClear(var A: TStringArray);
begin
  SetLength(A, 0);
end;

procedure StrATrim(var A: TStringArray);
var
  I: integer;
begin
  for i:= 0 to High(A) do
    A[i]:= trim(A[i]);
end;

procedure StrAReverse(var A: TStringArray);
var
  d: string;
  i: integer;
begin
  if length(a) < 2 then
    exit;
  for i:= 0 to high(a) div 2 do begin
    d:= A[i];
    A[i]:= A[high(A) - i];
    A[high(A) - i]:= d;
  end;
end;

procedure StrADelete(var A: TStringArray; Index: integer);
var
  I: integer;
begin
  for i:= index to High(A) - 1 do
    A[i]:= A[i + 1];
  SetLength(A, length(A) - 1);
end;

procedure StrAAdd(var A: TStringArray; S: string);
begin
  SetLength(A, Length(A) + 1);
  A[High(A)]:= S;
end;

procedure StrAInsert(var A: TStringArray; S: string; Index: integer);
var
  i: integer;
begin
  if Index > high(A) then
    StrAAdd(A, S)
  else begin
    SetLength(A, length(A) + 1);
    for i:= high(A) downto Index + 1 do
      A[i]:= A[i - 1];
    A[Index]:= S;
  end;
end;

function StrAPos(const A: TStringArray; Needle: string; Partial, IgnoreCase: boolean): integer;
var
  i: integer;
begin
  Result:= -1;
  if IgnoreCase then begin
    Needle:= LowerCase(Needle);
    if Partial then begin
      for i:= 0 to high(A) do
        if Pos(Needle, LowerCase(A[i])) > 0 then begin
          Result:= i;
          exit;
        end;
    end else begin
      for i:= 0 to high(A) do
        if Needle = LowerCase(A[i]) then begin
          Result:= i;
          exit;
        end;
    end;
  end else if Partial then begin
    for i:= 0 to high(A) do
      if Pos(Needle, A[i]) > 0 then begin
        Result:= i;
        exit;
      end;
  end else begin
    for i:= 0 to high(A) do
      if Needle = A[i] then begin
        Result:= i;
        exit;
      end;
  end;
end;

function ConcatString(parts: TStringArray; Sep: string = ''): string;
var
  i: integer;
begin
  Result:= '';
  if Length(Parts) = 0 then
    exit;
  for i:= 0 to high(parts) - 1 do
    Result:= Result + parts[i] + Sep;
  Result:= Result + parts[high(parts)];
end;

// Fetch a token and delete it from string:

function FetchToken(var s: string; delim: Char; SkipDelims: boolean = true): string;
var
  i: integer;
  L: integer;
begin
  L:= Length(s);
  Result:= '';
  i:= 1;
  while (i <= L) and not (s[i] = delim) do begin
    Result:= Result + s[i];
    Inc(I);
  end;
  // skip delims
  if SkipDelims then begin
    while (i <= L) and (s[i] = delim) do
      Inc(i);
  end else
    inc(i);
  s:= Copy(s, i, L - i + 1);
end;

function KillString(Substr, Str: string): string;
begin
  Result:= StringReplace(Str, Substr, '', [rfReplaceAll]);
end;

function KillChars(S: string; Chars: TCharSet): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 1 to length(S) do
    if not (S[i] in Chars) then
      Result:= Result + S[i];
end;

function QuotedString(const S: string; Quote: Char): string;
begin
  Result:= AnsiQuotedStr(S, Quote);
end;

function ExtractQuotedString(const S: string; Quote: Char): string;
var
  P: PChar;
begin
  P:= PChar(S);
  if P^ = Quote then
    Result:= AnsiExtractQuotedStr(P, Quote)
  else
    Result:= S;
end;

function RepeatChar(C: Char; Times: integer): string;
begin
  SetLength(Result, Times);
  FillChar(Result[1], Times, C);
end;

function CountChar(C: Char; S: string): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to length(s) do
    if S[i] = C then
      inc(Result);
end;

function RepeatString(S: string; Times: integer): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 1 to Times do
    Result:= Result + S;
end;

function StringFillRight(const S: string; Len: integer; Fill: char = ' '): string;
begin
  if Length(S) >= Len then
    Result:= S
  else
    Result:= S + RepeatChar(Fill, len - length(S));
end;

procedure DeleteRight(var S: string; Num: integer);
begin
  SetLength(S, length(S) - Num);
end;

function ReverseString(const S: string): string;
var
  i: integer;
begin
  SetLength(Result, Length(S));
  for i:= 1 to length(S) do
    Result[Length(S) + 1 - i]:= S[i];
end;

function IfThen(AValue: Boolean; const ATrue: string;
  AFalse: string = ''): string;
begin
  if AValue then
    Result:= ATrue
  else
    Result:= AFalse;
end;

function CaseStr(Value: string; Args: array of string; Sensitive: Boolean): Integer;
begin
  if Sensitive then begin
    for Result:= High(Args) downto 0 do
      if Value = Args[Result] then
        Break;
  end else begin
    for Result:= High(Args) downto 0 do
      if CompareText(Value, Args[Result]) = 0 then
        Break;
  end;
end;

function CompareText(S1, S2: string; IgnoreCase: boolean = true): integer;
begin
  if IgnoreCase then
    Result:= AnsiCompareText(S1, S2)
  else
    Result:= AnsiCompareStr(S1, S2);
end;

function IsSameText(S1, S2: string; IgnoreCase: boolean = true): boolean;
begin
  Result:= CompareText(S1, S2, IgnoreCase) = 0;
end;

function StartsWith(S1, S2: string; IgnoreCase: boolean = true): boolean;
begin
  Result:= IsSameText(copy(S1, 1, length(S2)), S2, Ignorecase);
end;

function EndsWith(S1, S2: string; IgnoreCase: boolean = true): boolean;
begin
  Result:= IsSameText(copy(S1, length(S1) - length(S2) + 1, length(S2)), S2, Ignorecase);
end;

function ConvertSpecials(S: string): string;
var
  i: integer;
begin
  i:= 1;
  while i <= length(s) do begin
    if (s[i] = '\') and (i < length(s)) then begin
      case s[i + 1] of
        't': Result:= Result + #9;
        'r': Result:= Result + #10;
        'n': Result:= Result + #13;
        's': Result:= Result + ' ';
        '\': Result:= Result + '\';
      end;
      inc(i);
    end else
      Result:= Result + S[i];
    inc(i);
  end;
end;

function IsHTMLEntity(Str: string): boolean;
var
  i: integer;
begin
  Result:= (length(Str) > 3) and                            // &lt; is 4
  (Str[1] = '&') and (Str[length(str)] = ';');
  if Result then
    for i:= 2 to length(Str) - 1 do
      if not (Str[i] in AlphaNum) then begin
        Result:= false;
        exit
      end;
end;

function StripTags(Input: string): string;
var
  p, c: integer;
begin
  c:= 0;
  result:= '';
  for p:= 1 to Length(Input) do begin
    case Input[p] of
      '<': inc(c);
      '>': dec(c);
    else if (c = 0) then
      result:= result + Input[p];
    end;
  end;
end;

{--------------------------------------------------------------------------}
{ Found in SF project 'watergage' as Trans_NiceCaseName                    }
{ slightly modified & extended version                                     }
{                                                                          }
{ This routine takes a name and puts capitals in it to make it look good,  }
{ but avoids changing already capitalised names.                           }
{                                                                          }
{ Examples: diane white     --> Diane White                                }
{           DIANE WHITE     --> Diane White                                }
{           Ramon vd Winkel --> Ramon vd Winkel                            }
{                                                                          }

function NiceCase(const Str: string): string;
var
  i: integer;

begin
  { check for all upper case }
  if (Str = UpperCase(Str)) then
    Result:= LowerCase(Str)
  else begin
    Result:= Str;
    { does the name contain a single upper case letter? }
    for i:= 1 to Length(Str) do
      if (Str[i] in ['A'..'Z']) then begin
        Exit;                                               { as-is }
      end;
  end;

  { everything must be lower case }
  { change chars following a space to a capital }

  for i:= 2 to length(Result) do
    if Result[i - 1] in ['-', ' '] then
      Result[i]:= UpCase(Result[i]);

  { first char to upper as well }
  Result[1]:= UpCase(Result[1]);
end;

end.

