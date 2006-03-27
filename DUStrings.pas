unit DUStrings;

interface

uses Sysutils;

type TStringArray = array of ShortString;

function  Cut   (var  s : string; index, count : Integer):string;
function CopyTo(S,Subst:string; Start:integer=1):string;
function CopyFrom(S,Subst:string; Count:integer=MaxInt):string;
function CopySubst(S,SubStart,SubEnd:string):string;

function SplitString (s:string; size:integer):TStringArray;
function ConcatString(parts:TStringArray):string;
function FetchToken(var s : string; delim : Char) : string;

function QuotedString(const S: string; Quote: Char): string;
function ExtractQuotedString(const S: string; Quote: Char): string;

function CompareText(S1,S2:String; IgnoreCase:boolean=true):integer;
function IsSameText(S1,S2:String; IgnoreCase:boolean=true):boolean;

procedure ConvertSpecials(var S:String);

implementation

function Cut   (var  s : string; index, count : Integer):string;
begin
  Result:= Copy(s,index,count);
  Delete(s,index,count);
end;

function CopyTo(S,Subst:string; Start:integer=1):string;
begin
  Result:= copy(S,Start,pos(Subst,S)-Start);
end;

function CopyFrom(S,Subst:string; Count:integer=MaxInt):string;
begin
  Result:= copy(S,pos(Subst,S)+1,Count);
end;

function CopySubst(S,SubStart,SubEnd:string):string;
begin
  Result:= CopyFrom(S,SubStart);
  Result:= CopyTo(Result,SubEnd);
end;

function SplitString(s:string; size:integer):TStringArray;
var c:integer;
begin
  SetLength(result,0);
  c:= 0;
  while Length(s)> size do begin
    SetLength(result,c+1);
    Result[c]:= Cut(s,1,size);
    inc(c);
  end;
  SetLength(result,c+1);
  Result[c]:= Cut(s,1,size);
end;

function ConcatString(parts:TStringArray):string;
var i:integer;
begin
  Result:='';
  for i:= 0 to high(parts) do
    Result:= Result+parts[i];
end;

  // Fetch a token and delete it from string:
function FetchToken(var s : string; delim : Char) : string;
var i : integer;
    L : integer;
begin
   L := Length(s);
   Result := '';
   i := 1;
   while (i <= L) and not (s[i]=delim) do
   begin
     Result := Result + s[i];
     Inc(I);
   end;
   // skip delims
   while (i <= L) and (s[i] = delim) do
     Inc(i);
   s := Copy(s, i, L-i+1);
end;

function QuotedString(const S: string; Quote: Char): string;
begin
  Result := AnsiQuotedStr(S, Quote);
end;

function ExtractQuotedString(const S: string; Quote: Char): string;
var
  P: PChar;
begin
  P := PChar(S);
  if P^ = Quote then
    Result := AnsiExtractQuotedStr(P, Quote)
  else
    Result := S;    
end;

function CompareText(S1,S2:String; IgnoreCase:boolean=true):integer;
begin
  if IgnoreCase then Result:= AnsiCompareText(S1,S2) else
    Result:= AnsiCompareStr(S1,S2);
end;

function IsSameText(S1,S2:String; IgnoreCase:boolean=true):boolean;
begin Result:= CompareText(S1,S2,IgnoreCase)=0; end;


procedure ConvertSpecials(var S:String);
begin
  s:= StringReplace(s,' /t ', #9,[rfReplaceAll]);
  s:= StringReplace(s,' /n ',#13,[rfReplaceAll]);
  s:= StringReplace(s,' // ','/',[rfReplaceAll]);
end;

end.
 