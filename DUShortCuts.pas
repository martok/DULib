//////////////////////////////////////////////////////////////////
//
//  This Procedures were copied from Menus.pas.
//  Menus.pas is part of the
//  {*******************************************************}
//  {       Borland Delphi Visual Component Library         }
//  {       Copyright (c) 1995,98 Inprise Corporation       }
//  {*******************************************************}
//
//  This Unit was made to handle shortcuts without using all the
//  Menus unit.
//
//////////////////////////////////////////////////////////////////



unit DUShortCuts;

interface

uses Classes,SysUtils, Consts, Windows;

type
  TMenuKeyCap = (mkcBkSp, mkcTab, mkcEsc, mkcEnter, mkcSpace, mkcPgUp,
    mkcPgDn, mkcEnd, mkcHome, mkcLeft, mkcUp, mkcRight, mkcDown, mkcIns,
    mkcDel, mkcShift, mkcCtrl, mkcAlt);

function ShortCut(Key: Word; Shift: TShiftState): TShortCut;
procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
function GetSpecialName(ShortCut: TShortCut): string;
function ShortCutToText(ShortCut: TShortCut): string;
function TextToShortCut(Text: string): TShortCut;


implementation


function ShortCut(Key: Word; Shift: TShiftState): TShortCut;
begin
  Result := 0;
  if WordRec(Key).Hi <> 0 then Exit;
  Result := Key;
  if ssShift in Shift then Inc(Result, scShift);
  if ssCtrl in Shift then Inc(Result, scCtrl);
  if ssAlt in Shift then Inc(Result, scAlt);
end;

procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  Key := ShortCut and not (scShift + scCtrl + scAlt);
  Shift := [];
  if ShortCut and scShift <> 0 then Include(Shift, ssShift);
  if ShortCut and scCtrl <> 0 then Include(Shift, ssCtrl);
  if ShortCut and scAlt <> 0 then Include(Shift, ssAlt);
end;


var
  MenuKeyCaps: array[TMenuKeyCap] of string = (
    SmkcBkSp, SmkcTab, SmkcEsc, SmkcEnter, SmkcSpace, SmkcPgUp,
    SmkcPgDn, SmkcEnd, SmkcHome, SmkcLeft, SmkcUp, SmkcRight,
    SmkcDown, SmkcIns, SmkcDel, SmkcShift, SmkcCtrl, SmkcAlt);

function GetSpecialName(ShortCut: TShortCut): string;
var
  ScanCode: Integer;
  KeyName: array[0..255] of Char;
begin
  Result := '';
  ScanCode := MapVirtualKey(WordRec(ShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameText(ScanCode, KeyName, SizeOf(KeyName));
    if (KeyName[1] = #0) and (KeyName[0] <> #0) then
      GetSpecialName := KeyName;
  end;
end;

function ShortCutToText(ShortCut: TShortCut): string;
var
  Name: string;
begin
  case WordRec(ShortCut).Lo of
    $08, $09:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcBkSp) + WordRec(ShortCut).Lo - $08)];
    $0D: Name := MenuKeyCaps[mkcEnter];
    $1B: Name := MenuKeyCaps[mkcEsc];
    $20..$28:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcSpace) + WordRec(ShortCut).Lo - $20)];
    $2D..$2E:
      Name := MenuKeyCaps[TMenuKeyCap(Ord(mkcIns) + WordRec(ShortCut).Lo - $2D)];
    $30..$39: Name := Chr(WordRec(ShortCut).Lo - $30 + Ord('0'));
    $41..$5A: Name := Chr(WordRec(ShortCut).Lo - $41 + Ord('A'));
    $60..$69: Name := Chr(WordRec(ShortCut).Lo - $60 + Ord('0'));
    $70..$87: Name := 'F' + IntToStr(WordRec(ShortCut).Lo - $6F);
  else
    Name := GetSpecialName(ShortCut);
  end;
  if Name <> '' then
  begin
    Result := '';
    if ShortCut and scShift <> 0 then Result := Result + MenuKeyCaps[mkcShift];
    if ShortCut and scCtrl <> 0 then Result := Result + MenuKeyCaps[mkcCtrl];
    if ShortCut and scAlt <> 0 then Result := Result + MenuKeyCaps[mkcAlt];
    Result := Result + Name;
  end
  else Result := '';
end;

{ This function is *very* slow.  Use sparingly.  Return 0 if no VK code was
  found for the text }

function TextToShortCut(Text: string): TShortCut;

  { If the front of Text is equal to Front then remove the matching piece
    from Text and return True, otherwise return False }

  function CompareFront(var Text: string; const Front: string): Boolean;
  begin
    Result := False;
    if (Length(Text) >= Length(Front)) and
      (AnsiStrLIComp(PChar(Text), PChar(Front), Length(Front)) = 0) then
    begin
      Result := True;
      Delete(Text, 1, Length(Front));
    end;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
begin
  Result := 0;
  Shift := 0;
  while True do
  begin
    if CompareFront(Text, MenuKeyCaps[mkcShift]) then Shift := Shift or scShift
    else if CompareFront(Text, '^') then Shift := Shift or scCtrl
    else if CompareFront(Text, MenuKeyCaps[mkcCtrl]) then Shift := Shift or scCtrl
    else if CompareFront(Text, MenuKeyCaps[mkcAlt]) then Shift := Shift or scAlt
    else Break;
  end;
  if Text = '' then Exit;
  for Key := $08 to $255 do { Copy range from table in ShortCutToText }
    if AnsiCompareText(Text, ShortCutToText(Key)) = 0 then
    begin
      Result := Key or Shift;
      Exit;
    end;
end;

end.
 