unit StrSearch;

interface

function Like(const AString, APattern: String): Boolean;
function MatchPattern(InpStr,Pattern :PChar) :Boolean;


implementation

{ Like prüft die Übereinstimmung eines Strings mit einem Muster. 
  So liefert Like('Delphi', 'D*p?i') true. 
  Der Vergleich berücksichtigt Klein- und Großschreibung. 
  Ist das nicht gewünscht, muss statt dessen 
  Like(AnsiUpperCase(AString), AnsiUpperCase(APattern)) benutzt werden: }

function Like(const AString, APattern: String): Boolean;
var
  StringPtr, PatternPtr: PChar;
  StringRes, PatternRes: PChar;
begin
  Result:=false;
  StringPtr:=PChar(AString);
  PatternPtr:=PChar(APattern);
  StringRes:=nil;
  PatternRes:=nil;
  repeat
    repeat // ohne vorangegangenes "*"
      case PatternPtr^ of
        #0: begin
          Result:=StringPtr^=#0;
          if Result or (StringRes=nil) or (PatternRes=nil) then
            Exit;
          StringPtr:=StringRes;
          PatternPtr:=PatternRes;
          Break;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
          Break;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          if StringPtr^=#0 then
            Exit;
          if StringPtr^<>PatternPtr^ then begin
            if (StringRes=nil) or (PatternRes=nil) then
              Exit;
            StringPtr:=StringRes;
            PatternPtr:=PatternRes;
            Break;
          end
          else begin
            inc(StringPtr);
            inc(PatternPtr);
          end;
        end;
      end;
    until false;
    repeat // mit vorangegangenem "*"
      case PatternPtr^ of
        #0: begin
          Result:=true;
          Exit;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          repeat
            if StringPtr^=#0 then
              Exit;
            if StringPtr^=PatternPtr^ then
              Break;
            inc(StringPtr);
          until false;
          inc(StringPtr);
          StringRes:=StringPtr;
          inc(PatternPtr);
          Break;
        end;
      end;
    until false;
  until false;
end; {Michael Winter}



{*****************************************************************}
{* This function implements a subset of regular expression based *}
{* search and is based on the translation of PattenMatch() API   *}
{* of common.c in MSDN Samples\VC98\sdk\sdktools\tlist           *}
{*****************************************************************}
{* MetaChars are  :                                              *}
{*            '*' : Zero or more chars.                          *}
{*            '?' : Any one char.                                *}
{*         [adgj] : Individual chars (inclusion).                *}
{*        [^adgj] : Individual chars (exclusion).                *}
{*          [a-d] : Range (inclusion).                           *}
{*         [^a-d] : Range (exclusion).                           *}
{*       [a-dg-j] : Multiple ranges (inclusion).                 *}
{*      [^a-dg-j] : Multiple ranges (exclusion).                 *}
{*  [ad-fhjnv-xz] : Mix of range & individual chars (inclusion). *}
{* [^ad-fhjnv-xz] : Mix of range & individual chars (exclusion). *}
{*****************************************************************}

function MatchPattern(InpStr,Pattern :PChar) :Boolean;
begin
  result := true;
  while (True) do begin
    case Pattern[0] of
      #0 :begin
            //End of pattern reached.
            Result := (InpStr[0] = #0); //TRUE if end of InpStr.
            Exit;
          end;
      '*':begin //Match zero or more occurances of any char.
            if (Pattern[1] = #0) then begin
              //Match any number of trailing chars.
              Result := True;
              Exit;
            end
            else
              Inc(Pattern);
            while (InpStr[0] <> #0) do begin
              //Try to match any substring of InpStr.
              if (MatchPattern(InpStr,Pattern)) then begin
                Result := True;
                Exit;
              end;
              //Continue testing next char...
              Inc(InpStr);
            end;
          end;
      '?':begin //Match any one char.
            if (InpStr[0] = #0) then begin
              Result := False;
              Exit;
            end;
            //Continue testing next char...
            Inc(InpStr);
            Inc(Pattern);
          end;
      '[':begin //Match given set of chars.
            if (Pattern[1] in [#0,'[',']']) then begin
              //Invalid Set - So no match.
              Result := False;
              Exit;
            end;
            if (Pattern[1] = '^') then begin
              //Match for exclusion of given set...
              Inc(Pattern,2);
              Result := True;
              while (Pattern[0] <> ']') do begin
                if (Pattern[1] = '-') then begin
                  //Match char exclusion range.
                  if (InpStr[0] >= Pattern[0]) and (InpStr[0] <= Pattern[2]) then begin
                    //Given char failed set exclusion range.
                    Result := False; 
                    Break; 
                  end
                  else
                    Inc(Pattern,3);
                end
                else begin
                  //Match individual char exclusion.
                  if (InpStr[0] = Pattern[0]) then begin
                    //Given char failed set element exclusion.
                    Result := False;
                    Break;
                  end
                  else
                    Inc(Pattern);
                end;
              end;
            end
            else begin
              //Match for inclusion of given set...
              Inc(Pattern);
              Result := False;
              while (Pattern[0] <> ']') do begin
                if (Pattern[1] = '-') then begin
                  //Match char inclusion range.
                  if (InpStr[0] >= Pattern[0])and(InpStr[0] <= Pattern[2]) then begin
                    //Given char matched set range inclusion.
                    // Continue testing...
                    Result := True;
                    Break;
                  end
                  else
                    Inc(Pattern,3);
                end
                else begin
                  //Match individual char inclusion.
                  if (InpStr[0] = Pattern[0]) then begin
                    //Given char matched set element inclusion.
                    // Continue testing...
                    Result := True;
                    Break;
                  end
                  else
                    Inc(Pattern);
                end;
              end;
            end;
            if (Result) then begin
              //Match was found. Continue further.
              Inc(InpStr);
              //Position Pattern to char after "]"
              while (Pattern[0] <> ']')and(Pattern[0] <> #0) do
                Inc(Pattern);
              if (Pattern[0] = #0) then begin
                //Invalid Pattern - missing "]"
                Result := False;
                Exit;
              end
              else
                Inc(Pattern);
            end
            else
              Exit;
          end;
     else begin //Match given single char.
            if (InpStr[0] <> Pattern[0]) then begin
              Result := False;
              Break;
            end;
            //Continue testing next char...
            Inc(InpStr);
            Inc(Pattern);
          end;
    end;
  end;
end;

end.
 