unit HTMLHandler;

interface

uses Windows, classes,sysutils, WinInet;

function GetTempFolder: String;

Function GetHTML(AUrl: string): string;
Function GiveSZ (HCode : String) : Char;
procedure HTML2TextFile(InFile, OutFile : String);
procedure HTML2Text(Html: String; var Plain : String);

implementation

function GetTempFolder: String;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, Buffer);
  Result := Buffer;
  if Result[Length(Result)]<>'\' then Result:= Result+'\';
end;

Function GetHTML(AUrl: string): string;
var
  databuffer : array[0..4095] of char;
  ResStr : string;
  hSession, hfile: hInternet;
  dwindex,dwcodelen,datalen,dwread,dwNumber: cardinal;
  dwcode : array[1..20] of char;
  res    : pchar;
  Str    : pchar;
begin
  ResStr:='';
  if pos('http://',lowercase(AUrl))=0 then
     AUrl:='http://'+AUrl;
  hSession:=InternetOpen('InetURL:/1.0',
                         INTERNET_OPEN_TYPE_PRECONFIG,
                         nil,
                         nil,
                         0);
  if assigned(hsession) then
  begin
    hfile:=InternetOpenUrl(
           hsession,
           pchar(AUrl),
           nil,
           0,
           INTERNET_FLAG_RELOAD,
           0);
    dwIndex  := 0;
    dwCodeLen := 10;
    HttpQueryInfo(hfile,
                  HTTP_QUERY_STATUS_CODE,
                  @dwcode,
                  dwcodeLen,
                  dwIndex);
    res := pchar(@dwcode);
    dwNumber := sizeof(databuffer)-1;
    if (res ='200') or (res ='302') then
    begin
      while (InternetReadfile(hfile,
                              @databuffer,
                              dwNumber,
                              DwRead)) do
      begin
        if dwRead =0 then
          break;
        databuffer[dwread]:=#0;
        Str := pchar(@databuffer);
        resStr := resStr + Str;
      end;
    end
    else
      ResStr := 'Status:'+res;
    if assigned(hfile) then
      InternetCloseHandle(hfile);
  end;
  InternetCloseHandle(hsession);
  Result := resStr;
end;

Function GiveSZ (HCode : String) : Char;
Begin
  Result := ' ';
  if (HCode='&quot;') or (HCode = '&#34;')   then Result := '"';
  if (HCode='&amp;')  or (HCode = '&#38;')   then Result := '&';
  if (HCode='&lt;')   or (HCode = '&#60;')   then Result := '<';
  if (HCode='&gt;')   or (HCode = '&#62;')   then Result := '>';

  // ISO 160 bis ISO 255 Codes
  if (HCode='&nbsp;')   or (HCode = '&#160;')      then Result := ' ';
  if (HCode='&iexl;')   or (HCode = '&#161;')      then Result := '�';
  if (HCode='&cent;')   or (HCode = '&#162;')      then Result := '�';
  if (HCode='&pound;')  or (HCode = '&#163;')      then Result := '�';
  if (HCode='&curren;') or (HCode = '&#164;')      then Result := '�';
  if (HCode='&yen;')    or (HCode = '&#165;')      then Result := '�';
  if (HCode='&brkbar;') or (HCode = '&#166;')      then Result := '�';
  if (HCode='&sect;')   or (HCode = '&#167;')      then Result := '�';
  if (HCode='&uml;')    or (HCode = '&#168;')      then Result := '�';
  if (HCode='&copy;')   or (HCode = '&#169;')      then Result := '�';
  if (HCode='&ordf;')   or (HCode = '&#170;')      then Result := '�';
  if (HCode='&laquo;')  or (HCode = '&#171;')      then Result := '�';
  if (HCode='&not;')    or (HCode = '&#172;')      then Result := '�';
  if (HCode='&shy;')    or (HCode = '&#173;')      then Result := '�';
  if (HCode='&reg;')    or (HCode = '&#174;')      then Result := '�';
  if (HCode='&hibar;')  or (HCode = '&#175;')      then Result := '�';
  if (HCode='&deg;')    or (HCode = '&#176;')      then Result := '�';
  if (HCode='&plusmn;') or (HCode = '&#177;')      then Result := '�';
  if (HCode='&sup2;')   or (HCode = '&#178;')      then Result := '�';
  if (HCode='&sup3;')    or (HCode = '&#179;')      then Result := '�';
  if (HCode='&acute;')   or (HCode = '&#180;')      then Result := '�';
  if (HCode='&micro;')   or (HCode = '&#181;')      then Result := '�';
  if (HCode='&para;')    or (HCode = '&#182;')      then Result := '�';
  if (HCode='&middot;')  or (HCode = '&#183;')      then Result := '�';
  if (HCode='&cedil;')   or (HCode = '&#184;')      then Result := '�';
  if (HCode='&sup1;')    or (HCode = '&#185;')      then Result := '�';
  if (HCode='&ordm;')    or (HCode = '&#186;')      then Result := '�';
  if (HCode='&raquo;')   or (HCode = '&#187;')      then Result := '�';
  if (HCode='&frac14;')  or (HCode = '&#188;')      then Result := '�';
  if (HCode='&frac12;')  or (HCode = '&#189;')      then Result := '�';
  if (HCode='&frac34;')  or (HCode = '&#190;')      then Result := '�';
  if (HCode='&iquest;')  or (HCode = '&#191;')      then Result := '�';
  if (HCode='&Agrave;')  or (HCode = '&#192;')      then Result := '�';
  if (HCode='&Aacute;')  or (HCode = '&#193;')      then Result := '�';
  if (HCode='&Acirc;')   or (HCode = '&#194;')      then Result := '�';
  if (HCode='&Atilde;')  or (HCode = '&#195;')      then Result := '�';
  if (HCode='&Auml;')    or (HCode = '&#196;')      then Result := '�';
  if (HCode='&Aring;')   or (HCode = '&#197;')      then Result := '�';
  if (HCode='&AEling;')  or (HCode = '&#198;')      then Result := '�';
  if (HCode='&Ccedil;')  or (HCode = '&#199;')      then Result := '�';
  if (HCode='&Egrave;')  or (HCode = '&#200;')      then Result := '�';
  if (HCode='&Eacute;')  or (HCode = '&#201;')      then Result := '�';
  if (HCode='&Ecirce;')  or (HCode = '&#202;')      then Result := '�';
  if (HCode='&Euml;')    or (HCode = '&#203;')      then Result := '�';
  if (HCode='&Igrave;')  or (HCode = '&#204;')      then Result := '�';
  if (HCode='&Iacute;')  or (HCode = '&#205;')      then Result := '�';
  if (HCode='&Icirce;')  or (HCode = '&#206;')      then Result := '�';
  if (HCode='&Iuml;')    or (HCode = '&#207;')      then Result := '�';
  if (HCode='&ETH;')     or (HCode = '&#208;')      then Result := '�';
  if (HCode='&Ntilde;')  or (HCode = '&#209;')      then Result := '�';
  if (HCode='&Ograve;')  or (HCode = '&#210;')      then Result := '�';
  if (HCode='&Oacute;')  or (HCode = '&#211;')      then Result := '�';
  if (HCode='&Ocirc;')   or (HCode = '&#212;')      then Result := '�';
  if (HCode='&Otilde;')  or (HCode = '&#213;')      then Result := '�';
  if (HCode='&Ouml;')    or (HCode = '&#214;')      then Result := '�';
  if (HCode='&times;')   or (HCode = '&#215;')      then Result := '�';
  if (HCode='&Oslash;')  or (HCode = '&#216;')      then Result := '�';
  if (HCode='&Ugrave;')  or (HCode = '&#217;')      then Result := '�';
  if (HCode='&Uacute;')  or (HCode = '&#218;')      then Result := '�';
  if (HCode='&Ucirc;')   or (HCode = '&#219;')      then Result := '�';
  if (HCode='&Uuml;')    or (HCode = '&#220;')      then Result := '�';
  if (HCode='&Yacute;')  or (HCode = '&#221;')      then Result := '�';
  if (HCode='&THORN;')   or (HCode = '&#222;')      then Result := '�';
  if (HCode='&szlig;')   or (HCode = '&#223;')      then Result := '�';
  if (HCode='&agrave;')  or (HCode = '&#224;')      then Result := '�';
  if (HCode='&aacute;')  or (HCode = '&#225;')      then Result := '�';
  if (HCode='&acirc;')   or (HCode = '&#226;')      then Result := '�';
  if (HCode='&atilde;')  or (HCode = '&#227;')      then Result := '�';
  if (HCode='&auml;')    or (HCode = '&#228;')      then Result := '�';
  if (HCode='&aring;')   or (HCode = '&#229;')      then Result := '�';
  if (HCode='&aeling;')  or (HCode = '&#230;')      then Result := '�';
  if (HCode='&ccedil;')  or (HCode = '&#231;')      then Result := '�';
  if (HCode='&egrave;')  or (HCode = '&#232;')      then Result := '�';
  if (HCode='&eacute;')  or (HCode = '&#233;')      then Result := '�';
  if (HCode='&ecirc;')   or (HCode = '&#234;')      then Result := '�';
  if (HCode='&euml;')    or (HCode = '&#235;')      then Result := '�';
  if (HCode='&igrave;')  or (HCode = '&#236;')      then Result := '�';
  if (HCode='&iacute;')  or (HCode = '&#237;')      then Result := '�';
  if (HCode='&icirc;')   or (HCode = '&#238;')      then Result := '�';
  if (HCode='&iuml;')    or (HCode = '&#239;')      then Result := '�';
  if (HCode='&eth;')     or (HCode = '&#240;')      then Result := '�';
  if (HCode='&ntilde;')  or (HCode = '&#241;')      then Result := '�';
  if (HCode='&ograve;')  or (HCode = '&#242;')      then Result := '�';
  if (HCode='&oacute;')  or (HCode = '&#243;')      then Result := '�';
  if (HCode='&ocirc;')   or (HCode = '&#244;')      then Result := '�';
  if (HCode='&otilde;')  or (HCode = '&#245;')      then Result := '�';
  if (HCode='&ouml;')    or (HCode = '&#246;')      then Result := '�';
  if (HCode='&divide;')  or (HCode = '&#247;')      then Result := '�';
  if (HCode='&oslash;')  or (HCode = '&#248;')      then Result := '�';
  if (HCode='&ugrave;')  or (HCode = '&#249;')      then Result := '�';
  if (HCode='&uacude;')  or (HCode = '&#250;')      then Result := '�';
  if (HCode='&ucirc;')   or (HCode = '&#251;')      then Result := '�';
  if (HCode='&uuml;')    or (HCode = '&#252;')      then Result := '�';
  if (HCode='&yacute;')  or (HCode = '&#253;')      then Result := '�';
  if (HCode='&thorn;')   or (HCode = '&#254;')      then Result := '�';
  if (HCode='&yuml;')    or (HCode = '&#255;')      then Result := '�';
end;

procedure HTML2TextFile(InFile, OutFile : String);
var s, t: TextFile;
  uml: String;
  param: char;
  j: integer;
  IsTag, Umlaut: Boolean;
begin
  AssignFile(s,InFile);
  AssignFile(t,OutFile);
  Reset(s);
  Rewrite(t);
  IsTag := False;

  while not Eof(s) do
  begin
    Umlaut := False;
    j := 0;
    read(s, param);

    if (param = '<') or (param = '{') then
    begin
      IsTag := true;
      write(t, ' ');
    end;

    // Wenn & gefunden, dann �berpr�fen ob Sonderzeichen vorliegt.
   if not IsTag and (Param = '&') then
    begin
      uml := param;
      j := 0;
      Umlaut := true;
    // Maximal 8 Zeichen lesen oder bis ; gefunden
    repeat
      read(s, param);
      inc (j);
      uml := uml + param;
    until (j = 8) or (param = ';');

    if (param = ';') then
    begin
      param := GiveSZ (uml);
      write(t,param);
    end
    else write(t,uml);
    Umlaut := false;
   end;
   if not IsTag and not Umlaut then write(t, param);
   if (param = '>') or (param = '}') then IsTag := false;
  end;

  CloseFile(s);
  CloseFile(t);
end;

procedure HTML2Text(Html: String; var Plain : String);
var fs:TFileStream;
begin
  try
    try
      fs:= TFileStream.Create(GetTempFolder+'HtmlFile.tmp',fmCreate);
      fs.Write(html,sizeof(html));
    finally
      fs.Free;
    end;
    HTML2TextFile(GetTempFolder+'HtmlFile.tmp',GetTempFolder+'PlainFile.tmp');
    try
      fs:= TFileStream.Create(GetTempFolder+'PlainFile.tmp',fmOpenRead);
      fs.Read(Plain,fs.Size);
    finally
      fs.Free;
    end;
  finally
    DeleteFile(GetTempFolder+'HtmlFile.tmp');
    DeleteFile(GetTempFolder+'PlainFile.tmp');
  end;
end;

end.
 