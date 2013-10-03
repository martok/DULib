{-----------------------------------------------------------------------------
 Unit Name: DUTimeFormat
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   Convert time to string

 History:   2006-08-01 initial relase
-----------------------------------------------------------------------------}
unit DUTimeFormat;

interface

uses SysUtils;

function GetGermanTimeString1(Time:TDateTime):string;
function GetGermanTimeString2(Time:TDateTime):string;

function FormatTime(Time:integer; const FormatStr:string='mm:ss.cc'):string;

implementation

const sOClock      = 'um %d';
      sAfterOClock = '%d nach %d';
      sBefHalf     = '%d vor halb %d';
      sAfterHalf   = '%d nach halb %d';
      sBefOClock   = '%d vor um %d';
      sQuarterAfter= 'viertel nach %d';
      sQuarterBefore1='viertel vor %d';
      sQuarterBefore2='dreiviertel %d';
      sHalf        = 'halb %d';

function GetGermanTimeString1(Time:TDateTime):string;
var h, mm, s, ms:word;
begin
  DecodeTime(Time,h,mm,s,ms);
  if h > 11 then h:= h-12;
  case mm of
         0 : Result:= Format(sOClock ,[h]);
     1..14 : Result:= Format(sAfterOClock,[mm,h]);
        15 : Result:= Format(sQuarterAfter,[h]);
    16..29 : Result:= Format(sBefHalf,[30-mm,h+1]);
        30 : Result:= Format(sHalf,[h+1]);
    31..44 : Result:= Format(sAfterHalf,[mm-30,h+1]);
        45 : Result:= Format(sQuarterBefore1,[h+1]);
    46..59 : Result:= Format(sBefOClock,[60-mm,h+1]);
  end;
end;

function GetGermanTimeString2(Time:TDateTime):string;
var h, mm, s, ms:word;
begin
  DecodeTime(Time,h,mm,s,ms);
  if h > 11 then h:= h-12;
  case mm of
         0 : Result:= Format(sOClock ,[h]);
     1..14 : Result:= Format(sAfterOClock,[mm,h]);
        15 : Result:= Format(sQuarterAfter,[h]);
    16..29 : Result:= Format(sBefHalf,[30-mm,h+1]);
        30 : Result:= Format(sHalf,[h+1]);
    31..44 : Result:= Format(sAfterHalf,[mm-30,h+1]);
        45 : Result:= Format(sQuarterBefore2,[h+1]);
    46..59 : Result:= Format(sBefOClock,[60-mm,h+1]);
  end;
end;


function FormatTime(Time:integer; const FormatStr:string='mm:ss.cc'):string;
const
   DAYS    = 1000 * 60 * 60 * 24;
   HOURS   = 1000 * 60 * 60;
   MINUTES = 1000 * 60;
   SECONDS = 1000;

var day, hour, min, sec, msec:integer;
   Tic, counter, Chrcount:integer;
   a, fmt, fmtstr:String;
   chr, chrLast:char;
begin
  Result:= '';
  tic:= time;

  day:= Tic div DAYS; //Tage
  dec(Tic, day * DAYS);

  hour := Tic div HOURS; //Stunden
  dec(Tic, hour * HOURS);

  min := Tic div MINUTES; //Minuten
  dec(Tic, min * MINUTES);

  sec := Tic div SECONDS; //Sekunden
  dec(tic, sec * seconds);

  msec:= tic;

  fmtStr:=trim(FormatStr)+#32;
  counter:= 1;

  Chrcount:= 1;
  chrLast:= #32;
  repeat
    a:= '';
    Chr:= fmtstr[counter];
    if chr=chrLast then begin
      inc(chrCount);
    end else begin
      fmt:= '%.'+IntTostr(chrcount)+'d';
      case chrLast of
        'd','D': a:= format(fmt,[day]);
        'h','H': a:= format(fmt,[hour]);
        'm','M': a:= format(fmt,[min]);
        's','S': a:= format(fmt,[sec]);
        'c','C': a:= format(fmt,[MSec]);
        ':': a:= TimeSeparator;
      else
        a:= chrLast;
      end;
      result:= result+a;
      chrcount:= 1;
    end;
    chrLast:= chr;
    inc(counter);
  until counter= Length(FmtStr)+1;
  delete(result,1,1);
end;


end.
