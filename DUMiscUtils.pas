unit DUMiscUtils;

interface

uses SysUtils, Windows;

function iif(Condition:boolean; IfTrue, IfFalse:Variant):Variant; overload;
function iif(Condition:boolean; IfTrue, IfFalse:TObject):TObject; overload;

function GetErrText(Err: Integer): String;

implementation

function iif(Condition:boolean; IfTrue, IfFalse:Variant):Variant;
begin
  if Condition then Result:= IfTrue else Result:= IfFalse;
end;

function iif(Condition:boolean; IfTrue, IfFalse:TObject):TObject;
begin
  if Condition then Result:= IfTrue else Result:= IfFalse;
end;

function GetErrText(Err: Integer): String;
begin
  Result:= SysErrorMessage(Err);
end;

end.
 
