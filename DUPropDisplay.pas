{-----------------------------------------------------------------------------
 Unit Name: DUPropDisplay
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   Property display Window,
            To deactivate, set 'NoPropViewer' to true
            Program calls do not have to be changed

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}


unit DUPropDisplay;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TypInfo;

type

  TPropDisplay = class(TForm)
    meProperties: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  PropDisplay  : TPropDisplay;

  NoPropViewer : boolean;
  
procedure CreatePropDisplay;
procedure DisplayProperties(Obj:TObject);
procedure FreeAndNilPropDisplay;

procedure GetClassProps(cl:TObject; OutTo : TStrings);

implementation

{$R *.DFM}

{ TPropDisplay }

{PROPREAD FUNCTIONS:}


function GetSetString(Value: Cardinal; PropInfo: PPropInfo): String;
var
  i: Integer;
  TypeInfo: PTypeInfo;
begin
  Result:='';

  TypeInfo:=GetTypeData(PropInfo.PropType^)^.CompType^;
  for i:=0 to 31 do
    if (Value and (1 shl i))>0 then begin
      if Result<>'' then  Result:=Result+',';
      Result:=Result+GetEnumName(TypeInfo, i);
    end;

  Result:='['+Result+']';
end;

function GetObjAsString(Instance: TObject): String;
begin
  if Instance=nil then Result:='nil'
  else if Instance is TComponent then
    Result:=TComponent(Instance).Name
  else Result:=Instance.ClassName;
end;

function GetPropValAsString(Instance: TObject; PropInfo: PPropInfo): String;
begin
  case PropInfo.PropType^.Kind of
    tkInteger: Result:=IntToStr(GetOrdProp(Instance, PropInfo));
    tkChar, tkWChar:
      Result:=Char(GetOrdProp(Instance, PropInfo));
    tkEnumeration:
      Result:=GetEnumName(PropInfo^.PropType^, GetOrdProp(Instance, PropInfo));
    tkFloat:
      Result:=FloatToStr(GetFloatProp(Instance, PropInfo));
    tkString, tkLString, tkWString:
      Result:=GetStrProp(Instance, PropInfo);
    tkSet: Result:=GetSetString(GetOrdProp(Instance, PropInfo), PropInfo);
    tkClass: Result:=GetObjAsString(TObject(GetOrdProp(Instance, PropInfo)));
    tkVariant: Result:=GetVariantProp(Instance, PropInfo);
  end;
end;

procedure GetClassProps(cl:TObject; OutTo : TStrings);
var
  i, count: Integer;
  TypeInfo: PTypeInfo;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  OutTo.Clear;

  if cl<>nil then try
    TypeInfo:= cl.ClassInfo;

    count:=GetPropList(TypeInfo, tkProperties, nil);
    GetMem(PropList, count * SizeOf(Pointer));
    try
      GetPropList(TypeInfo, tkProperties, PropList);
      for i:=0 to count-1 do begin
        PropInfo:=PropList^[i];
        {Speichern der Eigenschaftsnamen mit Zeiger auf dessen
         PropInfo-Struktur in der Liste}
        OutTo.add(format('%-15s : %s',[PropInfo.Name,GetPropValAsString(cl, PropInfo)]));
      end;
    finally
      FreeMem(PropList);
    end;
  except
    on E: Exception do begin
      MessageDlg('Fehler bei der Anzeige der Eigenschaften:'+#13+
                 E.Message, mtError, [mbOk], 0);
    end;
  end;
end;

{PROPDISPLAY}


procedure TPropDisplay.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:= caFree;
  PropDisplay:= nil;
end;


procedure CreatePropDisplay;
begin
  if NoPropViewer then exit;

  FreeAndNilPropDisplay;
  PropDisplay:= TPropDisplay.Create(Application);
  with PropDisplay do begin
    Width := 270;
    Height:= 435;
    Left  := Screen.Width-Width;
    Top   := 0;
  end;
end;

procedure DisplayProperties(Obj:TObject);
begin
  if NoPropViewer then exit;

  CreatePropDisplay;
  GetClassProps(obj,PropDisplay.meProperties.Lines);
  with PropDisplay do begin
    Caption:= format('Eigenschaften von "%s", Typ "%s"',[TComponent(obj).Name,obj.Classname]);
    Show;
  end;
end;

procedure FreeAndNilPropDisplay;
begin
  if NoPropViewer then exit;

  PropDisplay.Free;
  PropDisplay:= nil;
end;


initialization
  NoPropViewer := false;


end.
