{-----------------------------------------------------------------------------
 Unit Name: DUPropRead
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   Read properties to stringlist

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}
unit DUPropRead;

interface

uses TypInfo, Classes, SysUtils, Dialogs;

procedure GetClassProps(cl:TObject; OutTo : TStrings);

implementation     

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
    TypeInfo:= TComponent(cl).ClassInfo;

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

end.
