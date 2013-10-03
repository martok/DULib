{-----------------------------------------------------------------------------
 Unit Name: DUFrames
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   TFrame replacement, AutoScroll and Display(Parent);

 History:   2006-08-01 initial release
            2007-01-23 added OnActivate Event
            2010-12-31 allow focusing of frames
-----------------------------------------------------------------------------}
unit DUFrames;

interface

uses Classes, Controls, Forms, Messages;

type
  TDUFrame = class(TForm)
  protected
    procedure WMMouseActivate(var Message: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Display(AParent:TWinControl);
  end;


implementation

{ TFrame }

constructor TDUFrame.Create(AOwner: TComponent);
begin
  inherited;
  AutoScroll:= true;
  HorzScrollBar.Tracking:= true;
  VertScrollBar.Tracking:= true;
  BorderStyle:= bsNone;
  TabStop:= true;
  Align:= alClient;
end;

procedure TDUFrame.Display(AParent: TWinControl);
begin
  Parent:=AParent;
  Left:=0;
  Top:=0;
  Show;
  SetFocus;
  if Assigned(OnActivate) then
    OnActivate(Self);
end;

procedure TDUFrame.DoEnter;
begin
  inherited;
end;

procedure TDUFrame.DoExit;
begin
  inherited;
end;

procedure TDUFrame.WMMouseActivate(var Message: TWMMouseActivate);
begin
  SetFocus;
end;

end.
