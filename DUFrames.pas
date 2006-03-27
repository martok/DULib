unit DUFrames;

interface

uses Classes, Controls, Forms;

type
  TFrame = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;

    procedure Display(AParent:TWinControl);   
  end;


implementation

{ TFrame }

constructor TFrame.Create(AOwner: TComponent);
begin
  inherited;
  Align:= alClient;
  BorderStyle:= bsNone;
  AutoScroll:= true;
  HorzScrollBar.Tracking:= true;
  VertScrollBar.Tracking:= true;
end;

procedure TFrame.Display(AParent: TWinControl);
begin
  Parent:=AParent;
  Left:=0;
  Top:=0;
  Show;
end;

end.
