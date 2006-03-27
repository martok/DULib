unit Polytest;

{The code below was posted by:

"John Hutchings" <johnh@datavis.com.au>
Date:
Fri, 19 Oct 2001 06:20:44 +1000
Newsgroups:
borland.public.delphi.graphics

// The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
// with some minor modifications for speed.  It returns 1 for strictly
// interior points, 0 for strictly exterior, and 0 or 1 for points on
// the boundary.

Comment: returns a Boolean !
}

interface

uses
   Classes, SysUtils,
{$IFDEF WINDOWS}
   Wintypes;
{$ENDIF}
{$IFDEF WIN32}
   Windows;
{$ENDIF}
{$IFDEF LINUX}
   Types, Untranslated;
{$ENDIF}

function PointInPolygonTest(x, y, N: Integer; aList: Array of TPoint): Boolean;

implementation

function PointInPolygonTest(x, y, N: Integer; aList: Array of TPoint): Boolean;
var
   I, J : Integer;

   Function xp(aVal:Integer):Integer;
   Begin
     Result:= PPoint(@aList[aVal]).X;
   end;

   Function yp(aVal:Integer):Integer;
   Begin
     Result:= PPoint(@aList[aVal]).Y;
   end;

begin
   Result := False;
   {L := Length(aList);}
   if (N = 0) then exit;
   J := N-1;
   for I := 0 to N-1 do
   begin
     if ((((yp(I) <= y) and (y < yp(J))) or
          ((yp(J) <= y) and (y < yp(I)))) and
         (x < (xp(J)-xp(I))*(y-yp(I))/(yp(J)-yp(I))+xp(I)))
     then Result := not Result;
     J:=I;
   end;
end;

end.
