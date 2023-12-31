unit ajlclutils;

{$I aj.inc}

interface

uses
{$IFDEF WIN32}
  Windows,
{$ENDIF}
  Classes, SysUtils, Graphics, Controls, Forms
  ;

type
  TTextOrientation = (toHorizontal, toVertical90, toHorizontal180, toVertical270, toHorizontal360);

function WidthOf(R: TRect): Integer;
function HeightOf(R: TRect): Integer;

procedure RxFrame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);
function DrawButtonFrame(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: Boolean): TRect;
function DrawButtonFrameXP(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: Boolean): TRect;

procedure OutTextXY90(Canvas:TCanvas; X,Y:integer; Text:string; Orientation:TTextOrientation);

function IsForegroundTask: Boolean;
function ValidParentForm(Control: TControl): TCustomForm;
function CreateArrowBitmap:TBitmap;
{
function AllocMemo(Size: Longint): Pointer;
function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
procedure FreeMemo(var fpBlock: Pointer);
}

{$IFDEF WIN32}
type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

  PIconRec = ^TIconRec;
  TIconRec = packed record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    Reserved1: Word;
    Reserved2: Word;
    DIBSize: Longint;
    DIBOffset: Longint;
  end;

procedure ReadIcon(Stream: TStream; var Icon: HICON; ImageCount: Integer;
  StartOffset: Integer; const RequestedSize: TPoint; var IconSize: TPoint);
procedure OutOfResources;
{$ENDIF}

implementation
{$IFNDEF WIN32}
uses LCLProc, LCLIntf;
{$ENDIF}

function WidthOf(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function HeightOf(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

procedure RxFrame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

procedure DoRect;
var
  TopRight, BottomLeft: TPoint;
begin
  TopRight.X := Rect.Right;
  TopRight.Y := Rect.Top;
  BottomLeft.X := Rect.Left;
  BottomLeft.Y := Rect.Bottom;
  Canvas.Pen.Color := TopColor;
  Canvas.PolyLine([BottomLeft, Rect.TopLeft, TopRight]);
  Canvas.Pen.Color := BottomColor;
  Dec(BottomLeft.X);
  Canvas.PolyLine([TopRight, Rect.BottomRight, BottomLeft]);
end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

function DrawButtonFrame(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: Boolean): TRect;
begin
  Result := Client;
  if IsDown then
  begin
    RxFrame3D(Canvas, Result, clWindowFrame, clBtnHighlight, 1);
    if not IsFlat then
      RxFrame3D(Canvas, Result, clBtnShadow, clBtnFace, 1);
  end
  else
  begin
    if IsFlat then
      RxFrame3D(Canvas, Result, clBtnHighlight, clBtnShadow, 1)
    else
    begin
      RxFrame3D(Canvas, Result, clBtnHighlight, clWindowFrame, 1);
      RxFrame3D(Canvas, Result, clBtnFace, clBtnShadow, 1);
    end;
  end;
  InflateRect(Result, -1, -1);
end;

function DrawButtonFrameXP(Canvas: TCanvas; const Client: TRect; IsDown,
  IsFlat: Boolean): TRect;
begin
  Result := Client;
  Canvas.Brush.Color := $00EFD3C6;
  Canvas.FillRect(Client);
  RxFrame3D(Canvas, Result, $00C66931, $00C66931, 1);
end;

{$IFDEF WIN32}
type
  PCheckTaskInfo = ^TCheckTaskInfo;
  TCheckTaskInfo = record
    FocusWnd: HWnd;
    Found: Boolean;
  end;
//function CheckTaskWindow(Window: HWnd; Data: Longint): WordBool; stdcall;
function CheckTaskWindow(Window:HWND; Data:LPARAM):WINBOOL;stdcall;
begin
  Result := True;
  if PCheckTaskInfo(Data)^.FocusWnd = Window then begin
    Result := False;
    PCheckTaskInfo(Data)^.Found := True;
  end;
end;
{$ENDIF}

function IsForegroundTask: Boolean;
{$IFDEF WIN32}
var
  Info: TCheckTaskInfo;
{$ENDIF}
begin
{$IFDEF WIN32}
  Info.FocusWnd := GetActiveWindow;
  Info.Found := False;
  EnumThreadWindows(GetCurrentThreadID, @CheckTaskWindow, Longint(@Info));
  Result := Info.Found;
{$ELSE}
  Result:=true;
{$ENDIF}
end;

function ValidParentForm(Control: TControl): TCustomForm;
begin
  Result := GetParentForm(Control);
  if Result = nil then
    raise EInvalidOperation.CreateFmt('ParentRequired %s', [Control.Name]);
end;

procedure OutTextXY90(Canvas:TCanvas; X,Y:integer; Text:string; Orientation:TTextOrientation);
var
  W,H, i,j:integer;
  Bmp:TBitmap;
begin
  if Orientation = toHorizontal then
    Canvas.TextOut(X, Y, Text)
  else
  begin
    W:=Canvas.TextWidth(Text);
    H:=Canvas.TextHeight(Text);
    Bmp:=TBitMap.Create;
    try
      Bmp.Width:=W;
      Bmp.Height:=H;
      Bmp.Canvas.Brush.Style:=bsSolid;
      Bmp.Canvas.Brush.Color:=clWhite;
      Bmp.Canvas.FillRect(Rect(0,0,W,H));
      Bmp.Canvas.Font:=Canvas.Font;
      Bmp.Canvas.TextOut(0, 0, Text);
      if Orientation = toVertical90 then
      begin
        for i:=0 to W-1 do
          for j:=0 to H-1 do
            if Bmp.Canvas.Pixels[i,j]<>clWhite then
              Canvas.Pixels[(H-j)+X,i+Y]:=Bmp.Canvas.Pixels[i,j];
      end
      else
      if Orientation = toVertical270 then
      begin
        for i:=0 to W-1 do
          for j:=0 to H-1 do
            if Bmp.Canvas.Pixels[i,j]<>clWhite then
              Canvas.Pixels[j+X,(W-i)+Y]:=Bmp.Canvas.Pixels[i,j];
      end
      else
      if Orientation = toHorizontal180 then
      begin
        for i:=0 to W-1 do
          for j:=0 to H-1 do
            if Bmp.Canvas.Pixels[i,j]<>clWhite then
              Canvas.Pixels[i+X,(H-j)+Y]:=Bmp.Canvas.Pixels[i,j];
      end
      else
      if Orientation = toHorizontal360 then
      begin
        for i:=0 to W-1 do
          for j:=0 to H-1 do
            if Bmp.Canvas.Pixels[i,j]<>clWhite then
              Canvas.Pixels[(W-i)+X,j+Y]:=Bmp.Canvas.Pixels[i,j];
      end;
    finally
      Bmp.Free;
    end;
  end;
end;

{
function AllocMemo(Size: Longint): Pointer;
begin
  if Size > 0 then
    Result := GlobalAllocPtr(HeapAllocFlags or GMEM_ZEROINIT, Size)
  else Result := nil;
end;

function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
begin
  Result := GlobalReallocPtr(fpBlock, Size,
    HeapAllocFlags or GMEM_ZEROINIT);
end;

procedure FreeMemo(var fpBlock: Pointer);
begin
  if fpBlock <> nil then begin
    GlobalFreePtr(fpBlock);
    fpBlock := nil;
  end;
end;
}
{$IFDEF WIN32}
function CreateIcon(hInstance: HINST; nWidth, nHeight: Integer;
  cPlanes, cBitsPixel: Byte; lpbANDbits, lpbXORbits: Pointer): HICON; stdcall; external user32 name 'CreateIcon';

procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function DupBits(Src: HBITMAP; Size: TPoint; Mono: Boolean): HBITMAP;
var
  DC, Mem1, Mem2: HDC;
  Old1, Old2: HBITMAP;
  Bitmap: Windows.TBitmap;
begin
  Mem1 := CreateCompatibleDC(0);
  Mem2 := CreateCompatibleDC(0);

  try
    GetObject(Src, SizeOf(Bitmap), @Bitmap);
    if Mono then
      Result := CreateBitmap(Size.X, Size.Y, 1, 1, nil)
    else
    begin
      DC := GetDC(0);
      if DC = 0 then GDIError;
      try
        Result := CreateCompatibleBitmap(DC, Size.X, Size.Y);
        if Result = 0 then GDIError;
      finally
        ReleaseDC(0, DC);
      end;
    end;

    if Result <> 0 then
    begin
      Old1 := SelectObject(Mem1, Src);
      Old2 := SelectObject(Mem2, Result);

      StretchBlt(Mem2, 0, 0, Size.X, Size.Y, Mem1, 0, 0, Bitmap.bmWidth,
        Bitmap.bmHeight, SrcCopy);
      if Old1 <> 0 then SelectObject(Mem1, Old1);
      if Old2 <> 0 then SelectObject(Mem2, Old2);
    end;
  finally
    DeleteDC(Mem1);
    DeleteDC(Mem2);
  end;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

procedure TwoBitsFromDIB(var BI: TBitmapInfoHeader; var XorBits, AndBits: HBITMAP;
  const IconSize: TPoint);
type
  PLongArray = ^TLongArray;
  TLongArray = array[0..1] of Longint;
var
  Temp: HBITMAP;
  NumColors: Integer;
  DC: HDC;
  Bits: Pointer;
  Colors: PLongArray;
begin
  with BI do
  begin
    biHeight := biHeight shr 1; { Size in record is doubled }
    biSizeImage := BytesPerScanline(biWidth, biBitCount, 32) * biHeight;
    NumColors := GetDInColors(biBitCount);
  end;
  DC := GetDC(0);
  if DC = 0 then OutOfResources;
  try
    Bits := Pointer(Longint(@BI) + SizeOf(BI) + NumColors * SizeOf(TRGBQuad));
    Temp := GDICheck(CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS));
    try
      XorBits := DupBits(Temp, IconSize, False);
    finally
      DeleteObject(Temp);
    end;
    with BI do
    begin
      Inc(Longint(Bits), biSizeImage);
      biBitCount := 1;
      biSizeImage := BytesPerScanline(biWidth, biBitCount, 32) * biHeight;
      biClrUsed := 2;
      biClrImportant := 2;
    end;
    Colors := Pointer(Longint(@BI) + SizeOf(BI));
    Colors^[0] := 0;
    Colors^[1] := $FFFFFF;
    Temp := GDICheck(CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS));
    try
      AndBits := DupBits(Temp, IconSize, True);
    finally
      DeleteObject(Temp);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure ReadIcon(Stream: TStream; var Icon: HICON; ImageCount: Integer;
  StartOffset: Integer; const RequestedSize: TPoint; var IconSize: TPoint);
type
  PIconRecArray = ^TIconRecArray;
  TIconRecArray = array[0..300] of TIconRec;
var
  List: PIconRecArray;
  HeaderLen, Length: Integer;
  BitsPerPixel: Word;
  Colors, BestColor, C1, N, Index: Integer;
  DC: HDC;
  BI: PBitmapInfoHeader;
  ResData: Pointer;
  XorBits, AndBits: HBITMAP;
  XorInfo, AndInfo: Windows.TBitmap;
  XorMem, AndMem: Pointer;
  XorLen, AndLen: Integer;

  function AdjustColor(I: Integer): Integer;
  begin
    if I = 0 then
      Result := MaxInt
    else
      Result := I;
  end;

  function BetterSize(const Old, New: TIconRec): Boolean;
  var
    NewX, NewY, OldX, OldY: Integer;
  begin
    NewX := New.Width - IconSize.X;
    NewY := New.Height - IconSize.Y;
    OldX := Old.Width - IconSize.X;
    OldY := Old.Height - IconSize.Y;
    Result := (Abs(NewX) <= Abs(OldX)) and ((NewX <= 0) or (NewX <= OldX)) and
       (Abs(NewY) <= Abs(OldY)) and ((NewY <= 0) or (NewY <= OldY));
  end;

begin
  HeaderLen := SizeOf(TIconRec) * ImageCount;
  List := AllocMem(HeaderLen);
  try
    Stream.Read(List^, HeaderLen);
    if (RequestedSize.X or RequestedSize.Y) = 0 then
    begin
      IconSize.X := GetSystemMetrics(SM_CXICON);
      IconSize.Y := GetSystemMetrics(SM_CYICON);
    end
    else
      IconSize := RequestedSize;
    DC := GetDC(0);
    if DC = 0 then OutOfResources;
    try
      BitsPerPixel := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      if BitsPerPixel > 8 then
        Colors := MaxInt
      else
        Colors := 1 shl BitsPerPixel;
    finally
      ReleaseDC(0, DC);
    end;

    { Find the image that most closely matches (<=) the current screen color
      depth and the requested image size.  }
    Index := 0;
    BestColor := AdjustColor(List^[0].Colors);
    for N := 1 to ImageCount-1 do
    begin
      C1 := AdjustColor(List^[N].Colors);
      if (C1 <= Colors) and (C1 >= BestColor) and
        BetterSize(List^[Index], List^[N]) then
      begin
        Index := N;
        BestColor := C1;
      end;
    end;

    with List^[Index] do
    begin
      IconSize.X := Width;
      IconSize.Y := Height;
      BI := AllocMem(DIBSize);
      try
        Stream.Seek(DIBOffset  - (HeaderLen + StartOffset), 1);
        Stream.Read(BI^, DIBSize);
        TwoBitsFromDIB(BI^, XorBits, AndBits, IconSize);
        GetObject(AndBits, SizeOf(Windows.TBitmap), @AndInfo);
        GetObject(XorBits, SizeOf(Windows.TBitmap), @XorInfo);
        with AndInfo do
          AndLen := bmWidthBytes * bmHeight * bmPlanes;
        with XorInfo do
          XorLen :=  bmWidthBytes * bmHeight * bmPlanes;
        Length := AndLen + XorLen;
        ResData := AllocMem(Length);
        try
          AndMem := ResData;
          with AndInfo do
            XorMem := Pointer(Longint(ResData) + AndLen);
          GetBitmapBits(AndBits, AndLen, AndMem);
          GetBitmapBits(XorBits, XorLen, XorMem);
          DeleteObject(XorBits);
          DeleteObject(AndBits);
          Icon := CreateIcon(HInstance, IconSize.X, IconSize.Y,
            XorInfo.bmPlanes, XorInfo.bmBitsPixel, AndMem, XorMem);
          if Icon = 0 then GDIError;
        finally
          FreeMem(ResData, Length);
        end;
      finally
        FreeMem(BI, DIBSize);
      end;
    end;
  finally
    FreeMem(List, HeaderLen);
  end;
end;

procedure OutOfResources;
begin
  raise Exception.Create('SOutOfResources');
end;
{$ENDIF}

function CreateArrowBitmap:TBitmap;
begin
  Result:=Graphics.TBitmap.Create;
  Result.LoadFromLazarusResource('rxbtn_downarrow');
end;

end.


