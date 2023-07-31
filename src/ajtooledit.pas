unit ajtooledit;

{$I aj.inc}

interface

uses
  Classes, SysUtils, Calendar, LResources, LCLStrConsts, LCLType,
  LMessages, Graphics, MaskEdit, Controls, Buttons, ajdateutil, AJCConst,
  Dialogs, CalendarPopup;


  function PaintComboEdit(Editor: TCustomMaskEdit; const AText: string;
             AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TLMPaint): Boolean;
  function EditorTextMargins(Editor: TCustomMaskEdit): TPoint;


type

  TYearDigits = (dyDefault, dyFour, dyTwo);

  { TAJCustomEditButton }

  TAJCustomEditButton = class(TCustomMaskEdit)
  private
    FDefaultToday: Boolean;
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput: Boolean;
    FIsReadOnly: boolean;
    FOnButtonClick : TNotifyEvent;
    FYearDigits: TYearDigits;
    FDateFormat: string[10];
    FBlanksChar: Char;

    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetDirectInput: Boolean;
    function GetFlat: Boolean;
    function FourDigitYear: Boolean;
    function GetDateFormat: string;
    function GetDate: TDateTime;
    procedure SetYearDigits(const AValue: TYearDigits);


    procedure UpdateFormat;

    procedure CheckButtonVisible;
    procedure SetButtonHint(const AValue: TTranslateString);
    procedure SetButtonNeedsFocus(const AValue: Boolean);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetDirectInput(const AValue: Boolean);
    procedure SetFlat(const AValue: Boolean);
    procedure SetGlyph(Pic: TBitmap);
    function GetGlyph : TBitmap;
    procedure SetNumGlyphs(ANumber: Integer);
    function GetNumGlyphs:Integer;
    function GetMinHeight: Integer;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;

    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;

  protected
    function GetReadOnly: Boolean; override;
    function GetDefaultGlyph: TBitmap; virtual;
    function GetDefaultGlyphName: String; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetReadOnly(AValue: Boolean); override;
    procedure DoPositionButton; virtual;
    procedure DoButtonClick (Sender: TObject); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure SetDate(Value: TDateTime); virtual;

    // New properties.
    property ButtonWidth : Integer read GetButtonWidth write SetButtonWidth;
    property DirectInput : Boolean read GetDirectInput write SetDirectInput default true;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property Button: TSpeedButton read FButton;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;

    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyDefault;


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property ButtonOnlyWhenFocused: Boolean read FButtonNeedsFocus write SetButtonNeedsFocus default False;
    property Date: TDateTime read GetDate write SetDate;

    procedure UpdateMask; virtual;
    function GetDateMask: string;

  end;

    { TAJDateEdit }

  TAcceptDateEvent = procedure (Sender : TObject; var ADate : TDateTime;
    var AcceptDate: Boolean) of object;
  TCustomDateEvent = procedure (Sender : TObject; var ADate : string) of object;

  { TAJDateEdit }

  TAJDateEdit = class(TAJCustomEditButton)
  private
    FDialogTitle: TCaption;
    FDisplaySettings: TDisplaySettings;
    FOnAcceptDate: TAcceptDateEvent;
    FOnCustomDate: TCustomDateEvent;
    FOKCaption: TCaption;
    FCancelCaption: TCaption;
    FDroppedDown: Boolean;
    function GetDate: TDateTime;
    function IsStoreTitle: boolean;
    procedure SetDate(Value: TDateTime);
    procedure CalendarPopupReturnDate(Sender: TObject; const ADate: TDateTime);
  protected
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick(Sender: TObject); override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
 //   procedure DateFormatChanged; virtual;
 //   function GetDateFormat: string;
 //   property Date: TDateTime read GetDate write SetDate;
//    property Button;
    property EditMask;
  published
    property DialogTitle: TCaption read FDialogTitle write FDialogTitle stored IsStoreTitle;
    property CalendarDisplaySettings: TDisplaySettings read FDisplaySettings write FDisplaySettings;
    property OnAcceptDate: TAcceptDateEvent read FOnAcceptDAte write FOnAcceptDate;
    property OnCustomDate: TCustomDateEvent read FOnCustomDate write FOnCustomDate;
    property OKCaption: TCaption read FOKCaption write FOKCaption;
    property CancelCaption: TCaption read FCancelCaption write FCancelCaption;
    property ReadOnly;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday default False;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DirectInput;
    property Glyph;
    property NumGlyphs;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
    property YearDigits;
  end;

var
  DateGlyph: TBitmap;

procedure Register;

implementation

uses LCLIntf;

function PaintComboEdit(Editor: TCustomMaskEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean; var ACanvas: TControlCanvas;
  var Message: TLMPaint): Boolean;
var
  AWidth, ALeft: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
{$IFDEF RX_D4}
  ExStyle: DWORD;
const
  AlignStyle: array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
{$ENDIF}
begin
  Result := True;
  with Editor do
  begin
{$IFDEF RX_D4}
//    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
{$ENDIF}
    if StandardPaint and not(csPaintCopy in ControlState) then
    begin
{$IFDEF RX_D4}
(*      if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
      begin { This keeps the right aligned text, right aligned }
        ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
          (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
        if UseRightToLeftReading then
          ExStyle := ExStyle or WS_EX_RTLREADING;
        if UseRightToLeftScrollbar then
          ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
        ExStyle := ExStyle or
          AlignStyle[UseRightToLeftAlignment, AAlignment];
        if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
          SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
      end; *)
{$ENDIF RX_D4}
      Result := False;
      { return false if we need to use standard paint handler }
      Exit;
    end;
    { Since edit controls do not handle justification unless multi-line (and
      then only poorly) we will draw right and center justify manually unless
      the edit has the focus. }
    if ACanvas = nil then
    begin
      ACanvas := TControlCanvas.Create;
      ACanvas.Control := Editor;
    end;
    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    ACanvas.Handle := DC;
    try
      ACanvas.Font := Font;
      if not Enabled and NewStyleControls and not
        (csDesigning in ComponentState) and
        (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
        ACanvas.Font.Color := clGrayText;
      with ACanvas do
      begin
        R := ClientRect;
        if not (NewStyleControls {and Ctl3D}) and (BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
{          FrameRect(R);
          InflateRect(R, -1, -1);}
        end;
        Brush.Color := Color;
        S := AText;
        AWidth := TextWidth(S);
        Margins := EditorTextMargins(Editor);
{        if PopupVisible then
          ALeft := Margins.X
        else
        begin}
//          if ButtonWidth > 0 then Inc(AWidth);
          case AAlignment of
            taLeftJustify:
              ALeft := Margins.X;
            taRightJustify:
              ALeft := ClientWidth {- ButtonWidth} - AWidth - Margins.X - 2;
            else
              ALeft := (ClientWidth {- ButtonWidth} - AWidth) div 2;
          end;
{        end;}
{$IFDEF RX_D4}
        if SysLocale.MiddleEast then UpdateTextFlags;
{$ENDIF}
        Brush.Style := bsClear;
        TextRect(R, ALeft, Margins.Y, S);
      end;
    finally
      ACanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;

end;

function EditorTextMargins(Editor: TCustomMaskEdit): TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  with Editor do
  begin
    if NewStyleControls then
    begin
      if BorderStyle = bsNone then
        I := 0
      else
{      if Ctl3D then
        I := 1
      else}
        I := 2;
      Result.X := {SendMessage(Handle, LM_GETMARGINS, 0, 0) and $0000FFFF} + I;
      Result.Y := I;
    end
    else
    begin
      if BorderStyle = bsNone then
        I := 0
      else
      begin
        DC := GetDC(0);
        GetTextMetrics(DC, SysMetrics);
        SaveFont := SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, SaveFont);
        ReleaseDC(0, DC);
        I := SysMetrics.tmHeight;
        if I > Metrics.tmHeight then
          I := Metrics.tmHeight;
        I := I div 4;
      end;
      Result.X := I;
      Result.Y := I;
    end;
  end;
end;

procedure Register;
begin
 RegisterComponents('AJCtrls Standard', [TAJDateEdit]);
end;


{ TAJCustomEditButton }

function TAJCustomEditButton.GetButtonHint: TTranslateString;
begin
  Result:=FButton.Hint;
end;

function TAJCustomEditButton.GetButtonWidth: Integer;
begin
  Result:=FButton.Width;
end;

function TAJCustomEditButton.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

function TAJCustomEditButton.GetFlat: Boolean;
begin
   if Assigned(FButton) then
    Result := FButton.Flat
  else
    Result := False;
end;

function TAJCustomEditButton.FourDigitYear: Boolean;
begin
  Result := (FYearDigits = dyFour) or ((FYearDigits = dyDefault) and
    (ajdateutil.FourDigitYear));
end;

function TAJCustomEditButton.GetDateFormat: string;
begin
    Result := FDateFormat;
end;

function TAJCustomEditButton.GetDate: TDateTime;
begin
  if DefaultToday then Result := SysUtils.Date
  else Result := NullDate;
  Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

procedure TAJCustomEditButton.SetYearDigits(const AValue: TYearDigits);
begin
  if FYearDigits <> AValue then begin
    FYearDigits := AValue;
    UpdateMask;
  end;
end;

procedure TAJCustomEditButton.UpdateFormat;
begin
    FDateFormat := DefDateFormat(FourDigitYear);
end;

procedure TAJCustomEditButton.CheckButtonVisible;
begin
  If Assigned(FButton) then
    FButton.Visible:=(csdesigning in ComponentState) or
                     (Visible and (Focused or not FButtonNeedsFocus));
end;

procedure TAJCustomEditButton.SetButtonHint(const AValue: TTranslateString);
begin
  FButton.Hint:=AValue;
end;

procedure TAJCustomEditButton.SetButtonNeedsFocus(const AValue: Boolean);
begin
  if FButtonNeedsFocus<>AValue then
  begin
    FButtonNeedsFocus:=AValue;
    CheckButtonVisible;
  end;
end;

procedure TAJCustomEditButton.SetButtonWidth(const AValue: Integer);
begin
  FButton.Width:=AValue;
end;

procedure TAJCustomEditButton.SetDirectInput(const AValue: Boolean);
begin
  FDirectInput := AValue;
  Inherited SetReadOnly((not FDirectInput) or (FIsReadOnly))
end;

procedure TAJCustomEditButton.SetFlat(const AValue: Boolean);
begin
  If Assigned(FButton) then
    FButton.Flat:=AValue;
end;

procedure TAJCustomEditButton.SetGlyph(Pic: TBitmap);
begin
  FButton.Glyph:=Pic;
end;

function TAJCustomEditButton.GetGlyph: TBitmap;
begin
  Result:=FButton.Glyph;
end;

procedure TAJCustomEditButton.SetNumGlyphs(ANumber: Integer);
begin
   FButton.NumGlyphs:=ANumber;
end;

function TAJCustomEditButton.GetNumGlyphs: Integer;
begin
  Result:=FButton.NumGlyphs;
end;

function TAJCustomEditButton.GetMinHeight: Integer;
begin
  Result:=23;
end;

procedure TAJCustomEditButton.WMSetFocus(var Message: TLMSetFocus);
begin
   FButton.Visible:=True;
  inherited;
end;

procedure TAJCustomEditButton.WMKillFocus(var Message: TLMKillFocus);
begin
  if FButtonNeedsFocus then
    FButton.Visible:=False;
  inherited;
end;

function TAJCustomEditButton.GetReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

function TAJCustomEditButton.GetDefaultGlyph: TBitmap;
begin
  Result := nil;
end;

function TAJCustomEditButton.GetDefaultGlyphName: String;
begin
   Result := 'calendar';
end;

procedure TAJCustomEditButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
  begin
    DoPositionButton;
    CheckButtonVisible;
  end;
end;

procedure TAJCustomEditButton.SetReadOnly(AValue: Boolean);
begin
  FIsReadOnly := AValue;
  if Assigned(FButton) then
    FButton.Enabled := not FIsReadOnly and Enabled;
  inherited SetReadOnly(FIsReadOnly or (not DirectInput));
end;

procedure TAJCustomEditButton.DoPositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  FButton.Visible := Visible;
  FButton.AnchorToCompanion(akLeft,0,Self);
end;

procedure TAJCustomEditButton.DoButtonClick(Sender: TObject);
begin
  If not ReadOnly then
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
end;

procedure TAJCustomEditButton.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
  DoPositionButton;
end;

procedure TAJCustomEditButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TAJCustomEditButton.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);

  CheckButtonVisible;
end;

procedure TAJCustomEditButton.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);

  if (FButton<>nil) and (not ReadOnly) then
    FButton.Enabled:=Enabled;
end;

procedure TAJCustomEditButton.SetDate(Value: TDateTime);
var
  D: TDateTime;
begin
  if not ValidDate(Value) or (Value = NullDate) then begin
    if DefaultToday then Value := SysUtils.Date
    else Value := NullDate;
  end;
  D := Date;
 // TestDateBetween(Value);
  if Value = NullDate then Text := ''
  else Text := FormatDateTime(FDateFormat, Value);
  Modified := D <> Date;
end;

constructor TAJCustomEditButton.Create(AOwner: TComponent);
var
  B: TBitmap;
begin
  inherited Create(AOwner);
  FDirectInput:=true;
  FButton := TSpeedButton.Create(Self);
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  CheckButtonVisible;
  FButton.OnClick := @DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  B := GetDefaultGlyph;
  if B = nil
  then FButton.LoadGlyphFromLazarusResource(GetDefaultGlyphName)
  else FButton.Glyph := B;
  FBlanksChar:='_';
  UpdateFormat;

  ControlStyle := ControlStyle - [csSetCaption];
end;

destructor TAJCustomEditButton.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TAJCustomEditButton.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string[10];
begin
  DateValue := GetDate;
  OldFormat := FDateFormat;
  UpdateFormat;
  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then begin
    { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;
  //UpdatePopup;
  SetDate(DateValue);
end;

function TAJCustomEditButton.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;


{ TDateEdit }

function TAJDateEdit.GetDate: TDateTime;
var
  ADate: string;
begin
  if FDefaultToday then
    Result := SysUtils.Date
  else
    Result := NullDate;
  ADate := Trim(Text);
  if ADate <> '' then
  begin
    if Assigned(FOnCustomDate) then
      FOnCustomDate(Self, ADate);
    Result := StrToDateDef(ADate, Result);
  end;

end;

function TAJDateEdit.IsStoreTitle: boolean;
begin
  Result:=DialogTitle<>rsPickDate;
end;

procedure TAJDateEdit.SetDate(Value: TDateTime);
//var
  //D: TDateTime;
begin
 { if {not IsValidDate(Value) or }(Value = NullDate) then
  begin
    if DefaultToday then
      Value := SysUtils.Date
    else
      Value := NullDate;
  end;
  D := Self.Date;
  if Value = NullDate then
    Text := ''
  else
    Text := DateToStr(Value);
  if D <> Date then
    Change;    }
  inherited SetDate(Value);

end;

procedure TAJDateEdit.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  B:Boolean;
  D:TDateTime;
begin
  try
    B:=true;
    D:=ADate;
    if Assigned(FOnAcceptDate) then
      FOnAcceptDate(Self, D, B);
   if B then
      Self.Date:=D;
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;

end;

function TAJDateEdit.GetDefaultGlyph: TBitmap;
begin
  Result:=inherited GetDefaultGlyph;
end;

function TAJDateEdit.GetDefaultGlyphName: String;
begin
  Result:=inherited GetDefaultGlyphName;
end;

procedure TAJDateEdit.DoButtonClick(Sender: TObject);
var
  PopupOrigin:TPoint;
begin
  inherited DoButtonClick(Sender);
  PopupOrigin:=ControlToScreen(Point(0, Height));

// ShowCalendarPopup(PopupOrigin, Now, CalendarDisplaySettings,
             //      @CalendarPopupReturnDate, @CalendarPopupShowHide)
end;

procedure TAJDateEdit.DblClick;
begin
  inherited DblClick;
  DoButtonClick(nil);
end;

constructor TAJDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultToday := False;
  FDisplaySettings := [dsShowHeadings, dsShowDayNames];
  DialogTitle := rsPickDate;
  OKCaption := 'OK';
  CancelCaption := 'Cancel';
 // DateFormatChanged;

  UpdateMask;
end;

{procedure TAJDateEdit.DateFormatChanged;
begin
  FDateFormat := LongDateFormat;
end; }

{function TAJDateEdit.GetDateFormat: string;
begin
   Result := FDateFormat;
end; }

initialization
{$i ajlaz.lrs}

end.

