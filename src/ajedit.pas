
unit ajedit;

{$Mode Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, LResources, Controls, Graphics, SysUtils, Classes,
  MaskEdit, StdCtrls, db, DBCtrls;


type
  TAJEditDisplayFormat = (dfAlphabetic, dfAlphaNumeric, dfCheckChars,
  dfCustom, dfFloat, dfFloatGeneral, dfInteger,  dfNonCheckChars, dfNone,
  dfPercent, dfYear);

  TAJEditCriticalPointsCheck = (cpNone, cpMinValue, cpMaxValue, cpBoth);

  TAJCustomValidateEdit = class;

  { TAJEditCriticalPoints }

  TAJEditCriticalPoints = class(TPersistent)
  private
    FCheckPoints: TAJEditCriticalPointsCheck;
    FColorAbove: TColor;
    FColorBelow: TColor;
    FMaxValue: Double;
    FMinValue: Double;
    FMaxValueIncluded: Boolean;
    FMinValueIncluded: Boolean;
    FOnChange: TNotifyEvent;
    FDefCheckPoints: TAJEditCriticalPointsCheck;
    FDefColorAbove: TColor;
    FDefColorBelow: TColor;
    procedure DoChanged;
    procedure SetMinValue(NewValue: Double);
    procedure SetMaxValue(NewValue: Double);
    procedure SetColorAbove(NewValue: TColor);
    procedure SetColorBelow(NewValue: TColor);
    procedure SetCheckPoints(NewValue: TAJEditCriticalPointsCheck);
    function IsCheckPointsStored: Boolean;
    function IsColorAboveStored: Boolean;
    function IsColorBelowStored: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    procedure SetDefaults(ACheckPoints: TAJEditCriticalPointsCheck;
      AColorAbove, AColorBelow: TColor);
    constructor Create;
  published
    property CheckPoints: TAJEditCriticalPointsCheck read FCheckPoints
      write SetCheckPoints stored IsCheckPointsStored;
    property ColorAbove: TColor read FColorAbove write SetColorAbove stored IsColorAboveStored;
    property ColorBelow: TColor read FColorBelow write SetColorBelow stored IsColorBelowStored;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValueIncluded: Boolean read FMaxValueIncluded write FMaxValueIncluded;
    property MinValueIncluded: Boolean read FMinValueIncluded write FMinValueIncluded;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAJCustomTextValidateEvent = procedure(Sender: TObject; Key: Char;
    const AText: string; const Pos: Integer; var IsValid: Boolean) of object;
  TAJCustomIsValidEvent = procedure(Sender: TObject; var IsValid: Boolean) of object;

  { TAJCustomValidateEdit }

  TAJCustomValidateEdit = class(TCustomMaskEdit)
  private
    FColorRequired: TColor;
    FPainted: Boolean;
    FModifiedColor: TColor;
    FRequired: Boolean;
    FRequiredWidth: Integer;
    FSelfChange: Boolean;
    FCheckChars: string;
    FDecimalPlaces: Cardinal;
    FDisplayFormat: TAJEditDisplayFormat;
    FHasMaxValue: Boolean;
    FHasMinValue: Boolean;
    FMaxValue: Double;
    FMinValue: Double;
    FOnCustomValidate: TAJCustomTextValidateEvent;
    FOnValueChanged: TNotifyEvent;
    FZeroEmpty: Boolean;
    EnterText: string;
    FDisplayPrefix: string;
    FDisplaySuffix: string;
    FCriticalPoints: TAJEditCriticalPoints;
    FStandardFontColor: TColor;
    FAutoAlignment: Boolean;
    FTrimDecimals: Boolean;
    FOldFontChange: TNotifyEvent;
    FOnIsValid: TAJCustomIsValidEvent;
    FEnforcingMinMaxValue: Boolean;
    FShowModified: Boolean;
    FFontColor: TColor;
    FIsModified: Boolean;
    procedure PaintEdit;
    procedure SelectAll;
    procedure DisplayText;
    function GetModified: Boolean;
    function ScientificStrToFloat(SciString: string): Double;
    procedure SetHasMaxValue(NewValue: Boolean);
    procedure SetHasMinValue(NewValue: Boolean);
    procedure SetMaxValue(NewValue: Double);
    procedure SetMinValue(NewValue: Double);
    procedure SetDecimalPlaces(NewValue: Cardinal);
    procedure SetDisplayFormat(NewValue: TAJEditDisplayFormat);
    procedure SetModified(const AValue: Boolean);
    procedure SetZeroEmpty(NewValue: Boolean);
    function GetAsInteger: Int64;
    procedure SetAsInteger(NewValue: Int64);
    function GetAsFloat: Double;
    procedure SetAsFloat(NewValue: Double);
    function GetValue: Variant;
    procedure SetValue(NewValue: Variant);
    procedure SetCheckChars(const NewValue: string);
    function IsCheckCharsStored: Boolean;
    function CurrRangeValue(CheckValue: Currency): Currency; overload;
    function FloatRangeValue(CheckValue: Double): Double; overload;
    function IntRangeValue(CheckValue: Int64): Int64; overload;
    function GetEditText: string;
    procedure SetEditText(const NewValue: string);
    procedure ChangeText(const NewValue: string);
    function BaseToInt(const BaseValue: string; Base: Byte): Int64;
    function IntToBase(NewValue: Int64; Base: Byte): string;
    procedure DoValueChanged;
    procedure SetDisplayPrefix(const NewValue: string);
    procedure SetDisplaySuffix(const NewValue: string);
    procedure CriticalPointsChange(Sender: TObject);
    procedure SetFontColor;
    procedure FontChange(Sender: TObject);
    procedure EnforceMaxValue;
    procedure EnforceMinValue;
    procedure SetTrimDecimals(const Value: Boolean);
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
    procedure CMEnter(var Message: TLMEnter); message LM_ENTER;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure WMKeyUp(var Message: TLMKeyUp); message LM_KEYUP;
    procedure CheckForInvalidate;
  protected
    function IsValidChar(const S: string; var Key: Char; Posn: Integer): Boolean; virtual;
    function MakeValid(const ParseString: string): string;virtual;
    procedure Change; override;
    procedure LMPaste(var Msg: TLMessage); message LM_PASTE;
    procedure SetText(const NewValue: TCaption);
    property CheckChars: string read FCheckChars write SetCheckChars
      stored IsCheckCharsStored;
    property TrimDecimals: Boolean read FTrimDecimals write SetTrimDecimals;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces;
    property DisplayFormat: TAJEditDisplayFormat read FDisplayFormat
      write SetDisplayFormat;
    property HasMaxValue: Boolean read FHasMaxValue write SetHasMaxValue;
    property HasMinValue: Boolean read FHasMinValue write SetHasMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property OnCustomValidate: TAJCustomTextValidateEvent
      read FOnCustomValidate write FOnCustomValidate;
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
    property Value: Variant read GetValue write SetValue stored False;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty;
    property DisplayPrefix: string read FDisplayPrefix write SetDisplayPrefix;
    property DisplaySuffix: string read FDisplaySuffix write SetDisplaySuffix;
    property CriticalPoints: TAJEditCriticalPoints read FCriticalPoints
      write FCriticalPoints;
    property AutoAlignment: Boolean read FAutoAlignment write FAutoAlignment;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function DoValidate(const Key: Char; const AText: string;
      const Posn: Integer): Boolean;
    procedure Loaded; override;
    property OnIsValid: TAJCustomIsValidEvent read FOnIsValid write FOnIsValid;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor default clHighlight;
    property ShowModified: Boolean read FShowModified write FShowModified default False;
    property Modified: Boolean read GetModified write SetModified;
    property Required: Boolean read FRequired write FRequired;
    property RequiredColor: TColor read FColorRequired write FColorRequired;
    property RequiredWidth: Integer read FRequiredWidth write FRequiredWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsValid: Boolean; virtual; // fires OnIsValid if assigned

    procedure Assign(Source: TPersistent); override;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;

  end;

  TAJEdit = class(TAJCustomValidateEdit)
  published
    property Alignment default taRightJustify;
    property Anchors;
    property AutoAlignment default True;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property CheckChars;
    property CharCase;
    property Color;
    property Constraints;
    property CriticalPoints;
    property TrimDecimals default False;
    property DisplayFormat default dfNone;
    property DecimalPlaces default 0;
    property DisplayPrefix;
    property DisplaySuffix;
    property DragMode;
    property EditText;
    property Enabled;
    property Font;
    property HasMaxValue default False;
    property HasMinValue default False;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ModifiedColor;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property Required;
    property RequiredWidth;
    property RequiredColor;
    property ShowHint;
    property ShowModified;
    property TabOrder;
    property TabStop;
    property Text stored False;
    property TextHint;
    property Value;
    property Visible;
    property ZeroEmpty default False;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnCustomValidate;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnValueChanged;
    property OnIsValid;
  end;

  { TAJDBEdit }

  TAJDBEdit = class(TAJCustomValidateEdit)
  private
    FDataLink: TFieldDataLink;
    procedure DoCheckEnable;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
  protected
    { Protected declarations }
    procedure SetReadOnly(const AValue: Boolean);
    function GetReadOnly: Boolean;
    procedure ActiveChange(Sender:TObject);
    procedure DataChange(Sender:TObject);
    procedure EditingChange(Sender: TObject);
    procedure UpdateData(Sender:TObject);
    procedure LMCutToClip(var Message: TLMessage); message LM_CUT;
    procedure LMPasteFromClip(var Message: TLMessage); message LM_PASTE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure EditingDone; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property CriticalPoints;
    property TrimDecimals default False;
    property DisplayFormat default dfNone;
    property DecimalPlaces default 0;
    property DisplayPrefix;
    property DisplaySuffix;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
    property HasMaxValue default False;
    property HasMinValue default False;
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ModifiedColor;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property Required;
    property RequiredWidth;
    property RequiredColor;
    property ShowModified;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TextHint;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;

  end;

procedure register;

implementation

uses Math, ajstringutils;


resourcestring

  RsEBaseTooBig = 'Base > 36 not supported';
  RsEBaseTooSmall = 'Base must be greater than 1';


function IsGreater(Value, MaxValue: Double; MaxValueIncluded: Boolean): Boolean;
begin
  if MaxValueIncluded then
    Result := Value >= MaxValue
  else
    Result := Value > MaxValue;
end;

function IsLower(Value, MinValue: Double; MinValueIncluded: Boolean): Boolean;
begin
  if MinValueIncluded then
    Result := Value <= MinValue
  else
    Result := Value < MinValue;
end;

function StrEnsureNoPrefix(const Prefix, Text: string): string;
var
  PrefixLen : Integer;
begin
  PrefixLen := Length(Prefix);
  if Copy(Text, 1, PrefixLen) = Prefix then
    Result := Copy(Text, PrefixLen + 1, Length(Text))
  else
    Result := Text;
end;

function StrEnsureNoSuffix(const Suffix, Text: string): string;
var
  SuffixLen : Integer;
  StrLength : Integer;
begin
  SuffixLen := Length(Suffix);
  StrLength := Length(Text);
  if Copy(Text, StrLength - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Copy(Text, 1, StrLength - SuffixLen)
  else
    Result := Text;
end;

function MakeYear4Digit(Year, Pivot: Integer): Integer;
var
  Century: Integer;
begin
  { map 100 to zero }
  if Year = 100 then
    Year := 0;
  if Pivot = 100 then
    Pivot := 0;

  // turn 2 digit pivot to 4 digit
  if Pivot < 100 then
    Pivot := Pivot + 1900;

  { turn 2 digit years to 4 digits }
  if (Year >= 0) and (Year < 100) then
  begin
    Century := (Pivot div 100) * 100;

    Result := Year + Century; // give the result the same century as the pivot
    if Result < Pivot then
      //  cannot be lower than the Pivot
      Result := Result + 100;
  end
  else
    Result := Year;
end;

procedure FillString(var Buffer: string; Count: Integer; const Value: Char);
begin
  FillChar(Buffer[1], Count, Value);
end;

function MakeStr(C: Char; N: Integer): string; overload;
begin
  if N < 1 then
    Result := ''
  else
  begin
    SetLength(Result, N);
    FillString(Result, Length(Result), C);
  end;
end;

function AddChar(C: Char; const S: string; N: Integer): string;
begin
  if Length(S) < N then
    Result := MakeStr(C, N - Length(S)) + S
  else
    Result := S;
end;

function Numb2Dec(S: string; B: Byte): Int64;
var
  I, P: Int64;
begin
  I := Length(S);
  Result := 0;
  S := UpperCase(S);
  P := 1;
  while (I >= 1) do
  begin
    if S[I] > '@' then
      Result := Result + (Ord(S[I]) - 55) * P
    else
      Result := Result + (Ord(S[I]) - 48) * P;
    Dec(I);
    P := P * B;
  end;
end;

function Dec2Numb(N: Int64; A, B: Byte): string;
var
  C: Integer;
  Number: Cardinal;
begin
  if N = 0 then
    Result := '0'
  else
  begin
    Number := Cardinal(N);
    Result := '';
    while Number > 0 do
    begin
      C := Number mod B;
      if C > 9 then
        C := C + 55
      else
        C := C + 48;
      Result := Chr(C) + Result;
      Number := Number div B;
    end;
  end;
  if Result <> '' then
    Result := AddChar('0', Result, A);
end;

procedure register;
begin
    RegisterComponents('AJCtrls', [TAJEdit, TAJDBEdit]);
end;

{ TAJDBEdit }

procedure TAJDBEdit.DoCheckEnable;
begin
  Enabled := FDataLink.Active and (FDataLink.Field <> nil) and (not FDataLink.Field.ReadOnly);
end;

function TAJDBEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TAJDBEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TAJDBEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TAJDBEdit.SetDataField(AValue: string);
begin
  try
    FDataLink.FieldName := AValue;
  finally
    DoCheckEnable;
  end;
end;

procedure TAJDBEdit.SetDataSource(AValue: TDataSource);
begin
  FDataLink.DataSource := AValue;
  DoCheckEnable;
end;

procedure TAJDBEdit.SetReadOnly(const AValue: Boolean);
begin
  FDataLink.ReadOnly := AValue;
end;

procedure TAJDBEdit.ActiveChange(Sender: TObject);
begin
  DoCheckEnable;
end;

procedure TAJDBEdit.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
  begin
    if FDataLink.Field.IsNull then
      Text := ''
    else
      Text := FDataLink.Field.Text;
  end else Text := '';
end;

procedure TAJDBEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TAJDBEdit.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
  begin
    if Text <> '' then
      FDataLink.Field.Text := Text
    else FDataLink.Field.Clear;
  end;
end;

procedure TAJDBEdit.LMCutToClip(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TAJDBEdit.LMPasteFromClip(var Message: TLMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TAJDBEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_ESCAPE then
  begin
    // Cancela a edição
    FDataLink.Reset;
    SelectAll;
    Key := VK_UNKNOWN;
  end else if Key <> VK_UNKNOWN then
    FDataLink.Edit;
end;

procedure TAJDBEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

procedure TAJDBEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
       DataSource := nil;
  end;
end;

procedure TAJDBEdit.EditingDone;
begin
  inherited EditingDone;
  if FDataLink.CanModify then
    FDataLink.UpdateRecord;
end;

constructor TAJDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := ActiveChange;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TAJDBEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

//=== { TAJCustomValidateEdit } ==============================================

constructor TAJCustomValidateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelfChange := False;
  FAutoAlignment := True;
  FCriticalPoints := TAJEditCriticalPoints.Create;
  FCriticalPoints.OnChange := CriticalPointsChange;
  FDisplayFormat := dfAlphaNumeric;
  FCheckChars := '';
  Alignment := taLeftJustify;
  Text := '';
  AutoSize := True;
  FMinValue := 0;
  FMaxValue := 0;
  FHasMinValue := False;
  FHasMaxValue := False;
  FColorRequired := clRed;
  FRequiredWidth := 3;
  FZeroEmpty := False;
  FStandardFontColor := Font.Color;
  FOldFontChange := Font.OnChange;
  Font.OnChange := FontChange;
  FFontColor := Self.Font.Color;
  FModifiedColor := clHighlight;
end;

destructor TAJCustomValidateEdit.Destroy;
begin
  FreeAndNil(FCriticalPoints);
  inherited Destroy;
end;

procedure TAJCustomValidateEdit.Assign(Source: TPersistent);
var
  lcSource: TAJCustomValidateEdit;
begin
  if Source is TAJCustomValidateEdit then
  begin
    lcSource := TAJCustomValidateEdit(Source);
    CriticalPoints.Assign(lcSource.CriticalPoints);
    DisplayFormat := lcSource.DisplayFormat;
    DecimalPlaces := lcSource.DecimalPlaces;
    MinValue := lcSource.MinValue;
    MaxValue := lcSource.MaxValue;
    HasMinValue := lcSource.HasMinValue;
    HasMaxValue := lcSource.HasMaxValue;
    ZeroEmpty := lcSource.ZeroEmpty;
  end
  else
    inherited Assign(Source);
end;

procedure TAJCustomValidateEdit.Loaded;
begin
  inherited Loaded;
    EditText := Text;
end;

procedure TAJCustomValidateEdit.SetHasMaxValue(NewValue: Boolean);
begin
  if FHasMaxValue <> NewValue then
  begin
    FHasMaxValue := NewValue;
    if not (csLoading in ComponentState) then
      EnforceMaxValue;
  end;
end;

procedure TAJCustomValidateEdit.SetHasMinValue(NewValue: Boolean);
begin
  if FHasMinValue <> NewValue then
  begin
    FHasMinValue := NewValue;
    if not (csLoading in ComponentState) then
      EnforceMinValue;
  end;
end;

procedure TAJCustomValidateEdit.SetMaxValue(NewValue: Double);
begin
  if FMaxValue <> NewValue then
  begin
    FMaxValue := NewValue;
    { make MinValue consistent }
    if FMinValue > FMaxValue then
      FMinValue := FMaxValue;
    if not (csLoading in ComponentState) then
      EnforceMaxValue;
  end;
end;

procedure TAJCustomValidateEdit.SetMinValue(NewValue: Double);
begin
  if FMinValue <> NewValue then
  begin
    FMinValue := NewValue;
    { make MaxValue consistent }
    if FMaxValue < FMinValue then
      FMaxValue := FMinValue;
    if not (csLoading in ComponentState) then
      EnforceMinValue;
  end;
end;

procedure TAJCustomValidateEdit.SetTrimDecimals(const Value: Boolean);
begin
  if Value <> FTrimDecimals then
  begin
    FTrimDecimals := Value;
    if not (csLoading in ComponentState) then
      EditText := Text;
  end;
end;

procedure TAJCustomValidateEdit.WMExit(var Message: TLMExit);
begin
  if ShowModified then
          Font.Color := FFontColor;
end;

procedure TAJCustomValidateEdit.CMEnter(var Message: TLMEnter);
begin
  if ShowModified then
    if (Font.Color <> FFontColor) then
      FFontColor := Font.Color;
  if AutoSelect then
     SelectAll;
end;

procedure TAJCustomValidateEdit.WMPaint(var Msg: TLMPaint);
begin
  PaintEdit;
  inherited;
end;

procedure TAJCustomValidateEdit.CMTextChanged(var Message: TLMessage);
begin
  inherited;
  CheckForInvalidate;
end;

procedure TAJCustomValidateEdit.WMKeyUp(var Message: TLMKeyUp);
begin
  CheckForInvalidate;
end;

procedure TAJCustomValidateEdit.CheckForInvalidate;
begin
  if Required and (Length(Trim(Text)) = 0) then
  begin
    if not FPainted then
      Invalidate;
  end
  else if FPainted then
    Invalidate;
end;

procedure TAJCustomValidateEdit.SetDecimalPlaces(NewValue: Cardinal);
begin
  if ControlState = [csReadingState] then
    FDecimalPlaces := NewValue
  else
  if FDisplayFormat in [dfFloat, dfFloatGeneral, dfPercent] then
    FDecimalPlaces := NewValue;
  if not (csLoading in ComponentState) then
    EditText := Text;
end;

procedure TAJCustomValidateEdit.SetDisplayFormat(NewValue: TAJEditDisplayFormat);
const
  Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  Numbers = '0123456789';
var
  OldFormat: TAJEditDisplayFormat;
begin
  if FDisplayFormat <> NewValue then
  begin
    OldFormat := FDisplayFormat;
    FDisplayFormat := NewValue;
    case FDisplayFormat of
      dfAlphabetic:
        begin
          FCheckChars := Alphabet;
          if FAutoAlignment then
            Alignment := taLeftJustify;
        end;
      dfAlphaNumeric:
        begin
          FCheckChars := Alphabet + Numbers;
          if FAutoAlignment then
            Alignment := taLeftJustify;
        end;
      dfCheckChars, dfNonCheckChars:
        if FAutoAlignment then
          Alignment := taLeftJustify;
      dfCustom, dfNone:
        begin
          if (FDisplayFormat = dfCustom) or not (csLoading in ComponentState) then
            FCheckChars := '';
          if FAutoAlignment then
            Alignment := taLeftJustify;
        end;
      dfFloat, dfFloatGeneral, dfPercent:
        begin
          FCheckChars := Numbers + DecimalSeparator;
          if FAutoAlignment then
            Alignment := taRightJustify;
        end;
      dfInteger:
        begin
          FCheckChars := Numbers;
          if FAutoAlignment then
            Alignment := taRightJustify;
        end;
      dfYear:
        begin
          FCheckChars := Numbers;
          if FAutoAlignment then
            Alignment := taRightJustify;
          MaxLength := 4;
        end;
    end;

    if OldFormat = dfYear then
      MaxLength := 0;

    // Convert non-base 10 numbers to base 10 and base-10 numbers to non-base 10
      // ...or just display the value
      if not (csLoading in ComponentState) then
        EditText := Text;
    end;
end;

procedure TAJCustomValidateEdit.SetModified(const AValue: Boolean);
begin
  if FShowModified then
  begin
    if (avalue = false) then
      self.Font.Color := FFontColor
    else
      self.Font.Color := FModifiedColor;
  end;
 // FIsModified := value;
end;

procedure TAJCustomValidateEdit.SetZeroEmpty(NewValue: Boolean);
begin
  if FZeroEmpty <> NewValue then
  begin
    FZeroEmpty := NewValue;
    if not (csLoading in ComponentState) then
      EditText := Text;
  end;
end;

function TAJCustomValidateEdit.GetAsInteger: Int64;
begin
    Result := StrToInt64Def(Text, 0);
end;

procedure TAJCustomValidateEdit.SetAsInteger(NewValue: Int64);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfCustom,
    dfNonCheckChars, dfNone:
      EditText := IntToStr(NewValue);
    dfFloat, dfFloatGeneral, dfInteger, dfPercent, dfYear:
      EditText := IntToStr(IntRangeValue(NewValue));
  end;
end;

function TAJCustomValidateEdit.GetAsFloat: Double;
begin

    Result := StrToFloatDef(Text, 0);

end;

procedure TAJCustomValidateEdit.SetAsFloat(NewValue: Double);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfCustom,
    dfNonCheckChars, dfNone:
      EditText := FloatToStr(NewValue);
    dfInteger, dfYear:
      EditText := IntToStr(IntRangeValue(Trunc(NewValue)));
    dfFloat, dfPercent:
      EditText := Format('%.*n', [FDecimalPlaces, FloatRangeValue(NewValue)]);
    dfFloatGeneral:
      EditText := Format('%.*g', [FDecimalPlaces, FloatRangeValue(NewValue)]);
  end;
end;

function TAJCustomValidateEdit.GetValue: Variant;
var
  DisplayedText : string;
  Cur: Currency;
begin
  case FDisplayFormat of
    dfFloat, dfFloatGeneral, dfPercent:
      Result := StrToFloatDef(Text, 0);
    dfInteger, dfYear:
      Result := StrToIntDef(Text, 0);
    else
    begin
      DisplayedText := inherited Text;

      // Remove DisplayPrefix and DisplaySuffix
      DisplayedText := StrEnsureNoPrefix(DisplayPrefix, DisplayedText);
      DisplayedText := StrEnsureNoSuffix(DisplaySuffix, DisplayedText);
      Result := DisplayedText;
    end;
  end;
end;

procedure TAJCustomValidateEdit.SetValue(NewValue: Variant);
begin
  case FDisplayFormat of
    dfAlphabetic, dfAlphaNumeric, dfCheckChars, dfNonCheckChars, dfNone, dfCustom:
      EditText := NewValue;
    dfInteger, dfYear:
       SetAsInteger(Integer(NewValue));
    dfFloat, dfFloatGeneral, dfPercent:
      SetAsFloat(NewValue);
  end;
end;

procedure TAJCustomValidateEdit.SetCheckChars(const NewValue: string);
begin
  if (csLoading in ComponentState) or
     ((FDisplayFormat in [dfNone, dfCheckChars, dfNonCheckChars]) and
      (FCheckChars <> NewValue)) then
  begin
    FCheckChars := NewValue;
    EditText := MakeValid(Text);
  end;
end;

function TAJCustomValidateEdit.IsCheckCharsStored: Boolean;
begin
  Result := (FDisplayFormat in [dfNone, dfCheckChars, dfNonCheckChars]);
end;

procedure TAJCustomValidateEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Text, Key, SelStart + 1) and (Key <> #32) and
   (Key <> #8) and (Key <> #13) then
    Key := #0;
  inherited KeyPress(Key);
  //if inherited Modified then SetModified(true);
  SetModified(true);
end;

procedure TAJCustomValidateEdit.LMPaste(var Msg: TLMessage);
begin
  inherited;
  EditText := MakeValid(inherited Text);
end;

function TAJCustomValidateEdit.MakeValid(const ParseString: string): string;
var
  C: Char;
  I: Integer;
  L: Integer;
begin
  SetLength(Result, Length(ParseString));
  L := 0;
  for I := 1 to Length(ParseString) do
  begin
    C := ParseString[I];
    if IsValidChar(Copy(ParseString, 1, I - 1), C, I) then
    begin
      Result[L+1] := C;
      Inc(L);
    end;
  end;
  SetLength(Result, L);
end;

function TAJCustomValidateEdit.IsValidChar(const S: string;
  var Key: Char; Posn: Integer): Boolean;
var
  iPosE: Integer;
begin
  case FDisplayFormat of
     dfCheckChars, dfYear:
      Result := Pos(Key, FCheckChars) > 0;
    dfAlphabetic:
      Result := CharIsAlpha(Key);
    dfAlphaNumeric:
      Result := CharIsAlphaNum(Key);
    dfCustom:
      Result := DoValidate(Key, S, Posn);
    dfInteger:
      Result := (Pos(Key, FCheckChars) > 0) or
        ((Key = '+') and (Posn = 1) and ((Pos('+', S) = 0) or (SelLength > 0))) or
        ((Key = '-') and (Posn = 1) and ((Pos('-', S) = 0) or (SelLength > 0)));
    dfFloat, dfFloatGeneral, dfPercent:
      Result := (Pos(Key, FCheckChars) > 0) or
        ((Key = DecimalSeparator) and (Pos(DecimalSeparator, S) = 0)) or
        ((Key = '+') and (Posn = 1) and ((Pos('+', S) = 0) or (SelLength > 0))) or
        ((Key = '-') and (Posn = 1) and ((Pos('-', S) = 0) or (SelLength > 0)));
    dfNonCheckChars:
      Result := Pos(Key, FCheckChars) = 0;
    dfNone:
      Result := True;
   else
    Result := False;
  end;
end;

function TAJCustomValidateEdit.DoValidate(const Key: Char;
  const AText: string; const Posn: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomValidate) then
    FOnCustomValidate(Self, Key, AText, Posn, Result);
end;

procedure TAJCustomValidateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    EditText := EnterText;
    SelStart := 0;
    SelLength := Length(Text);
  end;
  inherited KeyDown(Key, Shift);
end;

function TAJCustomValidateEdit.CurrRangeValue(CheckValue: Currency): Currency;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := FMaxValue
  else
  if FHasMinValue and (CheckValue < FMinValue) then
    Result := FMinValue;
end;

function TAJCustomValidateEdit.FloatRangeValue(CheckValue: Double): Double;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := FMaxValue
  else
  if FHasMinValue and (CheckValue < FMinValue) then
    Result := FMinValue;
end;

function TAJCustomValidateEdit.IntRangeValue(CheckValue: Int64): Int64;
begin
  Result := CheckValue;
  if FHasMaxValue and (CheckValue > FMaxValue) then
    Result := Trunc(FMaxValue)
  else
  if FHasMinValue and (CheckValue < FMinValue) then
    Result := Trunc(FMinValue);
end;

function TAJCustomValidateEdit.GetEditText: string;
begin
  Result := Text;
end;

procedure TAJCustomValidateEdit.SetEditText(const NewValue: string);
begin
  Text := MakeValid(NewValue);
  if (FDisplayFormat = dfYear) and ((not FHasMaxValue) or
    (FHasMaxValue and (FMaxValue > 2000 + TwoDigitYearCenturyWindow))) and
    ((MaxLength = 0) or (MaxLength > 3)) then
    Text := IntToStr(MakeYear4Digit(StrToIntDef(Text, 0), TwoDigitYearCenturyWindow));
  if (FDisplayFormat in [dfFloat, dfFloatGeneral, dfInteger, dfPercent,
  dfYear]) then
  begin
    EnforceMaxValue;
    EnforceMinValue;
  end;
  DisplayText;
  DoValueChanged;
end;

procedure TAJCustomValidateEdit.ChangeText(const NewValue: string);
var
  S, Exponent: string;
  Ps, I: Integer;
begin
  FSelfChange := True;
  try
    Ps := 0;
    if TrimDecimals then
    begin
      I := Pos('e', LowerCase(NewValue));
      Ps := Pos(DecimalSeparator, NewValue);
      if Ps > 0 then
      begin
        while (I > Ps) and (NewValue[I] = '0') do
          Dec(I);
        if Ps = I then
          Dec(I); // skip decimal separator (Ivo Bauer)
        S := FDisplayPrefix + Copy(NewValue, 1, I) + Exponent + FDisplaySuffix;
      end;
    end;
    if Ps = 0 then
      S := FDisplayPrefix + NewValue + FDisplaySuffix;
    if S <> inherited Text then
      Text := S;
  finally
    FSelfChange := False;
  end;
end;

procedure TAJCustomValidateEdit.PaintEdit;
var
  CC: TControlCanvas;
begin

  if Required and (Length(Trim(Text)) = 0) then
  begin
    FPainted := true;
    CC := TControlCanvas.Create;
    try
      CC.Control := Self;
      CC.Pen.Color := FColorRequired;
      CC.Pen.Width := FRequiredWidth;
      CC.Rectangle(ClientRect);
    finally
      CC.Free;
    end;
  end else FPainted := false;

end;

procedure TAJCustomValidateEdit.SelectAll;
begin
  SelStart:=0;
  SelLength:=Length(Self.Text);
end;

procedure TAJCustomValidateEdit.DisplayText;
begin
  // The number types need to be formatted
  if (not Required) and (Text = '') then
    ChangeText('')
  else
  if (FDisplayFormat in [{dfBinary, dfCurrency,} dfFloat, dfFloatGeneral, dfInteger, {dfOctal,} dfPercent, {dfScientific,} dfYear]) and
    (AsFloat = 0) and FZeroEmpty then
    ChangeText('')
  else
  begin
    case FDisplayFormat of
      dfInteger:
        ChangeText(IntToStr(AsInteger));
      dfFloat:
        ChangeText(Format('%.*n', [FDecimalPlaces, AsFloat]));
      dfFloatGeneral:
        ChangeText(Format('%.*g', [FDecimalPlaces, AsFloat]));
      dfPercent:
        ChangeText(Format('%.*n%', [FDecimalPlaces, AsFloat]));
    else
      ChangeText(Text);
    end;

    // This needs to be done AFTER the text has been changed so that the color
    // is directly shown correctly. (Mantis 3493)
    if (FCriticalPoints.CheckPoints <> cpNone) and
      (FDisplayFormat in [{dfBinary, dfCurrency,} dfFloat, dfFloatGeneral, {dfHex,} dfInteger, {dfOctal,} dfPercent, {dfScientific,} dfYear]) then
      SetFontColor;
  end;
end;

function TAJCustomValidateEdit.GetModified: Boolean;
begin
  Result := fisModified;
end;

function TAJCustomValidateEdit.ScientificStrToFloat(SciString: string): Double;
var
  I: Cardinal;
  sMantissa, sExponent: string;
  bInExp: Boolean;
begin
  if Pos('E', UpperCase(SciString)) = 0 then
    Result := StrToFloatDef(SciString, 0)
  else
  begin
    sMantissa := '';
    sExponent := '';
    bInExp := False;
    for I := 1 to Length(SciString) do
    begin
      if UpperCase(SciString[I]) = 'E' then
        bInExp := True
      else
      begin
        if bInExp then
          sExponent := sExponent + SciString[I]
        else
          sMantissa := sMantissa + SciString[I];
      end;
    end;
    Result := StrToFloatDef(sMantissa, 0) * Power(10, StrToFloatDef(sExponent, 0));
  end;
end;

function TAJCustomValidateEdit.BaseToInt(const BaseValue: string; Base: Byte): Int64;
begin
  Assert(Base <= 36, RsEBaseTooBig);
  Assert(Base > 1, RsEBaseTooSmall);
  Result := Numb2Dec(BaseValue, Base);
end;

function TAJCustomValidateEdit.IntToBase(NewValue:Int64; Base: Byte): string;
begin
  Assert(Base <= 36, RsEBaseTooBig);
  Assert(Base > 1, RsEBaseTooSmall);
  Result := Dec2Numb(NewValue, 0, Base);
end;

procedure TAJCustomValidateEdit.DoValueChanged;
begin
  try
    if Assigned(FOnValueChanged) and (EnterText <> Text) then
      FOnValueChanged(Self);
  finally
    EnterText := Text;
  end;
end;

procedure TAJCustomValidateEdit.Change;
var
  DisplayedText: string;
begin
  // Update FEditText for User changes, so that the AsInteger, etc,
  // functions work while editing
  if not FSelfChange then
  begin
    DisplayedText := inherited Text;
    DisplayedText := StrEnsureNoPrefix(DisplayPrefix, DisplayedText);
    DisplayedText := StrEnsureNoSuffix(DisplaySuffix, DisplayedText);
    Text := DisplayedText;
  end;
  inherited Change;
end;

procedure TAJCustomValidateEdit.SetText(const NewValue: TCaption);
begin
  // If we are actually changing our value ourselves, there is no need
  // to do it again. This may even trigger an infinite recursion, especially
  // when in a derived component the display format is set in the constructor.
  // In that case, the recursion would kill Delphi almost instantly.
  if not FSelfChange then
  begin
    EditText := NewValue;
    DoValueChanged;
  end;
end;

procedure TAJCustomValidateEdit.SetDisplayPrefix(const NewValue: string);
begin
  FDisplayPrefix := NewValue;
  DisplayText;
end;

procedure TAJCustomValidateEdit.SetDisplaySuffix(const NewValue: string);
begin
  FDisplaySuffix := NewValue;
  DisplayText;
end;

procedure TAJCustomValidateEdit.CriticalPointsChange(Sender: TObject);
begin
  SetFontColor;
  Invalidate;
end;

function TAJCustomValidateEdit.IsValid: Boolean;
begin
  Result := True;
  case FCriticalPoints.CheckPoints of
    cpMaxValue:
      Result := IsLower(AsFloat, FCriticalPoints.MaxValue, FCriticalPoints.MaxValueIncluded);
    cpMinValue:
      Result := IsGreater(AsFloat, FCriticalPoints.MinValue, FCriticalPoints.MinValueIncluded);
    cpBoth:
      Result := IsLower(AsFloat, FCriticalPoints.MaxValue, FCriticalPoints.MaxValueIncluded) and
        IsGreater(AsFloat, FCriticalPoints.MinValue, FCriticalPoints.MinValueIncluded);
  end;
  if Assigned(FOnIsValid) then
    FOnIsValid(Self, Result);
end;

procedure TAJCustomValidateEdit.SetFontColor;
begin
  Font.OnChange := nil;
  case FCriticalPoints.CheckPoints of
    cpNone:
      Font.Color := FStandardFontColor;
    cpMinValue:
      if IsLower(AsFloat, FCriticalPoints.MinValue, not FCriticalPoints.MinValueIncluded) then
        Font.Color := FCriticalPoints.ColorBelow
      else
        Font.Color := FStandardFontColor;
    cpMaxValue:
      if IsGreater(AsFloat, FCriticalPoints.MaxValue, not FCriticalPoints.MaxValueIncluded) then
        Font.Color := FCriticalPoints.ColorAbove
      else
        Font.Color := FStandardFontColor;
    cpBoth:
      if IsGreater(AsFloat, FCriticalPoints.MaxValue, not FCriticalPoints.MaxValueIncluded) then
        Font.Color := FCriticalPoints.ColorAbove
      else
      if IsLower(AsFloat, FCriticalPoints.MinValue, not FCriticalPoints.MinValueIncluded) then
        Font.Color := FCriticalPoints.ColorBelow
      else
        Font.Color := FStandardFontColor;
  end;
  Font.OnChange := FontChange;
  Invalidate;
end;

procedure TAJCustomValidateEdit.FontChange(Sender: TObject);
begin
  FStandardFontColor := Font.Color;
  if Assigned(FOldFontChange) then
    FOldFontChange(Sender);
end;

procedure TAJCustomValidateEdit.EnforceMaxValue;
begin
  { Check the Value is within this range }
  if FHasMaxValue and (FDisplayFormat in [{dfBinary, dfCurrency,} dfFloat, dfFloatGeneral,
    {dfHex,} dfInteger, {dfOctal,} dfPercent, {dfScientific,} dfYear]) and
    (AsFloat > FMaxValue) and not FEnforcingMinMaxValue then
  begin
    FEnforcingMinMaxValue := True;
    try
      SetAsFloat(FMaxValue);
    finally
      FEnforcingMinMaxValue := False;
    end;
  end;
end;

procedure TAJCustomValidateEdit.EnforceMinValue;
begin
  { Check the Value is within this range }
  if FHasMinValue and (FDisplayFormat in [{dfBinary, dfCurrency,} dfFloat, dfFloatGeneral,
    {dfHex,} dfInteger, {dfOctal,} dfPercent, {dfScientific,} dfYear]) and
    (AsFloat < FMinValue) and not FEnforcingMinMaxValue  then
  begin
    FEnforcingMinMaxValue := True;
    try
      SetAsFloat(FMinValue);
    finally
      FEnforcingMinMaxValue := False;
    end;
  end;
end;

//=== { TAJEditCriticalPoints } ======================================

constructor TAJEditCriticalPoints.Create;
begin
  inherited Create;
  SetDefaults(cpNone, clBlue, clRed);
  FMaxValueIncluded := False;
  FMinValueIncluded := False;
end;

procedure TAJEditCriticalPoints.SetCheckPoints(NewValue: TAJEditCriticalPointsCheck);
begin
  if FCheckPoints <> NewValue then
  begin
    FCheckPoints := NewValue;
    DoChanged;
  end;
end;

procedure TAJEditCriticalPoints.SetColorAbove(NewValue: TColor);
begin
  if FColorAbove <> NewValue then
  begin
    FColorAbove := NewValue;
    DoChanged;
  end;
end;

procedure TAJEditCriticalPoints.SetColorBelow(NewValue: TColor);
begin
  if FColorBelow <> NewValue then
  begin
    FColorBelow := NewValue;
    DoChanged;
  end;
end;

procedure TAJEditCriticalPoints.SetMaxValue(NewValue: Double);
begin
  if FMaxValue <> NewValue then
  begin
    FMaxValue := NewValue;
    DoChanged;
  end;
end;

procedure TAJEditCriticalPoints.SetMinValue(NewValue: Double);
begin
  if FMinValue <> NewValue then
  begin
    FMinValue := NewValue;
    DoChanged;
  end;
end;

procedure TAJEditCriticalPoints.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TAJEditCriticalPoints.Assign(Source: TPersistent);
var
  LocalSource: TAJEditCriticalPoints;
begin
  if Source is TAJEditCriticalPoints then
  begin
    LocalSource := TAJEditCriticalPoints(Source);
    CheckPoints := LocalSource.CheckPoints;
    ColorAbove := LocalSource.ColorAbove;
    ColorBelow := LocalSource.ColorBelow;
    MaxValue := LocalSource.MaxValue;
    MinValue := LocalSource.MinValue;
  end
  else
    inherited Assign(Source);
end;

function TAJEditCriticalPoints.IsCheckPointsStored: Boolean;
begin
  Result := (FCheckPoints <> FDefCheckPoints);
end;

function TAJEditCriticalPoints.IsColorAboveStored: Boolean;
begin
  Result := (FColorAbove <> FDefColorAbove);
end;

function TAJEditCriticalPoints.IsColorBelowStored: Boolean;
begin
  Result := (FColorBelow <> FDefColorBelow);
end;

procedure TAJEditCriticalPoints.SetDefaults(ACheckPoints: TAJEditCriticalPointsCheck;
  AColorAbove, AColorBelow: TColor);
begin
  FDefCheckPoints := ACheckPoints;
  FCheckPoints := ACheckPoints;
  FDefColorAbove := AColorAbove;
  FColorAbove := AColorAbove;
  FDefColorBelow := AColorBelow;
  FColorBelow := AColorBelow;
end;

initialization
  {$i ajlaz.lrs}


end.

