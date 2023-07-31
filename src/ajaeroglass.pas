// TAeroGlass

unit ajaeroglass;

{$mode delphi}

interface

uses {$ifdef windows}
    Windows,
    {$endif}
    Messages, Classes, Controls, Forms, Graphics, LResources;

{$ifdef windows}
type
  _MARGINS = packed record
    cxLeftWidth    : Integer;
    cxRightWidth   : Integer;
    cyTopHeight    : Integer;
    cyBottomHeight : Integer;
  end;

  PMargins = ^_MARGINS;
  TMargins = _MARGINS;

  DwmIsCompositionEnabledFunc      = function(pfEnabled: PBoolean): HRESULT; stdcall;
  DwmExtendFrameIntoClientAreaFunc = function(destWnd: HWND; const pMarInset: PMargins): HRESULT; stdcall;
  SetLayeredWindowAttributesFunc   = function(destWnd: HWND; cKey: TColor; bAlpha: Byte; dwFlags: DWord): BOOL; stdcall;
{$endif}

type
  { TAeroGlass }

  TAeroGlass = class(TComponent)
  private
    FBlurColorKey: TColor;
    FEnableGlass: Boolean;
    FForm: TForm;
    FMarginBottom: Integer;
    FMarginLeft: Integer;
    FMarginRight: Integer;
    FMarginTop: Integer;
    FSheetOfGlass: Boolean;
    procedure SetBlurColorKey(AValue: TColor);
    procedure SetEnableGlass(AValue: Boolean);
    procedure SetMarginBottom(AValue: Integer);
    procedure SetMarginLeft(AValue: Integer);
    procedure SetMarginRight(AValue: Integer);
    procedure SetMarginTop(AValue: Integer);
    procedure SetSheetOfGlass(AValue: Boolean);
    {$ifdef windows}
    procedure GlassForm(frm: TForm; tmpMargins: TMargins; cBlurColorKey: TColor = $00DCDBDA);
    function IsWindowsVista: Boolean;
    procedure NewActivate(Sender: TObject);
    {$endif}
    procedure NewOnPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EnableGlass: Boolean read FEnableGlass write SetEnableGlass;
    property BlurColorKey: TColor read FBlurColorKey write SetBlurColorKey;
    property MarginLeft: Integer read FMarginLeft write SetMarginLeft;
    property MarginTop: Integer read FMarginTop write SetMarginTop;
    property MarginRight: Integer read FMarginRight write SetMarginRight;
    property MarginBottom: Integer read FMarginBottom write SetMarginBottom;
    property SheetOfGlass: Boolean read FSheetOfGlass write SetSheetOfGlass;
  end;

procedure Register;

implementation

procedure TAeroGlass.SetBlurColorKey(AValue: TColor);
begin
  if FBlurColorKey = AValue then Exit;
  FBlurColorKey := AValue;
end;

procedure TAeroGlass.SetEnableGlass(AValue: Boolean);
begin
  if FEnableGlass = AValue then Exit;
  FEnableGlass := AValue;
end;

procedure TAeroGlass.SetMarginBottom(AValue: Integer);
begin
  if FMarginBottom = AValue then Exit;
  FMarginBottom := AValue;
end;

procedure TAeroGlass.SetMarginLeft(AValue: Integer);
begin
  if FMarginLeft = AValue then Exit;
  FMarginLeft := AValue;
end;

procedure TAeroGlass.SetMarginRight(AValue: Integer);
begin
  if FMarginRight = AValue then Exit;
  FMarginRight := AValue;
end;

procedure TAeroGlass.SetMarginTop(AValue: Integer);
begin
  if FMarginTop = AValue then Exit;
  FMarginTop := AValue;
end;

procedure TAeroGlass.SetSheetOfGlass(AValue: Boolean);
begin
  if FSheetOfGlass = AValue then Exit;
  FSheetOfGlass := AValue;
  FMarginBottom := -1;
  FMarginLeft := -1;
  FMarginTop := -1;
  FMarginRight := -1;
end;

{$ifdef windows}
procedure TAeroGlass.GlassForm(frm: TForm; tmpMargins: TMargins;
  cBlurColorKey: TColor);
var
  hDwmDLL: Cardinal;
  fDwmIsCompositionEnabled: DwmIsCompositionEnabledFunc;
  fDwmExtendFrameIntoClientArea: DwmExtendFrameIntoClientAreaFunc;
  fSetLayeredWindowAttributesFunc: SetLayeredWindowAttributesFunc;
  bCmpEnable: Boolean;
  mgn: TMargins;
begin

  if csDesigning in ComponentState then
  begin


  end else begin
    { Continue if Windows version is compatible }
    if IsWindowsVista then begin
      { Continue if 'dwmapi' library is loaded }
      hDwmDLL := LoadLibrary('dwmapi.dll');
      if hDwmDLL <> 0 then begin
        { Get values }
        @fDwmIsCompositionEnabled        := GetProcAddress(hDwmDLL, 'DwmIsCompositionEnabled');
        @fDwmExtendFrameIntoClientArea   := GetProcAddress(hDwmDLL, 'DwmExtendFrameIntoClientArea');
        @fSetLayeredWindowAttributesFunc := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
        { Continue if values are <> nil }
        if (
        (@fDwmIsCompositionEnabled <> nil) and
        (@fDwmExtendFrameIntoClientArea <> nil) and
        (@fSetLayeredWindowAttributesFunc <> nil)
        )
        then begin
          { Continue if composition is enabled }
          fDwmIsCompositionEnabled(@bCmpEnable);
          if bCmpEnable = True then begin
            { Set Form Color same as cBlurColorKey }
            frm.Color := cBlurColorKey;
            { ... }
            SetWindowLong(frm.Handle, GWL_EXSTYLE, GetWindowLong(frm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
            { ... }
            fSetLayeredWindowAttributesFunc(frm.Handle, cBlurColorKey, 0, LWA_COLORKEY);
            { Set margins }
            ZeroMemory(@mgn, SizeOf(mgn));
            mgn.cxLeftWidth    := tmpMargins.cxLeftWidth;
            mgn.cxRightWidth   := tmpMargins.cxRightWidth;
            mgn.cyTopHeight    := tmpMargins.cyTopHeight;
            mgn.cyBottomHeight := tmpMargins.cyBottomHeight;
            { Extend Form }
            fDwmExtendFrameIntoClientArea(frm.Handle,@mgn);
          end;
        end;
        { Free loaded 'dwmapi' library }
        FreeLibrary(hDWMDLL);
      end;
    end;
  end;

end;

function TAeroGlass.IsWindowsVista: Boolean;
var
  osVinfo: TOSVERSIONINFO;
begin
  ZeroMemory(@osVinfo, SizeOf(osVinfo));
  OsVinfo.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
  if (
  (GetVersionEx(osVInfo)   = True) and
  (osVinfo.dwPlatformId    = VER_PLATFORM_WIN32_NT) and
  (osVinfo.dwMajorVersion >= 6)
  )
  then Result:=True
  else Result:=False

end;

procedure TAeroGlass.NewActivate(Sender: TObject);
var
  tmpMartins: TMargins;
begin
  if EnableGlass then
  begin
    if SheetOfGlass then
    begin
      tmpMartins.cxLeftWidth := -1;
      tmpMartins.cyTopHeight := -1;
      tmpMartins.cyBottomHeight := -1;
      tmpMartins.cxRightWidth := -1;
    end else begin
      tmpMartins.cxLeftWidth := MarginLeft;
      tmpMartins.cxRightWidth := MarginRight;
      tmpMartins.cyBottomHeight := MarginBottom;
      tmpMartins.cyTopHeight := MarginTop;
    end;
    GlassForm(FForm, tmpMartins, FBlurColorKey);
  end;
end;


{$endif}

procedure TAeroGlass.NewOnPaint(Sender: TObject);
begin
  {$ifdef windows}
  if IsWindowsVista then
  begin
    if not SheetOfGlass then
      if (MarginTop > 0) or (MarginRight > 0) or (MarginLeft > 0)
        or (MarginBottom > 0) then
      begin
        FForm.Canvas.Brush.Color := clDefault;
        FForm.Canvas.Brush.Style := bsSolid;
        FForm.Canvas.FillRect(MarginLeft, MarginTop,
        FForm.ClientWidth - MarginRight, FForm.ClientHeight - MarginBottom);
      end;
  end;
  {$endif}
end;

constructor TAeroGlass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := TForm(GetParentForm(TControl(AOwner)));
  FForm.OnPaint := NewOnPaint;
  {$ifdef windows}
  FForm.OnActivate := NewActivate;
  FBlurColorKey := $00DCDBDA;
  {$else}
  FBlurColorKey := clDefault;
  {$endif}
  FEnableGlass := false;
  FSheetOfGlass := false;
  FMarginBottom := 48;
  FMarginLeft := 8;
  FMarginRight := 8;
  FMarginTop := 27;
end;

destructor TAeroGlass.Destroy;
begin
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('AJCtrls Vista Style', [TAeroGlass]);
end;

initialization
  {$i ajlaz.lrs}

end.
