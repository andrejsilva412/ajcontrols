unit ajselprinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, Spin, ExtCtrls;

type

  { TfrmAJSelPrinter }

  TfrmAJSelPrinter = class(TForm)
    Bevel1: TBevel;
    cmdcancelar: TBitBtn;
    cmdok: TBitBtn;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    lstimpressoras: TListBox;
    spincopias: TSpinEdit;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
    {$ifdef windows}
    FGlassBottonMargin: Integer;
    FGlassLeftMargin: Integer;
    FGlassRightMargin: Integer;
    FGlassTopMargin: Integer;
    property GlassLeftMargin: Integer read FGlassLeftMargin write FGlassLeftMargin;
    property GlassRightMargin: Integer read FGlassRightMargin write FGlassRightMargin;
    property GlassBottonMargin: Integer read FGlassBottonMargin write FGlassBottonMargin;
    property GlassTopMargin: Integer read FGlassTopMargin write FGlassTopMargin;
    {$endif}
  public
    { public declarations }
  end; 

var
  frmAJSelPrinter: TfrmAJSelPrinter;

implementation

uses AJLCLAppUtils;

{ TfrmAJSelPrinter }

procedure TfrmAJSelPrinter.FormActivate(Sender: TObject);
{$ifdef windows}
var
  tmpMargins: TMargins;
{$endif}
begin
  {$ifdef windows}
  if IsWindowsVista then
  begin
    tmpMargins.cxLeftWidth    := GlassLeftMargin;
    tmpMargins.cxRightWidth   := GlassRightMargin;
    tmpMargins.cyBottomHeight := GlassBottonMargin;
    tmpMargins.cyTopHeight    := GlassTopMargin;
    GlassForm(Self, tmpMargins, $00DCDBDA);
  end;
  {$endif}

end;

procedure TfrmAJSelPrinter.FormCreate(Sender: TObject);
begin

  {$ifdef windows}
  if IsWindowsVista then
    Bevel1.Visible := false;
  GlassLeftMargin := 8;
  GlassRightMargin := 8;
  GlassBottonMargin := 48;
  GlassTopMargin := 27;
  {$endif}

end;

procedure TfrmAJSelPrinter.FormPaint(Sender: TObject);
begin
  {$ifdef windows}
  if IsWindowsVista then
  begin
    if (GlassTopMargin > 0) or (GlassRightMargin > 0) or (GlassLeftMargin > 0)
      or (GlassBottonMargin > 0) then
    begin
      Canvas.Brush.Color := clDefault;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(GlassLeftMargin, GlassTopMargin,
        ClientWidth - GlassRightMargin , ClientHeight - GlassBottonMargin);
    end;
  end;
  {$endif}
end;

initialization
  {$I ajselprinter.lrs}

end.

