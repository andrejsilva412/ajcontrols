{ *********************************************************************** }
{                                                                         }
{                          VDO Print Components                           }
{                             TVDOCaPrinter                               }
{              Copyright (C) 2003-2006 Vinicius de Oliveira               }
{                                                                         }
{                       Version 3.1.0 - 07/06/2006                        }
{                                                                         }
{ *********************************************************************** }

{ *********************************************************************** }
{  License Agreement:                                                     }
{                                                                         }
{  The contents of this file are subject to the Mozilla Public License    }
{  Version 1.1 (the "License"); you may not use this file except in       }
{  compliance with the License. You may obtain a copy of the License at   }
{  http://www.mozilla.org/MPL/                                            }
{                                                                         }
{  Software distributed under the License is distributed on an "AS IS"    }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See    }
{  the License for the specific language governing rights and limitations }
{  under the License.                                                     }
{                                                                         }
{  The Original Code is VDOCaPrinter.pas                                  }
{                                                                         }
{  The Initial Developer of the Original Code is Vinicius de Oliveira.    }
{  All Rights Reserved.                                                   }
{                                                                         }
{  Contact :                                                              }
{     vncsoliveira@yahoo.com.br                                           }
{     http://vdo.sourceforge.net                                          }
{ *********************************************************************** }

//{$D-}

unit AJCaPrinter;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Dialogs, Printers, AJBasePrinter,
  LResources;

type
  TAJCaFont = class(TPersistent)
  private
    FAlign: TAlignment;
    FColor: TColor;
    FName: TFontName;
    FPitch: TFontPitch;
    FSize: Integer;
    FStyle: TFontStyles;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Align: TAlignment read FAlign write FAlign;
    property Color: TColor read FColor write FColor;
    property Name: TFontName read FName write FName;
    property Pitch: TFontPitch read FPitch write FPitch;
    property Size: Integer read FSize  write FSize;
    property Style: TFontStyles read FStyle write FStyle;
  end;
  { **** }
  TAJCaPaper = class(TPersistent)
  private
    FAutoNewPage: Boolean;
    FColumns: Integer;
    FLines: Integer;
    FMarginLeft: Integer;
    FMarginRight: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AutoNewPage: Boolean read FAutoNewPage write FAutoNewPage;
    property Columns: Integer read FColumns write FColumns;
    property Lines: Integer read FLines write FLines;
    property MarginLeft: Integer read FMarginLeft write FMarginLeft;
    property MarginRight: Integer read FMarginRight write FMarginRight;
  end;
  { **** }
  TAJCaSets = class(TPersistent)
  private
    FFont: TAJCaFont;
    FLineSpacing: Integer;
    FPaper: TAJCaPaper;
    FOrientation: TPrinterOrientation;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Font: TAJCaFont read FFont  write FFont;
    property LineSpacing: Integer read FLineSpacing write FLineSpacing;
    property Paper: TAJCaPaper read FPaper write FPaper;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation;
  end;
  { **** }

  { TAJCaPrinter }

  TAJCaPrinter = class(TAJBasePrinter)
  private
    { Variáveis internas }
    FColPos: Integer;
    FIncLineSpace: Integer;
    FLinePos: Integer;
   // FPrinter: TPrinter;
    FTextY: Integer;
    { Variáveis p/ propriedades Read / Write publicadas }
    FFont: TAJCaFont;
    FLineSpacing: Integer;
    FOrientation: TPrinterOrientation;
    FPaper: TAJCaPaper;
    { Procedures e Functions internas }
    procedure InitInternalVariables;
    procedure DefinePrinter;
    procedure DefineFont;
    procedure WriteOnPrinter(AColumn: Integer; AText: string);
    { Procedures e functions internas p/ o Preview }
    procedure ParsePreviewFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Métodos }
    procedure Abort;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewLine(ALines: Integer = 1);
    procedure NewPage(APages: Integer = 1);
    procedure Print(AColumn: Integer; AText: string;
      ALineFeed: Boolean = False); overload;
    procedure Print(ALine, AColumn: Integer; AText: string;
      ALineFeed: Boolean = False); overload;
    procedure Replicate(AColumn, ACount: Integer;
      AChar: Char = '-'; ALineFeed: Boolean = True); overload;
    procedure Replicate(ALine, AColumn, ACount: Integer;
      AChar: Char = '-'; ALineFeed: Boolean = True); overload;
    procedure WriteStr(AColumn: Integer; AText: string);
  published
    { Propriedades Read / Write }
    property Font: TAJCaFont read FFont write FFont;
    property LineSpacing: Integer read FLineSpacing write FLineSpacing;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation;
    property Paper: TAJCaPaper read FPaper write FPaper;
  end;

procedure Register;

implementation

uses
  Controls, AJPrintConsts, {VDOPreview,} AJPrintProgress, ajselprinter;

procedure Register;
begin
  RegisterComponents('AJCtrls Print', [TAJCaPrinter]);
end;

{ *********************************************************************** }
{                                                                         }
{                             TAJCaFont                                  }
{                                                                         }
{ *********************************************************************** }

constructor TAJCaFont.Create;
begin
  inherited;
  FAlign := taLeftJustify;
  FColor := clBlack;
  FName := 'Courier New';
  FPitch := fpFixed;
  FSize := 10;
  FStyle := [];
end;

destructor TAJCaFont.Destroy;
begin
  inherited;
end;

{ *********************************************************************** }
{                                                                         }
{                             TAJCaPaper                                 }
{                                                                         }
{ *********************************************************************** }

constructor TAJCaPaper.Create;
begin
  inherited;
  FAutoNewPage := False;
  FColumns := 119;
  FLines := 72;
  FMarginLeft := 0;
  FMarginRight := 0;
end;

destructor TAJCaPaper.Destroy;
begin
  inherited;
end;

{ *********************************************************************** }
{                                                                         }
{                             TAJCaSets                                  }
{                                                                         }
{ *********************************************************************** }

constructor TAJCaSets.Create;
begin
  inherited;
  FFont := TAJCaFont.Create;
  FLineSpacing := 10;
  FPaper := TAJCaPaper.Create;
  FOrientation := poPortrait;
end;

destructor TAJCaSets.Destroy;
begin
  FFont.Free;
  FPaper.Free;
  inherited;
end;

{ *********************************************************************** }
{                                                                         }
{                            TAJCaPrinter                                }
{                                                                         }
{ *********************************************************************** }

constructor TAJCaPrinter.Create(AOwner: TComponent);
begin
  inherited;
  InitInternalVariables;
  { **** }
  FFont := TAJCaFont.Create;
  FLineSpacing := 10;
  FOrientation := poPortrait;
  FPaper := TAJCaPaper.Create;
end;

destructor TAJCaPrinter.Destroy;
begin
  if FIsPrinting then Abort;
  FFont.Free;
  FPaper.Free;
  inherited;
end;

{ *********************************************************************** }
{                     Procedures e functions internas                     }
{ *********************************************************************** }

procedure TAJCaPrinter.InitInternalVariables;
begin
  FLastCol := 0;
  FTextBuf := '';
end;

procedure TAJCaPrinter.DefinePrinter;
var
  Imp: String;
begin

     if Printer.Printers.Count > 0 then
     begin
          frmAJSelPrinter := TfrmAJSelPrinter.Create(nil);
          try
             frmAJSelPrinter.lstimpressoras.Items := Printer.Printers;
             frmAJSelPrinter.lstimpressoras.ItemIndex:=0;
             frmAJSelPrinter.ShowModal;
             Imp := frmAJSelPrinter.lstimpressoras.Items[
                  frmAJSelPrinter.lstimpressoras.ItemIndex];
             Printer.PrinterIndex:=frmAJSelPrinter.lstimpressoras.ItemIndex;
             Printer.SetPrinter(imp);
             PrinterName := imp;
             Printer.Copies := frmAJSelPrinter.spincopias.Value;
          finally
             frmAJSelPrinter.Free;
          end;
      end else begin
          MessageDlg('Atenção', 'Nenhuma impressora esta instalada.',
           mtWarning, [mbOK, mbCancel], 0);
      end;
end;

procedure TAJCaPrinter.DefineFont;
begin
  with Printer.Canvas.Font do
  begin
    Color := FFont.Color;
    Name := FFont.Name;
    Size := FFont.Size;
    Style := FFont.Style;
  end;

end;

procedure TAJCaPrinter.WriteOnPrinter(AColumn: Integer; AText: string);
begin
  if not FIsPrinting then
    RaiseError(Format(SNotPrinting, [PrinterName]));
  try
    Printer.Canvas.TextOut(FColPos, FLinePos, AText);
    FTextY := Printer.Canvas.TextHeight(#32);
  except
    on E: Exception do RaiseError(E.Message);
  end;
end;

{ *********************************************************************** }
{               Procedures e functions internas p/ o Preview              }
{ *********************************************************************** }

//var
//  PreviewFont: TVDOPreviewFont;

procedure TAJCaPrinter.ParsePreviewFont;
begin
{  with PreviewFont do
  begin
    Color := FFont.Color;
    Name := FFont.Name;
    Size := FFont.Size;
    Style := FFont.Style;
  end; }
end;

{ *********************************************************************** }
{                                 Métodos                                 }
{ *********************************************************************** }

procedure TAJCaPrinter.Abort;
begin
  if not FIsPrinting then Exit;
 // if ShowPreview then Exit;
  try
    //FPrinter.Abort;
    Printer.Abort;

    //FIsPrinting := FPrinter.Printing;
    FIsPrinting := Printer.Printing;

    if not FromPreview then
      if Assigned(OnAbort) then OnAbort(Self);
    if ShowProgress then ClosePrintProgress;
  except
    on E: Exception do RaiseError(E.Message);
  end;
end;

procedure TAJCaPrinter.BeginDoc;
begin
  FCurrentLine := 1;
  FCurrentPage := 1;
  if FIsPrinting then
    RaiseError(Format(SPrinting, [PrinterName]));
 // if ShowPreview then
  //begin
   // BeginPreviewDoc;
//    frmVDOPreview.CaPrinter := Self;
   // Exit;
  //end;
  { **** }
//   PrinterName:=Printer.Printers[Printer.PrinterIndex];
  {if not (PrinterName = '') then
  begin
    if IsValidPrinter(PrinterName) then
      Printer.PrinterIndex := Printer.Printers.IndexOf(PrinterName)
    else
    begin
      MessageDlg(Format(SInvalidPrinter, [PrinterName]), mtError, [mbOK], 0);
      ShowDialog := True;
    end;
  end;  }
  DefinePrinter;

  Printer.Orientation:=FOrientation;
  Printer.Title:=Title;
  DefineFont;
  FIncLineSpace := FTextY + FLineSpacing;
  FLinePos := FTextY + FLineSpacing;

  if PrinterName = '' then
    RaiseError(SNoDefaultPrinter);
  try
  Printer.BeginDoc;
  FIsPrinting := Printer.Printing;
  if not FromPreview then
  begin
   if Assigned(OnStart) then OnStart(Self);
   if Assigned(OnNewPage) then OnNewPage(Self);
  end;
  if ShowProgress then ShowPrintProgress;
  except
    on E: Exception do RaiseError(E.Message);
  end;
end;

procedure TAJCaPrinter.EndDoc;
begin
  if not FIsPrinting then Exit;
  InitInternalVariables;
 { if ShowPreview then
  begin
   // BeginPreviewDoc;
    EndPreviewDoc;
    Exit;
  end; }
  try
   // FPrinter.EndDoc;
    Printer.EndDoc;

//    FIsPrinting := FPrinter.Printing;
    FIsPrinting := Printer.Printing;

    if not FromPreview then
      if Assigned(OnTerminate) then OnTerminate(Self);
    if ShowProgress then ClosePrintProgress;
  except
    on E: Exception do RaiseError(E.Message);
  end;
end;

procedure TAJCaPrinter.NewLine(ALines: Integer = 1);
var
  I: Integer;
begin
  if not FIsPrinting then Exit;
  FIncLineSpace := FTextY + FLineSpacing;
  for I := 1 to ALines do
  begin
    Inc(FLinePos, FIncLineSpace);
    //if ShowPreview then
      //frmVDOPreview.NewLine;
    Inc(FCurrentLine);
    if not FromPreview then
      if Assigned(OnNewLine) then OnNewLine(Self);
    if (FPaper.AutoNewPage) and (FPaper.Lines > 0) then
      if FCurrentLine > FPaper.Lines then
        NewPage;
  end;
  FLastCol := 0;
end;

procedure TAJCaPrinter.NewPage(APages: Integer = 1);
var
  I: Integer;
begin
  if not FIsPrinting then Exit;
  for I := 1 to APages do
  begin
    try
      //if not ShowPreview then
       // FPrinter.NewPage
        Printer.NewPage;

      //else frmVDOPreview.NewPage;
      Inc(FCurrentPage);
      FCurrentLine := 1;
      FLinePos := FTextY + FLineSpacing;
      if not FromPreview then
        if Assigned(OnNewPage) then OnNewPage(Self);
    except
      on E: Exception do RaiseError(E.Message);
    end;
  end;
end;

procedure TAJCaPrinter.Print(AColumn: Integer; AText: string;
  ALineFeed: Boolean = False);
var
  Pad: string;
  Cols: Integer;
begin
  if not FIsPrinting then Exit;
  if not ShowPreview then
  begin
    DefineFont;
//    FColPos := FPrinter.Canvas.TextWidth(#32) * (AColumn + FPaper.FMarginLeft);
    FColPos := Printer.Canvas.TextWidth(#32) * (AColumn + FPaper.FMarginLeft);

  end
  else
  begin
    Pad := StringOfChar(#32, AColumn - FLastCol);
    WriteStr(FLastCol, Pad);
  end;
  FTextBuf := AText;
  if FPaper.Columns > 0 then
  begin
    Cols := FPaper.Columns - FPaper.MarginLeft - FPaper.MarginRight;
    case FFont.Align of
      taCenter: FTextBuf := StringOfChar(#32, (Cols - Length(FTextBuf)) div 2) + FTextBuf;
      taRightJustify: FTextBuf := StringOfChar(#32, (Cols - Length(FTextBuf))) + FTextBuf;
    end;
  end;
  WriteStr(AColumn, FTextBuf);
  FLastCol := AColumn + Length(FTextBuf);
  FTextBuf := '';
  if not FromPreview then
    if Assigned(OnPrint) then OnPrint(Self);
  if ALineFeed then
    NewLine;
end;

procedure TAJCaPrinter.Print(ALine, AColumn: Integer; AText: string;
  ALineFeed: Boolean = False);
begin
  NewLine(ALine - FCurrentLine);
  Print(AColumn, AText, ALineFeed);
end;

procedure TAJCaPrinter.Replicate(AColumn, ACount: Integer;
  AChar: Char = '-'; ALineFeed: Boolean = True);
begin
  Print(AColumn, StringOfChar(AChar, ACount), ALineFeed);
end;

procedure TAJCaPrinter.Replicate(ALine, AColumn, ACount: Integer;
  AChar: Char = '-'; ALineFeed: Boolean = True);
begin
  Print(ALine, AColumn, StringOfChar(AChar, ACount), ALineFeed);
end;

procedure TAJCaPrinter.WriteStr(AColumn: Integer; AText: string);
begin
//  if ShowPreview then
  //begin
   // ParsePreviewFont;
  //  frmVDOPreview.Write(AText, AColumn, PreviewFont);
 // end
//  else
   WriteOnPrinter(AColumn, AText);
end;

initialization
  {$i ajlaz.lrs}

end.
