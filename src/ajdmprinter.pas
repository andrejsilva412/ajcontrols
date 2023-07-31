{ *********************************************************************** }
{                                                                         }
{                          VDO Print Components                           }
{                             TVDODmPrinter                               }
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
{  The Original Code is VDODmPrinter.pas                                  }
{                                                                         }
{  The Initial Developer of the Original Code is Vinicius de Oliveira.    }
{  All Rights Reserved.                                                   }
{                                                                         }
{  Contact :                                                              }
{     vncsoliveira@yahoo.com.br                                           }
{     http://vdo.sourceforge.net                                          }
{ *********************************************************************** }

//{$D-}

unit AJDmPrinter;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows} Windows, WinSpool, {$endif} SysUtils, Classes, Graphics,
  Dialogs, Printers, AJBasePrinter, LResources;

type
  TAJDmCharMode = (cmClear, cmNormal);
  TAJDmFontAlign = (faCenter, faLeft, faRight);
  TAJDmFontType = (ftDraft, ftNLQ);
  TAJDmNLQFont = (ntRoman, ntSansSerif);
  TAJDmFontPitch = (fp10cpp, fp12cpp);
  TAJDmFontSize = (fsCondensed, fsDefault, fsLargeCondensed, fsLarge);
  TAJDmFontStyle = (fsDmBold, fsDmDoubleStrike, fsDmItalic, fsDmSuperScript,
    fsDmSubScript, fsDmUnderline);
  TAJDmFontStyles = set of TAJDmFontStyle;
  TAJDmLineSpacing = (ls6lpp, ls8lpp, lsCustom);
  { **** }
  TAJDmFont = class(TPersistent)
  private
    FAlign: TAJDmFontAlign;
    FFontType: TAJDmFontType;
    FNLQFont: TAJDmNLQFont;
    FPitch: TAJDmFontPitch;
    FSize: TAJDmFontSize;
    FStyle: TAJDmFontStyles;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Align: TAJDmFontAlign read FAlign write FAlign;
    property FontType: TAJDmFontType read FFontType write FFontType;
    property NLQFont: TAJDmNLQFont read FNLQFont write FNLQFont;
    property Pitch: TAJDmFontPitch read FPitch write FPitch;
    property Size: TAJDmFontSize read FSize write FSize;
    property Style: TAJDmFontStyles read FStyle write FStyle;
  end;
  { **** }
  TAJDmPaper = class(TPersistent)
  private
    FAutoNewPage: Boolean;
    FAutoNewPageLines: Integer;
    FColumns: Integer;
    FLines: Integer;
    FMarginLeft: Integer;
    FMarginRight: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AutoNewPage: Boolean read FAutoNewPage write FAutoNewPage;
    property AutoNewPageLines: Integer read FAutoNewPageLines write FAutoNewPageLines;
    property Columns: Integer read FColumns write FColumns;
    property Lines: Integer read FLines write FLines;
    property MarginLeft: Integer read FMarginLeft write FMarginLeft;
    property MarginRight: Integer read FMarginRight write FMarginRight;
  end;
  { **** }
  TAJDmSets = class(TPersistent)
  private
    FCharMode: TAJDmCharMode;
    FFont: TAJDmFont;
    FLineSpacing: TAJDmLineSpacing;
    FLineSpacingCustomValue: Byte;
    FPaper: TAJDmPaper;
    procedure SetLineSpacingCustomValue(const Value: Byte);
    procedure SetLineSpacing(const Value: TAJDmLineSpacing);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property CharMode: TAJDmCharMode read FCharMode write FCharMode;
    property Font: TAJDmFont read FFont write FFont;
    property LineSpacing: TAJDmLineSpacing read FLineSpacing write SetLineSpacing;
    property LineSpacingCustomValue: Byte read FLineSpacingCustomValue write SetLineSpacingCustomValue;
    property Paper: TAJDmPaper read FPaper write FPaper;
  end;
  { **** }

  { TAJDmPrinter }

  TAJDmPrinter = class(TAJBasePrinter)
  private
    { Variáveis internas }
    FEscape: string;
    FLastEsc: string;
    FPrinterHandle: DWord;
    FPrinterIndex: Integer;
    {$ifdef windows}
    FDevMode: PDevMode;
    {$endif}
    { Variáveis p/ propriedades Read / Write publicadas }
    FCharMode: TAJDmCharMode;
    FFont: TAJDmFont;
    FLineSpacing: TAJDmLineSpacing;
    FLineSpacingCustomValue: Byte;
    FPaper: TAJDmPaper;
    FShowCopies: Boolean;
    { Procedures e Functions Get/Set de propriedades  }
    procedure SetLineSpacing(const Value: TAJDmLineSpacing);
    procedure SetLineSpacingCustomValue(const Value: Byte);
    { Procedures e Functions internas }
    procedure InitInternalVariables;
    procedure DefinePrinter;
    procedure DefinePaper;
    procedure DefineLineSpacing(ASend: Boolean = False);
    procedure DefineFont;
    function CleanText(S: string): string;
    procedure WriteOnPrinter(AText: string);
    { Procedures e functions internas p/ o Preview }
    procedure ParsePreviewFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Métodos }
    procedure Abort;
    procedure BeginDoc;
    procedure EndDoc(EjectPage: Boolean = True);
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
    procedure WriteStr(AText: string; APreviewText: string = '');
  published
    { Propriedades Read / Write }
    property CharMode: TAJDmCharMode read FCharMode write FCharMode;
    property Font: TAJDmFont read FFont write FFont;
    property LineSpacing: TAJDmLineSpacing read FLineSpacing write SetLineSpacing;
    property LineSpacingCustomValue: Byte read FLineSpacingCustomValue write SetLineSpacingCustomValue;
    property Paper: TAJDmPaper read FPaper write FPaper;
    property ShowCopies: Boolean read FShowCopies write FShowCopies;
  end;

procedure Register;

implementation

uses
  IniFiles, Controls, AJPrintConsts, {VDOPreview,} AJPrintProgress, ajselprinter;

procedure Register;
begin
  RegisterComponents('AJCtrls Print', [TAJDmPrinter]);
end;

{ *********************************************************************** }
{                                                                         }
{                             TAJDmFont                                  }
{                                                                         }
{ *********************************************************************** }

constructor TAJDmFont.Create;
begin
  inherited;
  FAlign := faLeft;
  FFontType := ftDraft;
  FNLQFont := ntSansSerif;
  FSize := fsDefault;
  FStyle := [];
  FPitch := fp10cpp;
end;

destructor TAJDmFont.Destroy;
begin
  inherited;
end;

{ *********************************************************************** }
{                                                                         }
{                             TAJDmPaper                                 }
{                                                                         }
{ *********************************************************************** }

constructor TAJDmPaper.Create;
begin
  inherited;
  FAutoNewPage := False;
  FAutoNewPageLines := 3;
  FColumns := 80;
  FLines := 66;
  FMarginLeft := 0;
  FMarginRight := 0;
end;

destructor TAJDmPaper.Destroy;
begin
  inherited;
end;

{ *********************************************************************** }
{                                                                         }
{                             TAJDmSets                                  }
{                                                                         }
{ *********************************************************************** }

constructor TAJDmSets.Create;
begin
  inherited;
  FCharMode := cmClear;
  FFont := TAJDmFont.Create;
  FLineSpacing := ls6lpp;
  FLineSpacingCustomValue := 36;
  FPaper := TAJDmPaper.Create;
end;

destructor TAJDmSets.Destroy;
begin
  FFont.Free;
  FPaper.Free;
  inherited;
end;

procedure TAJDmSets.SetLineSpacing(const Value: TAJDmLineSpacing);
begin
  case Value of
    ls6lpp: FLineSpacingCustomValue := 36;
    ls8lpp: FLineSpacingCustomValue := 27;
  end;
  FLineSpacing := Value;
end;

procedure TAJDmSets.SetLineSpacingCustomValue(const Value: Byte);
begin
  case Value of
    36: FLineSpacing := ls6lpp;
    27: FLineSpacing := ls8lpp;
    else
      FLineSpacing := lsCustom;
  end;
  FLineSpacingCustomValue := Value;
end;

{ *********************************************************************** }
{                                                                         }
{                            TAJDmPrinter                                }
{                                                                         }
{ *********************************************************************** }

constructor TAJDmPrinter.Create(AOwner: TComponent);
begin
  inherited;
  FPrinterIndex := 0;
  InitInternalVariables;
  { **** }
  FCharMode := cmClear;
  FFont := TAJDmFont.Create;
  FLineSpacing := ls6lpp;
  FLineSpacingCustomValue := 36;
  FPaper := TAJDmPaper.Create;
end;

destructor TAJDmPrinter.Destroy;
var
  ini: TIniFile;
begin
  if FIsPrinting then Abort;
  FFont.Free;
  FPaper.Free;
  inherited;
end;

{ *********************************************************************** }
{                      Get/Set de propriedades                            }
{ *********************************************************************** }

procedure TAJDmPrinter.SetLineSpacing(const Value: TAJDmLineSpacing);
begin
  case Value of
    ls6lpp: FLineSpacingCustomValue := 36;
    ls8lpp: FLineSpacingCustomValue := 27;
  end;
  FLineSpacing := Value;
end;

procedure TAJDmPrinter.SetLineSpacingCustomValue(const Value: Byte);
begin
  case Value of
    36: FLineSpacing := ls6lpp;
    27: FLineSpacing := ls8lpp;
    else
      FLineSpacing := lsCustom;
  end;
  FLineSpacingCustomValue := Value;
end;

{ *********************************************************************** }
{                     Procedures e functions internas                     }
{ *********************************************************************** }

procedure TAJDmPrinter.InitInternalVariables;
var
  ini: TIniFile;
begin
  FEscape := '';
  FLastCol := 0;
  FLastEsc := '';
  FTextBuf := '';
  ini := TIniFile.Create('dmprinter.ini');
  try
    if FPrinterIndex <> 0 then { a impressora mudou }
     ini.WriteInteger('PRINTER', 'INDEX', FPrinterIndex)
    else FPrinterIndex := ini.ReadInteger('PRINTER', 'INDEX', 0);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TAJDmPrinter.DefinePrinter;
var
   imp: String;
begin
  if Printer.Printers.Count > 0 then
     begin
          frmAJSelPrinter := TfrmAJSelPrinter.Create(nil);
          try
             frmAJSelPrinter.lstimpressoras.Items := Printer.Printers;
             frmAJSelPrinter.lstimpressoras.ItemIndex:=0;
             if not ShowCopies then
             begin
               frmAJSelPrinter.Label1.Enabled := false;
               frmAJSelPrinter.spincopias.Enabled := false;
             end;
             frmAJSelPrinter.lstimpressoras.ItemIndex := FPrinterIndex;
             if frmAJSelPrinter.ShowModal = mrOK then
             begin
               Imp := frmAJSelPrinter.lstimpressoras.Items[
                    frmAJSelPrinter.lstimpressoras.ItemIndex];
               Printer.PrinterIndex:=frmAJSelPrinter.lstimpressoras.ItemIndex;
               FPrinterIndex := Printer.PrinterIndex;
               PrinterName := imp;
               Printer.Copies := frmAJSelPrinter.spincopias.Value;
             end else PrinterName := '';
          finally
             frmAJSelPrinter.Free;
          end;
      end else begin
          MessageDlg('Atenção', 'Nenhuma impressora esta instalada.',
           mtWarning, [mbOK, mbCancel], 0);
      end;
end;

procedure TAJDmPrinter.DefinePaper;
begin
  with FPaper do
  begin
    if Lines > 0 then
    begin
      WriteStr(EscPaperSize + Chr(Lines));
      if AutoNewPage then
        WriteStr(EscAutoNewPageOn + Chr(AutoNewPageLines))
      else WriteStr(EscAutoNewPageOff);
    end;
    if MarginLeft > 0 then WriteStr(EscMarginLeft + Chr(MarginLeft));
    if MarginRight > 0 then WriteStr(EscMarginRight + Chr(MarginRight));
  end;
end;

procedure TAJDmPrinter.DefineLineSpacing(ASend: Boolean = False);
begin
  case FLineSpacing of
    ls6lpp:
    	begin
        if not ASend then
	    		FEscape := FEscape + Esc6lpp
        else WriteStr(Esc6lpp);
      end;
    ls8lpp:
     	begin
        if not ASend then
	    		FEscape := FEscape + Esc8lpp
        else WriteStr(Esc8lpp);
      end;
    lsCustom:
      begin
        if not ASend then
          FEscape := FEscape + EscCustomLS + Chr(FLineSpacingCustomValue)
        else WriteStr(EscCustomLS + Chr(FLineSpacingCustomValue));
      end;
  end;
end;

procedure TAJDmPrinter.DefineFont;
begin
  case FFont.Pitch of
    fp10cpp: FEscape := FEscape + Esc10cpp;
    fp12cpp: FEscape := FEscape + Esc12cpp;
  end;
  { **** }
  if FFont.FontType = ftNLQ then
  begin
    case Font.NLQFont of
      ntRoman: FEscape := FEscape + EscNLQ + EscNLQRoman;
      ntSansSerif: FEscape := FEscape + EscNLQ + EscNLQSansSerif;
    end;
  end
  else FEscape := FEscape + EscDraft;
  { **** }
  case FFont.Size of
    fsCondensed: FEscape := FEscape + EscCondensedOn + EscLargeOff;
    fsDefault: FEscape := FEscape + EscCondensedOff + EscLargeOff;
    fsLarge: FEscape := FEscape + EscCondensedOff + EscLargeOn;
    fsLargeCondensed: FEscape := FEscape + EscCondensedOn  + EscLargeOn;
  end;
  { **** }
  if fsDmBold in FFont.Style then
    FEscape := FEscape + EscBoldOn
  else FEscape := FEscape + EscBoldOff;
  { **** }
  if fsDmItalic in FFont.Style then
    FEscape := FEscape + EscItalicOn
  else FEscape := FEscape + EscItalicOff;
  { **** }
  if fsDmDoubleStrike in FFont.Style then
    FEscape := FEscape + EscDblStrikeOn
  else FEscape := FEscape + EscDblStrikeOff;
  { **** }
  if fsDmSuperScript in FFont.Style then
    FEscape := FEscape + EscSupScriptOn
  else FEscape := FEscape + EscScriptOff;
  { **** }
  if fsDmSubScript in FFont.Style then
    FEscape := FEscape + EscSubScriptOn
  else FEscape := FEscape + EscScriptOff;
  { **** }
  if fsDmUnderline in FFont.Style then
    FEscape := FEscape + EscUnderlineOn
  else FEscape := FEscape + EscUnderlineOff;
end;

function TAJDmPrinter.CleanText(S: string): string;
const
  S1 = 'áéíóúÁÉÍÓÚàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛäëïöüÄËÏÖÜãõÃÕçÇ';
  S2 = 'aeiouAEIOUaeiouAEIOUaeiouAEIOUaeiouAEIOUaoAOcC';
var
  I, APos: Integer;
begin
  for I := 1 to Length(S) do
  begin
    APos := Pos(S[I], S1);
    if APos > 0 then
      S[I] := S2[APos];
  end;
  Result := S;
end;

procedure TAJDmPrinter.WriteOnPrinter(AText: string);
var
  Len, Count: DWord;
  {$ifdef linux}
  Written: Integer;
  {$endif}
begin
  if not FIsPrinting then
    RaiseError(Format(SNotPrinting, [PrinterName]));
  Len := Length(AText);
  {$ifdef linux}
  Printer.BeginDoc;
  Printer.Write(AText[1], Len, Written);
  Printer.EndDoc;
  {$endif}
  {$ifdef windows}
  WritePrinter(FPrinterHandle, Pointer(AText), Len, @Count);
  {$endif}
  if not Count = Len then
    RaiseError(Format(SWriteError, [PrinterName]));
end;

{ *********************************************************************** }
{               Procedures e functions internas p/ o Preview              }
{ *********************************************************************** }

//var
 // PreviewFont: TVDOPreviewFont;

procedure TAJDmPrinter.ParsePreviewFont;
//const
 // DefaultSize = 10;
begin
 { with PreviewFont do
  begin
    Name := 'Courier New';
    Size := DefaultSize;
    Style := [];
  end;
  with FFont do
  begin
    case Size of
      fsCondensed:
        PreviewFont.Size := Variant(DefaultSize * 0.80);
      fsDefault:
        PreviewFont.Size := DefaultSize;
      fsLargeCondensed:
        PreviewFont.Size := Variant(DefaultSize * 1.20);
      fsLarge:
        PreviewFont.Size := Variant(DefaultSize * 2);
    end;
    if fsDmBold in Style then
      PreviewFont.Style := PreviewFont.Style + [fsBold];
    if fsDmItalic in Style then
      PreviewFont.Style := PreviewFont.Style + [fsItalic];
    if fsDmUnderline in Style then
      PreviewFont.Style := PreviewFont.Style + [fsUnderline];
  end; }
end;

{ *********************************************************************** }
{                                 Métodos                                 }
{ *********************************************************************** }

procedure TAJDmPrinter.Abort;
begin
  if not FIsPrinting then Exit;
  {$ifdef windows}
  if ShowPreview then Exit;

  if Printer.Aborted then
  begin
       if not FromPreview then
          if Assigned(OnAbort) then OnAbort(Self);
  end else RaiseError(Format(SAbortPrinterError, [PrinterName]));
  if ShowProgress then ClosePrintProgress;
  if AbortPrinter(FPrinterHandle) then
  begin
    if ClosePrinter(FPrinterHandle) then
    begin
      if not FromPreview then
        if Assigned(OnAbort) then OnAbort(Self);
    end
    else RaiseError(Format(SClosePrinterError, [PrinterName]));
  end
  else RaiseError(Format(SAbortPrinterError, [PrinterName]));
  {$endif}
  if ShowProgress then ClosePrintProgress;
end;

procedure TAJDmPrinter.BeginDoc;
{$ifdef windows}
var
  DocInfo: TDocInfo1;
  PrnDefs: TPrinterDefaults;
  i: Integer;
{$endif}
begin
  FCurrentLine := 1;
  FCurrentPage := 1;
  if FIsPrinting then
    RaiseError(Format(SPrinting, [PrinterName]));
  if ShowPreview then
  begin
    BeginPreviewDoc;
//    frmVDOPreview.DmPrinter := Self;
    DefinePaper;
    DefineLineSpacing(True);
    Exit;
  end;
  { **** }
  if ShowDialog then
  begin
   DefinePrinter;
   if PrinterName = '' then
    RaiseError(SNoInstalledPrinter);
  end else begin
    if PrinterName <> '' then
    begin
      for i := 0 to Printer.Printers.Count -1 do
      begin
        if Printer.Printers[i] = PrinterName then
        begin
          FPrinterIndex := i;
          break;
        end;
      end;
    end;
    Printer.PrinterIndex := FPrinterIndex;
    PrinterName := Printer.PrinterName;
  end;

  Printer.Title:='AJPrint doc';
  {$ifndef windows}
  Printer.RawMode:=true; // Ativa o modo de impressão rápida
  {$endif}
  FIsPrinting:=true;
  if not FromPreview then
  begin
       if Assigned(OnStart) then OnStart(Self);
       if Assigned(OnNewPage) then  OnNewPage(Self);
  end;
  if ShowProgress then ShowPrintProgress;
  DefinePaper;
  DefineLineSpacing(true);

  {$ifdef windows}
  if PrinterName = '' then
    RaiseError(SNoInstalledPrinter);
  with PrnDefs do
  begin
    pDataType := 'RAW';
    pDevMode := FDevMode;
    DesiredAccess := PRINTER_ACCESS_USE;
  end;
  if OpenPrinter(PChar(PrinterName), @FPrinterHandle, @PrnDefs) then
  begin
    with DocInfo do
    begin
      pDocName := PChar(Title);
      pOutputFile := nil;
      pDatatype := 'RAW';
    end;
    if StartDocPrinter(FPrinterHandle, 1, @DocInfo) <> 0 then
    begin
      if StartPagePrinter(FPrinterHandle) then
      begin
        FIsPrinting := True;
        if not FromPreview then
        begin
          if Assigned(OnStart) then OnStart(Self);
          if Assigned(OnNewPage) then OnNewPage(Self);
        end;
        if ShowProgress then ShowPrintProgress;
        DefinePaper;
        DefineLineSpacing(True);
      end
      else RaiseError(Format(SStartPageError, [PrinterName]));
    end
    else RaiseError(Format(SStartDocError, [Title, PrinterName]));
  end
  else RaiseError(Format(SOpenPrinterError, [PrinterName]));
  {$endif}
end;

procedure TAJDmPrinter.EndDoc(EjectPage: Boolean = True);
begin
  if not FIsPrinting then Exit;
  if EjectPage then
    WriteStr(EscFF);
  WriteStr(EscReset);
  InitInternalVariables;
  if ShowPreview then
  begin
    EndPreviewDoc;
    Exit;
  end;
  FIsPrinting:=False;
  if not FromPreview then
     if Assigned(OnTerminate) then OnTerminate(Self);
  {$ifdef windows}
  if EndPagePrinter(FPrinterHandle) then
  begin
    if EndDocPrinter(FPrinterHandle) then
    begin
      if ClosePrinter(FPrinterHandle) then
      begin
        FIsPrinting := False;
        if not FromPreview then
          if Assigned(OnTerminate) then OnTerminate(Self);
      end
      else RaiseError(Format(SClosePrinterError, [PrinterName]));
    end
    else RaiseError(Format(SEndDocError, [Title, PrinterName]));
  end
  else RaiseError(Format(SEndPageError, [PrinterName]));
  {$endif}
  if ShowProgress then ClosePrintProgress;
end;

procedure TAJDmPrinter.NewLine(ALines: Integer = 1);
var
  I: Integer;
begin
  if not FIsPrinting then Exit;
  for I := 1 to ALines do
  begin
    WriteStr(EscCR + EscLF);
//    if ShowPreview then
  //    frmVDOPreview.NewLine;
    Inc(FCurrentLine);
    if not FromPreview then
      if Assigned(OnNewLine) then OnNewLine(Self);
    if FPaper.AutoNewPage and (FPaper.Lines > 0) then
      if FCurrentLine > (FPaper.Lines - FPaper.AutoNewPageLines) then
         NewPage;
  end;
  FLastCol := 0;
end;

procedure TAJDmPrinter.NewPage(APages: Integer = 1);
var
  I: Integer;
begin
  if not FIsPrinting then Exit;
  for I := 1 to APages do
  begin
    WriteStr(EscFF);
   // if ShowPreview then
    //  frmVDOPreview.NewPage;
    Inc(FCurrentPage);
    FCurrentLine := 1;
    FLastCol := 0;
    if not FromPreview then
      if Assigned(OnNewPage) then OnNewPage(Self);
  end;
end;

procedure TAJDmPrinter.Print(AColumn: Integer; AText: string;
  ALineFeed: Boolean = False);
var
  Pad: string;
  Cols: Integer;
begin
  if not FIsPrinting then Exit;
  Pad := StringOfChar(#32, AColumn - FLastCol);
  if FLastCol = 0 then
  begin
    DefineLineSpacing;
  	DefineFont;
	  if FEscape = FLastEsc then
    	FEscape := '';
  end;
  WriteStr(FEscape + Pad, Pad);
  FEscape := '';
  DefineLineSpacing;
  DefineFont;
  FTextBuf := AText;
  if FPaper.Columns > 0 then
  begin
    Cols := FPaper.Columns;
    if FFont.Pitch = fp12cpp then
      Cols := Variant(Cols * 1.20);
    Cols := Cols - FPaper.MarginLeft - FPaper.MarginRight;
    with FFont do
    begin
      case Size of
        fsCondensed:
        case Pitch of
          fp10cpp: Cols := Variant(Cols * 1.70);
          fp12cpp: Cols := Variant(Cols * 1.66);
        end;
        fsLarge:   Cols := Variant(Cols * 0.50);
        fsLargeCondensed:
        case Pitch of
          fp10cpp: Cols := Variant(Cols * 0.85);
          fp12cpp: Cols := Variant(Cols * 0.83);
        end;
      end;
      case Align of
        faCenter: FTextBuf := StringOfChar(#32, (Cols - Length(FTextBuf)) div 2) + FTextBuf;
        faRight:  FTextBuf := StringOfChar(#32, (Cols - Length(FTextBuf))) + FTextBuf;
      end;
    end;
  end;
  if FCharMode = cmClear then
    FTextBuf := CleanText(FTextBuf);
  if not (FEscape = FLastEsc) then
    WriteStr(FEscape + FTextBuf, FTextBuf)
  else WriteStr(FTextBuf, FTextBuf);
  FLastCol := AColumn + Length(FTextBuf);
  FLastEsc := FEscape;
  FEscape := '';
  FTextBuf := '';
  if not FromPreview then
    if Assigned(OnPrint) then OnPrint(Self);
  if ALineFeed then
    NewLine;
end;

procedure TAJDmPrinter.Print(ALine, AColumn: Integer; AText: string;
  ALineFeed: Boolean = False);
begin
  NewLine(ALine - FCurrentLine);
  Print(AColumn, AText, ALineFeed);
end;

procedure TAJDmPrinter.Replicate(AColumn, ACount: Integer;
  AChar: Char = '-'; ALineFeed: Boolean = True);
begin
  Print(AColumn, StringOfChar(AChar, ACount), ALineFeed);
end;

procedure TAJDmPrinter.Replicate(ALine, AColumn, ACount: Integer;
  AChar: Char = '-'; ALineFeed: Boolean = True);
begin
  Print(ALine, AColumn, StringOfChar(AChar, ACount), ALineFeed);
end;

procedure TAJDmPrinter.WriteStr(AText: string; APreviewText: string = '');
begin
 // if ShowPreview then
 // begin
   // ParsePreviewFont;
  //  frmVDOPreview.Write(APreviewText, AText, PreviewFont);
  //end
 // else
   WriteOnPrinter(AText);
end;

initialization
  {$i ajlaz.lrs}

end.
