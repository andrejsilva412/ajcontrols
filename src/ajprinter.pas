{ *********************************************************************** }
{                                                                         }
{                          VDO Print Components                           }
{                              TVDOPrinter                                }
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
{  The Original Code is VDOPrinter.pas                                  }
{                                                                         }
{  The Initial Developer of the Original Code is Vinicius de Oliveira.    }
{  All Rights Reserved.                                                   }
{                                                                         }
{  Contact :                                                              }
{     vncsoliveira@yahoo.com.br                                           }
{     http://vdo.sourceforge.net                                          }
{ *********************************************************************** }

//{$D-}

unit AJPrinter;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, SysUtils, Classes, Printers, AJBasePrinter, AJCaPrinter,
  AJDmPrinter, LResources;

type
  TAJPrinterType = (ptCanvas, ptDotMatrix);
  { **** }
  TAJPrinter = class(TAJBasePrinter)
  private
    { Vari�veis internas }
    FCaPrinter: TAJCaPrinter;
    FDmPrinter: TAJDmPrinter;
    { Vari�veis p/ propriedades Read / Write publicadas }
    FCaSets: TAJCaSets;
    FDmSets: TAJDmSets;
    FPrinterType: TAJPrinterType;
    FShowTypeDialog: Boolean;
    { Procedures e Functions internas }
    function ShowTypeDlg: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { M�todos }
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
  published
    { Propriedades Read / Write }
    property CanvasSets: TAJCaSets read FCaSets write FCaSets;
    property DotMatrixSets: TAJDmSets read FDmSets write FDmSets;
    property PrinterType: TAJPrinterType read FPrinterType write FPrinterType;
    property ShowTypeDialog: Boolean read FShowTypeDialog write FShowTypeDialog;
  end;

procedure Register;

implementation

uses
  Controls, ajprintertypedlg, AJPrintConsts;

procedure Register;
begin
  RegisterComponents('AJCtrls Print', [TAJPrinter]);
end;

{ *********************************************************************** }
{                                                                         }
{                             TAJPrinter                                 }
{                                                                         }
{ *********************************************************************** }

constructor TAJPrinter.Create(AOwner: TComponent);
begin
  inherited;
  FCaPrinter := TAJCaPrinter.Create(nil);
  FCaSets := TAJCaSets.Create;
  FDmPrinter := TAJDmPrinter.Create(nil);
  FDmSets := TAJDmSets.Create;
  FPrinterType := ptCanvas;
  FShowTypeDialog := True;
end;

destructor TAJPrinter.Destroy;
begin
  FCaPrinter.Free;
  FDmPrinter.Free;
  inherited;
end;

{ *********************************************************************** }
{                     Procedures e functions internas                     }
{ *********************************************************************** }

function TAJPrinter.ShowTypeDlg: Boolean;
begin
  frmAJPrinterTypeDlg := TfrmAJPrinterTypeDlg.Create(nil);
  with frmAJPrinterTypeDlg do
  begin
    case FPrinterType of
      ptCanvas: SelectedType := 0;
      ptDotMatrix: SelectedType := 1;
    end;
    ShowModal;
    case SelectedType of
      0: FPrinterType := ptCanvas;
      1: FPrinterType := ptDotMatrix;
    end;
    frmAJPrinterTypeDlg.Free;
    frmAJPrinterTypeDlg := nil;
  end;
  Result := True;
end;

{ *********************************************************************** }
{                                 M�todos                                 }
{ *********************************************************************** }

procedure TAJPrinter.Abort;
begin
  case FPrinterType of
    ptCanvas:     FCaPrinter.Abort;
    ptDotMatrix:  FDmPrinter.Abort;
  end;
  if not FromPreview then
    if Assigned(OnAbort) then OnAbort(Self);
end;

procedure TAJPrinter.BeginDoc;
begin
  FCurrentLine := 1;
  FCurrentPage := 1;
  if FShowTypeDialog then ShowTypeDlg;
  case FPrinterType of
    ptCanvas:
      begin
        with FCaPrinter do
        begin
          LineSpacing := FCaSets.LineSpacing;
          Orientation := FCaSets.Orientation;
          with Paper do
          begin
            Columns := FCaSets.Paper.Columns;
            Lines := FCaSets.Paper.Lines;
            MarginLeft := FCaSets.Paper.MarginLeft;
            MarginRight := FCaSets.Paper.MarginRight;
          end;
          PrinterName := Self.PrinterName;
          ShowDialog := Self.ShowDialog;
          ShowPreview := Self.ShowPreview;
          ShowProgress := Self.ShowProgress;
          Title := Self.Title;
          BeginDoc;
        end;
        FIsPrinting := FCaPrinter.IsPrinting;
      end;
    ptDotMatrix:
      begin
        with FDmPrinter do
        begin
          CharMode := FDmSets.CharMode;
          LineSpacing := FDmSets.LineSpacing;
          with Paper do
          begin
            Columns := FDmSets.Paper.Columns;
            Lines := FDmSets.Paper.Lines;
            MarginLeft := FDmSets.Paper.MarginLeft;
            MarginRight := FDmSets.Paper.MarginRight;
          end;
          PrinterName := Self.PrinterName;
          ShowDialog := Self.ShowDialog;
          ShowPreview := Self.ShowPreview;
          ShowProgress := Self.ShowProgress;
          Title := Self.Title;
          BeginDoc;
        end;
        FIsPrinting := FDmPrinter.IsPrinting;
      end;
  end;
  if not FromPreview then
  begin
    if Assigned(OnStart) then OnStart(Self);
    if Assigned(OnNewPage) then OnNewPage(Self);
  end;
end;

procedure TAJPrinter.EndDoc(EjectPage: Boolean = True);
begin
  case FPrinterType of
    ptCanvas:
      begin
        FCaPrinter.EndDoc;
        FIsPrinting := FCaPrinter.IsPrinting;
      end;
    ptDotMatrix:
      begin
        FDmPrinter.EndDoc(EjectPage);
        FIsPrinting := FDmPrinter.IsPrinting;
      end;
  end;
  if not FromPreview then
    if Assigned(OnTerminate) then OnTerminate(Self);
end;

procedure TAJPrinter.NewLine(ALines: Integer = 1);
var
  I: Integer;
begin
  for I := 1 to ALines do
  begin
    case FPrinterType of
      ptCanvas:
        begin
          FCaPrinter.NewLine;
          Inc(FCurrentLine);
          if not FromPreview then
            if Assigned(OnNewLine) then OnNewLine(Self);
          if (FCaSets.Paper.AutoNewPage) and (FCaSets.Paper.Lines > 0) then
            if FCurrentLine > FCaSets.Paper.Lines then
              NewPage;
        end;
      ptDotMatrix:
        begin
          FDmPrinter.NewLine;
          Inc(FCurrentLine);
          if not FromPreview then
            if Assigned(OnNewLine) then OnNewLine(Self);
          if FDmSets.Paper.AutoNewPage and (FDmSets.Paper.Lines > 0) then
            if FCurrentLine > (FDmSets.Paper.Lines - FDmSets.Paper.AutoNewPageLines) then
             NewPage;
        end;
    end;
  end;
end;

procedure TAJPrinter.NewPage(APages: Integer = 1);
var
  I: Integer;
begin
  for I := 1 to APages do
  begin
    case FPrinterType of
      ptCanvas: FCaPrinter.NewPage;
      ptDotMatrix: FDmPrinter.NewPage;
    end;
    Inc(FCurrentPage);
    FCurrentLine := 1;
    if not FromPreview then
      if Assigned(OnNewPage) then OnNewPage(Self);
  end;
end;

procedure TAJPrinter.Print(AColumn: Integer; AText: string;
  ALineFeed: Boolean = False);
begin
  case FPrinterType of
    ptCanvas:
      begin
        FCaPrinter.Font := FCaSets.Font;
        FCaPrinter.Print(AColumn, AText);
      end;
    ptDotMatrix:
      begin
        FDmPrinter.Font := FDmSets.Font;
        FDmPrinter.Print(AColumn, AText);
      end;
  end;
  if not FromPreview then
    if Assigned(OnPrint) then OnPrint(Self);
  if ALineFeed then
    NewLine;
end;

procedure TAJPrinter.Print(ALine, AColumn: Integer; AText: string;
  ALineFeed: Boolean = False);
begin
  NewLine(ALine - FCurrentLine);
  Print(AColumn, AText, ALineFeed);
end;

procedure TAJPrinter.Replicate(AColumn, ACount: Integer;
  AChar: Char = '-'; ALineFeed: Boolean = True);
begin
  Print(AColumn, StringOfChar(AChar, ACount), ALineFeed);
end;

procedure TAJPrinter.Replicate(ALine, AColumn, ACount: Integer;
  AChar: Char = '-'; ALineFeed: Boolean = True);
begin
  Print(ALine, AColumn, StringOfChar(AChar, ACount), ALineFeed);
end;

initialization
  {$i ajlaz.lrs}

end.
