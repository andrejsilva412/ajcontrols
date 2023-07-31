{ *********************************************************************** }
{                                                                         }
{                          VDO Print Components                           }
{                            TVDOBasePrinter                              }
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
{  The Original Code is VDOBasePrinter.pas                                }
{                                                                         }
{  The Initial Developer of the Original Code is Vinicius de Oliveira.    }
{  All Rights Reserved.                                                   }
{                                                                         }
{  Contact :                                                              }
{     vncsoliveira@yahoo.com.br                                           }
{     http://vdo.sourceforge.net                                          }
{ *********************************************************************** }

//{$D-}

unit AJBasePrinter;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Dialogs, Printers;

type
  EAJPrinter = class(Exception);
  { **** }

  { TAJBasePrinter }

  TAJBasePrinter = class(TComponent)
  private
    { Vari�veis p/ propriedades Read Only }
    FPrintersList: TStrings;
    { Vari�veis p/ propriedades Read / Write }
    FFromPreview: Boolean;
    { Vari�veis p/ propriedades Read / Write publicadas }
    FPrinterName: string;
    FShowDialog: Boolean;
    FShowPreview: Boolean;
    FShowProgress: Boolean;
    FTitle: string;
    { Vari�veis para eventos publicados }
    FOnAbort: TNotifyEvent;
    FOnError: TNotifyEvent;
    FOnNewLine: TNotifyEvent;
    FOnNewPage: TNotifyEvent;
    FOnPrint: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    { Procedures e Functions Get/Set de propriedades  }
    function GetPrintersList: TStrings;
    function GetAJPrintVersion: string;
    procedure SetAJPrintVersion(const Value: string);
  protected
    { Vari�veis internas acess�veis aos descendentes }
    FLastCol: Integer;
    FTextBuf: string;
    { Vari�veis p/ propriedades Read Only acess�veis aos descendentes }
    FCurrentLine: Integer;
    FCurrentPage: Integer;
    FIsPrinting: Boolean;
    { Procedures e Functions internas acess�veis aos descendentes }
    procedure RaiseError(AMsg: string);
    function IsValidPrinter(APrinterName: string): Boolean;
    function ShowPrintDialog: Boolean;
    procedure ShowPrintProgress;
    procedure ClosePrintProgress;
    { Procedures e functions internas p/ o Preview acess�veis aos descendentes }
    procedure BeginPreviewDoc;
    procedure EndPreviewDoc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Propriedades Read Only }
    property CurrentLine: Integer read FCurrentLine;
    property CurrentPage: Integer read FCurrentPage;
    property IsPrinting: Boolean read FIsPrinting;
    property PrintersList: TStrings read GetPrintersList;
    { Propriedades Read / Write }
    property FromPreview: Boolean read FFromPreview write FFromPreview;
  published
    { Propriedades Read / Write }
    property PrinterName: string read FPrinterName write FPrinterName;
    property ShowDialog: Boolean read FShowDialog write FShowDialog;
    property ShowPreview: Boolean read FShowPreview write FShowPreview;
    property ShowProgress: Boolean read FShowProgress write FShowProgress;
    property Title: string read FTitle write FTitle;
    property Version: string read GetAJPrintVersion write SetAJPrintVersion stored False;
    { Eventos }
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnNewLine: TNotifyEvent read FOnNewLine write FOnNewLine;
    property OnNewPage: TNotifyEvent read FOnNewPage write FOnNewPage;
    property OnPrint: TNotifyEvent read FOnPrint write FOnPrint;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

implementation

uses
  AJPrintConsts, {VDOPreview,} AJPrintProgress;

{ *********************************************************************** }
{                                                                         }
{                            TAJBasePrinter                                }
{                                                                         }
{ *********************************************************************** }

constructor TAJBasePrinter.Create(AOwner: TComponent);
begin
  inherited;
  FIsPrinting := False;
  FFromPreview := False;
  { **** }
  FPrinterName := '';
 // FShowDialog := True;
  FShowPreview := False;
  FShowProgress := True;
  FTitle := 'AJPrint Document';
end;

destructor TAJBasePrinter.Destroy;
begin
  FPrintersList.Free;
  inherited;
end;

{ *********************************************************************** }
{                      Get/Set de propriedades                            }
{ *********************************************************************** }

function TAJBasePrinter.GetPrintersList: TStrings;
var
  DefaultIndex, I: Integer;
begin
  if FPrintersList = nil then
    FPrintersList := TStringList.Create;
  FPrintersList.Clear;
  if not (Printer.Printers.Count = 0) then
  begin
    FPrintersList := Printer.Printers;
    DefaultIndex := Printer.PrinterIndex;
  end;
  Result := FPrintersList;
end;

function TAJBasePrinter.GetAJPrintVersion: string;
begin
  Result := SAJPrintVersion;
end;

procedure TAJBasePrinter.SetAJPrintVersion(const Value: string);
begin
 { Only for show in the Object inspector }
end;

{ *********************************************************************** }
{        Procedures e functions internas acess�veis aos descendentes      }
{ *********************************************************************** }

procedure TAJBasePrinter.RaiseError(AMsg: string);
begin
  if Assigned(FOnError) then FOnError(Self);
  raise EAJPrinter.Create(AMsg);
end;

function TAJBasePrinter.IsValidPrinter(APrinterName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to (PrintersList.Count - 1) do
  begin
    Result := (PrintersList.Strings[I] = APrinterName);
    if Result then Break;
  end;
end;

function TAJBasePrinter.ShowPrintDialog: Boolean;
//var
 // PrintDlg: TPrintDialog;
begin
 // FPrinterName := '';
 // PrintDlg := TPrintDialog.Create(nil);
 // Result := PrintDlg.Execute;
 // PrintDlg.Free;
end;

procedure TAJBasePrinter.ShowPrintProgress;
begin
  if frmAJPrintProgress = nil then
    frmAJPrintProgress := TfrmAJPrintProgress.Create(nil);
  frmAJPrintProgress.Show;
  frmAJPrintProgress.Update;
end;

procedure TAJBasePrinter.ClosePrintProgress;
begin
  if not (frmAJPrintProgress = nil) then
  begin
    frmAJPrintProgress.CanClose := True;
    frmAJPrintProgress.Close;
    frmAJPrintProgress.Free;
    frmAJPrintProgress := nil;
  end;
end;


{ *********************************************************************** }
{               Procedures e functions internas p/ o Preview              }
{ *********************************************************************** }

procedure TAJBasePrinter.BeginPreviewDoc;
begin
//  frmVDOPreview := TfrmVDOPreview.Create(nil);
 // frmVDOPreview.BeginDoc;
  FIsPrinting := True;
  if Assigned(FOnStart) then FOnStart(Self);
  if Assigned(FOnNewPage) then FOnNewPage(Self);
  if FShowProgress then ShowPrintProgress;
end;

procedure TAJBasePrinter.EndPreviewDoc;
begin
  FIsPrinting := False;
  if Assigned(FOnTerminate) then FOnTerminate(Self);
  if FShowProgress then ClosePrintProgress;
end;

end.
