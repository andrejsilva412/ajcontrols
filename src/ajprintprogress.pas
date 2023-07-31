{ *********************************************************************** }
{                                                                         }
{                          VDO Print Components                           }
{                          TfrmVDOPrintProgress                           }
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
{  The Original Code is VDOPrintProgress.pas                              }
{                                                                         }
{  The Initial Developer of the Original Code is Vinicius de Oliveira.    }
{  All Rights Reserved.                                                   }
{                                                                         }
{  Contact :                                                              }
{     vncsoliveira@yahoo.com.br                                           }
{     http://vdo.sourceforge.net                                          }
{ *********************************************************************** }


unit AJPrintProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TfrmAJPrintProgress }

  TfrmAJPrintProgress = class(TForm)
    lblPrinting: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    CanClose: Boolean;
  end;


var
  frmAJPrintProgress: TfrmAJPrintProgress;

implementation

uses AJPrintConsts, AJLCLAppUtils;

{ TfrmAJPrintProgress }

procedure TfrmAJPrintProgress.FormCreate(Sender: TObject);
begin
  lblPrinting.Caption := SPrintProgress;
end;

procedure TfrmAJPrintProgress.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if not CanClose then
    CloseAction := caNone;
end;

procedure TfrmAJPrintProgress.FormActivate(Sender: TObject);
{$ifdef windows}
var
  tmpMargins: TMargins;
{$endif}
begin
  {$ifdef windows}
  if IsWindowsVista then
  begin
    tmpMargins.cxLeftWidth    := -1;
    tmpMargins.cxRightWidth   := -1;
    tmpMargins.cyBottomHeight := -1;
    tmpMargins.cyTopHeight    := -1;
    GlassForm(Self, tmpMargins, $00DCDBDA);
  end;
  {$endif}
end;

procedure TfrmAJPrintProgress.FormShow(Sender: TObject);
begin
  Update;
end;

initialization
  {$I ajprintprogress.lrs}

end.

