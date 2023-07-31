{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ajctrls;

{$warn 5023 off : no warning about unused units}
interface

uses
  AJBasePrinter, AJButton, AJCaPrinter, AJCConst, ajdateutil, ajdbutils, 
  AJDmPrinter, ajedit, AJPanel, AJPrinter, ajaeroglass, ajprogressbar, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AJButton', @AJButton.Register);
  RegisterUnit('AJCaPrinter', @AJCaPrinter.Register);
  RegisterUnit('AJDmPrinter', @AJDmPrinter.Register);
  RegisterUnit('ajedit', @ajedit.Register);
  RegisterUnit('AJPanel', @AJPanel.Register);
  RegisterUnit('AJPrinter', @AJPrinter.Register);
  RegisterUnit('ajaeroglass', @ajaeroglass.Register);
  RegisterUnit('ajprogressbar', @ajprogressbar.Register);
end;

initialization
  RegisterPackage('ajctrls', @Register);
end.
