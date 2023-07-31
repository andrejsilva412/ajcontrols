unit AJdvcpfcnpj;

{$mode objfpc}{$H+}

interface

uses
  LResources, SysUtils, Classes, Controls, Graphics,
  ajstrutils, Dialogs;

type
    TDV = (DvCPF, DvCNPJ);

type

  { TAJDVCPFCNPJ }

  TAJDVCPFCNPJ = class(TComponent)
  private
    FDigito1: Integer;
    FDigito2: Integer;
    FTipoDv: TDV;
    FValido: Boolean;
    FValue: String;
    function chInt ( ch: Char ): ShortInt;
    function intCh ( int: ShortInt ): Char;
    function dvModulo11ParaCPF ( str: String ): Char;
    function DvModulo11 ( str: String ): Char;
    function DvModulo10 ( str: String ): Char;
    function FDvCNPJ ( str: String ): String;
    function FDvCPF ( str: String ): String;
    function ValidaCNPJ ( str: String ): Boolean;
    function ValidaCPF ( str: String ): Boolean;
    procedure SetDigito1(const AValue: Integer);
    procedure SetDigito2(const AValue: Integer);
    procedure SetTipoDv(const AValue: TDV);
    procedure SetValido(const AValue: Boolean);
    procedure SetValue(const AValue: String);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Aowner : TComponent); override;
  published
    { Published declarations }
    property Digito1: Integer read FDigito1 write SetDigito1;
    property Digito2: Integer read FDigito2 write SetDigito2;
    property Valido: Boolean read FValido write SetValido;
    property Value: String read FValue write SetValue;
    property TipoDv: TDV read FTipoDv write SetTipoDv;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('AJCtrls Utils', [TAJDVCPFCNPJ]);
end;

procedure TAJDVCPFCNPJ.SetDigito2(const AValue: Integer);
begin
  if FDigito2=AValue then exit;
  FDigito2:=AValue;
end;

procedure TAJDVCPFCNPJ.SetTipoDv(const AValue: TDV);
begin
  if FTipoDv=AValue then exit;
  FTipoDv:=AValue;
end;

procedure TAJDVCPFCNPJ.SetValido(const AValue: Boolean);
begin
  if FValido=AValue then exit;
  FValido:=AValue;
end;

procedure TAJDVCPFCNPJ.SetValue(const AValue: String);
var
   dv1, dv2, tDV: String;
begin
  if FValue=AValue then exit;
  FValue:=AValue;
  FValue := DelChars(FValue, '.');
  FValue := DelChars(FValue, '-');
  FValue := DelChars(FValue, '/');
  if FTipoDv = DvCNPJ then
  begin
      FValido := ValidaCNPJ(FValue);
      tdv := FDvCNPJ(Copy(FValue, 1, 12));
      dv1 := Copy(tDV, 0, 1);
      dv2 := Copy(tDV, 2, 1);
      FDigito1:=StrToInt(dv1);
      FDigito2:=StrToInt(dv2);
  end;
  if FTipoDv = DvCPF then
  begin
      FValido := ValidaCPF(FValue);
      tdv := FDvCPF(Copy(FValue, 1, 9));
      dv1 := Copy(tDV, 0, 1);
      dv2 := Copy(tDV, 2, 1);
      FDigito1:=StrToInt(dv1);
      FDigito2:=StrToInt(dv2);
  end;
end;

{ DvModulo11 - Retorna 1 dv calculado pelo método do modulo 11 padrão. }
function TAJDVCPFCNPJ.DvModulo11(str: String): Char;
var
 soma, fator, i: Integer;
begin
  soma := 0;
  fator := 2;
  for i := Length ( str ) downto 1 do
  begin
   soma := soma + chInt ( str[i] ) * fator;
   Inc ( fator );
   if fator = 10 then
      fator := 2;
  end;
  soma := 11 - ( soma mod 11 );
  if soma >= 10 then
     Result := '0'
  else
     Result := intCh ( soma );
end;
{ DvModulo10 - Retorna 1 dv calculado pelo método do modulo 10 padrão. }
function TAJDVCPFCNPJ.DvModulo10(str: String): Char;
var
 soma, fator, i: Integer;
begin
    soma := 0;
    fator := 2;
    for i := Length ( str ) downto 1 do
      begin
        soma := soma + chInt ( str[i] ) * fator;
        Dec ( fator );
        if fator = 0 then
            fator := 2;
      end;
    soma := 10 - ( soma div 11 );
    if soma >= 10 then
        Result := '0'
    else
        Result := intCh ( soma );
end;
{ DvCNPJ - Retorna os dois dvs de um CNPJ, dado o CNPJ sem os dvs em forma de
  string (12 caracteres numéricos). }
function TAJDVCPFCNPJ.FDvCNPJ(str: String): String;
var
dv1: Char;
begin
  dv1 := DvModulo11 ( str );
  Result := dv1 + DvModulo11 ( str + dv1 );
end;

function TAJDVCPFCNPJ.FDvCPF(str: String): String;
var dv1: Char;
begin
   dv1 := dvModulo11ParaCPF ( str );
   Result := dv1 + dvModulo11ParaCPF ( str + dv1 );
end;

function TAJDVCPFCNPJ.ValidaCNPJ(str: String): Boolean;
begin
    Result := Copy ( str, 13, 2 ) = FDvCNPJ ( Copy ( str, 1, 12 ) );
end;

function TAJDVCPFCNPJ.ValidaCPF(str: String): Boolean;
begin
    Result := Copy ( str, 10, 2 ) = FDvCPF ( Copy ( str, 1, 9 ) );
end;

procedure TAJDVCPFCNPJ.SetDigito1(const AValue: Integer);
begin
  if FDigito1=AValue then exit;
  FDigito1:=AValue;
end;

constructor TAJDVCPFCNPJ.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
end;

{ chInt - Converte um caracter numérico para o valor inteiro correspondente. }

function TAJDVCPFCNPJ.chInt ( ch: Char ): ShortInt;
  begin
    Result := Ord ( ch ) - Ord ( '0' );
  end;

{ intCh = Converte um valor inteiro (de 0 a 9) para o caracter numérico
  correspondente. }

function TAJDVCPFCNPJ.intCh ( int: ShortInt ): Char;
  begin
    Result := Chr ( int + Ord ( '0' ) );
  end;

{ dvModulo11ParaCPF - Retorna 1 dv calculado pelo método do modulo 11
  ligeiramente alterado para o CPF. }

function TAJDVCPFCNPJ.dvModulo11ParaCPF ( str: String ): Char;
  var soma, fator, i: Integer;
  begin
    soma := 0;
    fator := 2;
    for i := Length ( str ) downto 1 do
      begin
        soma := soma + chInt ( str[i] ) * fator;
        Inc ( fator );
      end;
    soma := 11 - ( soma mod 11 );
    if soma >= 10 then
        Result := '0'
    else
        Result := intCh ( soma );
  end;

initialization
  {$i ajlaz.lrs}

end.
