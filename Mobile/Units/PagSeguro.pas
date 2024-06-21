unit PagSeguro;

interface

uses System.Classes, System.Net.HttpClient, System.Net.URLClient, REST.Json,
     System.JSON, System.StrUtils;

type
  TAmbiente = (PRODUCAO, HOMOLOGACAO);
  TMetodoPagamento = (CARTAOCREDITO, BOLETO);
  TCobrancaStatus = (AUTHORIZED, PAID, DECLINED, CANCELED, WAITING);

  TPagSeguro = class
  private
    FCodigoVerificacao: string;
    FAnoExpiracao: integer;
    FNumeroCartao: string;
    FNomeTitular: string;
    FMesExpiracao: integer;
    FToken: string;
    FAmbiente: TAmbiente;
    FIdentificador: string;
    FDescricao: string;
    FValorTotal: double;
    FMetodoPagamento: TMetodoPagamento;
    FNumParcelas: integer;
    FCapture: boolean;
    FNomeNaFatura: string;
    FURLNotificacao: string;
    FMoeda: string;
    FCobrancaId: string;
    FCobrancaStatus: TCobrancaStatus;
    FResponseMessage: string;
    FCobrancaStatusString: string;
    FStatusCode: integer;
    FDataCriacao: string;
    procedure SetAmbiente(const Value: TAmbiente);
    procedure ProcessarResponseCobranca(const AJSON: string);
    procedure ProcessarResponseConsulta(const AJSON: string);

  public
    constructor Create;
    destructor Destroy; override;

    property Ambiente: TAmbiente read FAmbiente write SetAmbiente;
    property Token: string read FToken write FToken;

    property Identificador: string read FIdentificador write FIdentificador;
    property Descricao: string read FDescricao write FDescricao;
    property ValorTotal: double read FValorTotal write FValorTotal;
    property Moeda: string read FMoeda write FMoeda;
    property MetodoPagamento: TMetodoPagamento read FMetodoPagamento write FMetodoPagamento;
    property NumParcelas: integer read FNumParcelas write FNumParcelas;
    property Capture: boolean read FCapture write FCapture;
    property NomeNaFatura: string read FNomeNaFatura write FNomeNaFatura;

    property NumeroCartao: string read FNumeroCartao write FNumeroCartao;
    property MesExpiracao: integer read FMesExpiracao write FMesExpiracao;
    property AnoExpiracao: integer read FAnoExpiracao write FAnoExpiracao;
    property CodigoVerificacao: string read FCodigoVerificacao write FCodigoVerificacao;
    property NomeTitular: string read FNomeTitular write FNomeTitular;

    property URLNotificacao: string read FURLNotificacao write FURLNotificacao;

    property CobrancaId: string read FCobrancaId write FCobrancaId;
    property CobrancaStatus: TCobrancaStatus read FCobrancaStatus write FCobrancaStatus;
    property CobrancaStatusString: string read FCobrancaStatusString write FCobrancaStatusString;
    property DataCriacao: string read FDataCriacao write FDataCriacao;

    property ResponseMessage: string read FResponseMessage write FResponseMessage;
    property StatusCode: integer read FStatusCode write FStatusCode;

    function ToJSON: string;
    function CobrarCartao: boolean;
    function ConsultarCobranca(ACobrancaId: string): boolean;
  end;

  TAmount = class
  private
    FValue: Extended;
    FCurrency: string;
  public
    property value : Extended read FValue write FValue;
    property currency: string  read FCurrency write FCurrency;
  end;

  TCardHolder = class
  private
    FName: string;
  public
    property name: string  read FName write FName;
  end;

  TCard = class
  private
    FExp_year: integer;
    FSecurity_code: string;
    FNumber: string;
    FExp_month: integer;
    FHolder: TCardHolder;
  public
    constructor Create;
    destructor Destroy; override;
    property number: string  read FNumber write FNumber;
    property exp_month: integer  read FExp_month write FExp_month;
    property exp_year: integer  read FExp_year write FExp_year;
    property security_code: string  read FSecurity_code write FSecurity_code;
    property holder: TCardHolder read FHolder write FHolder;
  end;

  TPaymentMethod = class
  private
    FType: string;
    FInstallments: integer;
    FCapture: boolean;
    FSoft_descriptor: string;
    FCard: TCard;

  public
    constructor Create;
    destructor Destroy; override;
    property &type: string read FType write FType;
    property installments: integer read FInstallments write FInstallments;
    property capture: boolean read FCapture write FCapture;
    property soft_descriptor: string read FSoft_descriptor write FSoft_descriptor;
    property card: TCard read FCard write FCard;
  end;

  TCobranca = class
  private
    FAmount: TAmount;
    FDescription: string;
    FReference_id: string;
    FPayment_method: TPaymentMethod;
    FNotification_urls: Array[0..0] of string;

    constructor Create;
    destructor Destroy; override;
    function GetNotification_urls(Index: Integer): String;
    procedure SetNotification_urls(Index: Integer; const Value: String);

  public
    property reference_id : string read FReference_id write FReference_id;
    property description : string read FDescription write FDescription;
    property amount: TAmount read FAmount write FAmount;
    property payment_method: TPaymentMethod read FPayment_method write FPayment_method;
    property notification_urls[Index: Integer]: String read GetNotification_urls write SetNotification_urls;
  end;

implementation

var
  URL_BASE: string;

Const
    URL_PRODUCAO = 'https://api.pagseguro.com';
    URL_HOMOLOGACAO = 'https://sandbox.api.pagseguro.com';
    STATUS_COBRANCA: array[0..4] of string = ('AUTHORIZED', 'PAID', 'DECLINED', 'CANCELED', 'WAITING');

{
URLs:

Painel de testes: https://sandbox.pagseguro.uol.com.br
Doc da API: https://dev.pagseguro.uol.com.br/reference/pagseguro-reference-intro
Cartoes de Teste: https://dev.pagseguro.uol.com.br/reference/testing-cards
Solicitar Homologacao: https://dev.pagseguro.uol.com.br/reference/request-approval
}

{ TPagSeguro }

constructor TPagSeguro.Create;
begin
    Ambiente := Homologacao;
    FCapture := True;
    FMoeda := 'BRL';
    FNumParcelas := 1;
end;

destructor TPagSeguro.Destroy;
begin

  inherited;
end;

procedure TPagSeguro.ProcessarResponseCobranca(const AJSON: string);
var
    jsonObj: TJSONObject;
    indexStatus: integer;
begin
    try
        jsonObj := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;

        Self.CobrancaId := jsonObj.GetValue<string>('id', '');

        indexStatus := AnsiIndexStr(jsonObj.GetValue<string>('status', ''), STATUS_COBRANCA);

        Self.CobrancaStatusString := STATUS_COBRANCA[indexStatus];
        Self.CobrancaStatus := TCobrancaStatus(Integer(indexStatus));
    finally
        jsonObj.DisposeOf;
    end;
end;

procedure TPagSeguro.ProcessarResponseConsulta(const AJSON: string);
var
    jsonObj: TJSONObject;
    indexStatus: integer;
begin
    try
        jsonObj := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;

        Self.CobrancaId := jsonObj.GetValue<string>('id', '');

        indexStatus := AnsiIndexStr(jsonObj.GetValue<string>('status', ''), STATUS_COBRANCA);

        Self.CobrancaStatusString := STATUS_COBRANCA[indexStatus];
        Self.CobrancaStatus := TCobrancaStatus(Integer(indexStatus));
        Self.CobrancaId := jsonObj.GetValue<string>('id', '');
        Self.DataCriacao := jsonObj.GetValue<string>('created_at', '');
        Self.Descricao := jsonObj.GetValue<string>('description', '');
        Self.Identificador := jsonObj.GetValue<string>('reference_id', '');
    finally
        jsonObj.DisposeOf;
    end;
end;

function TPagSeguro.ToJSON: string;
var
  FCobranca: TCobranca;
begin
    try
      FCobranca := TCobranca.Create;
      FCobranca.reference_id := Self.Identificador;
      FCobranca.description := Self.Descricao;

      FCobranca.amount.value := Self.ValorTotal;
      FCobranca.amount.currency := Self.Moeda;

      FCobranca.payment_method.&type := 'CREDIT_CARD';
      FCobranca.payment_method.installments := Self.NumParcelas;
      FCobranca.payment_method.capture := Self.Capture;
      FCobranca.payment_method.soft_descriptor := Self.NomeNaFatura;
      FCobranca.payment_method.card.number := Self.NumeroCartao;
      FCobranca.payment_method.card.exp_month := Self.MesExpiracao;
      FCobranca.payment_method.card.exp_year := Self.AnoExpiracao;
      FCobranca.payment_method.card.security_code := Self.CodigoVerificacao;
      FCobranca.payment_method.card.holder.name := Self.NomeTitular;
      FCobranca.notification_urls[0] := Self.URLNotificacao;

      Result := TJson.ObjectToJsonString(FCobranca);
    finally
      if Assigned(FCobranca) then
        FCobranca.DisposeOf;
    end;
end;

procedure TPagSeguro.SetAmbiente(const Value: TAmbiente);
begin
    FAmbiente := Value;

    if Value = HOMOLOGACAO then
        URL_BASE := URL_HOMOLOGACAO
    else
        URL_BASE := URL_PRODUCAO;
end;

function TPagSeguro.CobrarCartao: boolean;
var
    http: THTTPClient;
    body: TStringStream;
    response: IHttpResponse;
begin
    try
        http := THTTPClient.Create;
        http.ContentType := 'application/json';
        http.Accept := '*/*';

        body := TStringStream.Create(Self.ToJson);

        response := http.Post(URL_BASE + '/charges',
                    body,
                    nil,
                    //TNetHeaders.Create(TNameValuePair.Create('Authorization', 'Bearer' + FToken))
                    TNetHeaders.Create(TNameValuePair.Create('Authorization', FToken))
                    );

        Self.StatusCode := response.StatusCode;
        Self.ResponseMessage := response.ContentAsString;

        Result := response.StatusCode = 201;

        if Result then
        begin
            ProcessarResponseCobranca(response.ContentAsString);
        end;
    finally
        http.DisposeOf;

        if Assigned(body) then
            body.DisposeOf;
    end;
end;

function TPagSeguro.ConsultarCobranca(ACobrancaId: string): boolean;
var
    http: THTTPClient;
    response: IHttpResponse;
begin
    try
        http := THTTPClient.Create;
        http.ContentType := 'application/json';
        http.Accept := '*/*';

        response := http.Get(URL_BASE + '/charges/' + ACobrancaId,
                    nil,
                    TNetHeaders.Create(TNameValuePair.Create('Authorization', FToken))
                    );

        Self.StatusCode := response.StatusCode;
        Self.ResponseMessage := response.ContentAsString;

        Result := response.StatusCode = 200;

        if Result then
            ProcessarResponseConsulta(response.ContentAsString);

    finally
        http.DisposeOf;
    end;
end;

{ TCobranca }

constructor TCobranca.Create;
begin
    FAmount := TAmount.Create;
    FPayment_method := TPaymentMethod.Create;

end;

destructor TCobranca.Destroy;
begin
    FAmount.DisposeOf;
    FPayment_method.DisposeOf;

    inherited;
end;

function TCobranca.GetNotification_urls(Index: Integer): String;
begin
    Result := FNotification_urls[Index];
end;

procedure TCobranca.SetNotification_urls(Index: Integer; const Value: String);
begin
    FNotification_urls[Index] := Value;
end;

{ TPaymentMethod }

constructor TPaymentMethod.Create;
begin
    FCard := TCard.Create;
end;

destructor TPaymentMethod.Destroy;
begin
    FCard.DisposeOf;
    inherited;
end;

{ TCard }

constructor TCard.Create;
begin
    FHolder := TCardHolder.Create;
end;

destructor TCard.Destroy;
begin
    FHolder.DisposeOf;
    inherited;
end;

end.
