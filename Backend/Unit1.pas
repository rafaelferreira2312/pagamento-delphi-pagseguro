unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.JSON,
  System.Net.HttpClient, System.Net.URLClient, REST.Json;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    procedure FormShow(Sender: TObject);
  private
    function ConsultarNotificacao(aID: string): string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses IdSSLOpenSSL,
     Horse,
     Horse.Jhonson,
     Horse.Compression,
     Horse.OctetStream,
     Horse.CORS;

{$R *.fmx}

Const
    Token = '??????????????????';
    Ambiente = 'HOMOLOGACAO';
	Email = 'seuemail@teste.com';

{
'-- STATUS DA FATURA --
1 	Aguardando pagamento: o comprador iniciou a transação, mas até o momento o PagSeguro não recebeu nenhuma informação sobre o pagamento.
2 	Em análise: o comprador optou por pagar com um cartão de crédito e o PagSeguro está analisando o risco da transação.
3 	Paga: a transação foi paga pelo comprador e o PagSeguro já recebeu uma confirmação da instituição financeira responsável pelo processamento.
4 	Disponível: a transação foi paga e chegou ao final de seu prazo de liberação sem ter sido retornada e sem que haja nenhuma disputa aberta.
5 	Em disputa: o comprador, dentro do prazo de liberação da transação, abriu uma disputa.
6 	Devolvida: o valor da transação foi devolvido para o comprador.
7 	Cancelada: a transação foi cancelada sem ter sido finalizada.
8 	Debitado: o valor da transação foi devolvido para o comprador.
9 	Retenção temporária: o comprador contestou o pagamento junto à operadora do cartão de crédito ou abriu uma demanda judicial ou administrativa (Procon).



URL Documentacao:
https://dev.pagseguro.uol.com.br/v1.0/docs/api-notificacao-v1?_ga=2.26331197.1281462722.1648218567-1842443867.1626873974#recebendo-uma-notificacao-de-transacao
}


function ConsultarNotificacao(aID: string): string;
var
    http: THTTPClient;
    response: IHttpResponse;
    url_base: string;
begin
    try
        if Ambiente = 'PRODUCAO' then
            url_base := 'https://ws.pagseguro.uol.com.br'
        else
            url_base := 'https://ws.sandbox.pagseguro.uol.com.br';

        http := THTTPClient.Create;
        http.ContentType := 'application/json';
        http.Accept := '*/*';

        response := http.Get(url_base + '/v3/transactions/notifications/' + aID + '?email=' + Email + '&token=' + Token);

        Result := response.StatusCode.ToString + ', ' + response.ContentAsString;

    finally
        http.DisposeOf;
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
    THorse.Use(Compression());
    THorse.Use(Jhonson());
    THorse.Use(OctetStream);
    THorse.Use(CORS);

    THorse.Post('/notificacoes', procedure (Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
        notificationCode, notificationType: string;
        body: TJSONObject;
    begin
        try
            body := req.Body<TJSONObject>;

            if NOT body.ToJSON.IsEmpty then
                memo.Lines.Add(body.ToJSON);
        except
        end;


        // Notificacao via POST (alteracao de status)
        if Req.ContentFields.TryGetValue('notificationCode', notificationCode) then
        begin
            memo.Lines.Add(notificationCode);

            if Req.ContentFields.TryGetValue('notificationType', notificationType) then
                memo.Lines.Add(notificationType);

            memo.Lines.Add('Consultando a Notificacao...');
            Memo.Lines.Add(ConsultarNotificacao(notificationCode));
        end;


        memo.Lines.Add('----------------');
    end);

    THorse.Listen(80, procedure(Horse: THorse)
    begin
        Memo.lines.Add('Servidor executando na porta: ' + Horse.Port.ToString);
    end);
end;

end.
