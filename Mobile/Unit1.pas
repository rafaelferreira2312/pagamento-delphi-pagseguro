unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Objects, FMX.TabControl, FMX.Layouts,
  PagSeguro;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    edtFatura: TEdit;
    btnConsultar: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Layout1: TLayout;
    imgCard: TImage;
    rectComprar: TRectangle;
    Label1: TLabel;
    edtCartao: TEdit;
    edtMes: TEdit;
    edtAno: TEdit;
    edtVerif: TEdit;
    edtNome: TEdit;
    procedure rectComprarClick(Sender: TObject);
    procedure btnConsultarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{
Plataforma Pagseguro:
https://sandbox.pagseguro.uol.com.br/transacoes.html

'-- STATUS DA FATURA --
1 	Aguardando pagamento: o comprador iniciou a transa��o, mas at� o momento o PagSeguro n�o recebeu nenhuma informa��o sobre o pagamento.
2 	Em an�lise: o comprador optou por pagar com um cart�o de cr�dito e o PagSeguro est� analisando o risco da transa��o.
3 	Paga: a transa��o foi paga pelo comprador e o PagSeguro j� recebeu uma confirma��o da institui��o financeira respons�vel pelo processamento.
4 	Dispon�vel: a transa��o foi paga e chegou ao final de seu prazo de libera��o sem ter sido retornada e sem que haja nenhuma disputa aberta.
5 	Em disputa: o comprador, dentro do prazo de libera��o da transa��o, abriu uma disputa.
6 	Devolvida: o valor da transa��o foi devolvido para o comprador.
7 	Cancelada: a transa��o foi cancelada sem ter sido finalizada.
8 	Debitado: o valor da transa��o foi devolvido para o comprador.
9 	Reten��o tempor�ria: o comprador contestou o pagamento junto � operadora do cart�o de cr�dito ou abriu uma demanda judicial ou administrativa (Procon).
}

procedure TForm1.btnConsultarClick(Sender: TObject);
var
    pagSeguro: TPagSeguro;
begin
    try
        pagSeguro := TPagSeguro.Create;
        pagSeguro.Token := '44107E46493D4FB899EFF0C00CF670B7';
        pagSeguro.Ambiente := HOMOLOGACAO;

        if pagSeguro.ConsultarCobranca(edtFatura.Text) then
        begin
            Memo1.Lines.Add('Fatura encontrada....');
            Memo1.Lines.Add('ID: ' + pagSeguro.CobrancaId);
            Memo1.Lines.Add('Identificador: ' + pagSeguro.Identificador);
            Memo1.Lines.Add('Status: ' + pagSeguro.CobrancaStatusString);
            Memo1.Lines.Add('Dt. Criacao: ' + pagSeguro.DataCriacao);
            Memo1.Lines.Add('Descricao: ' + pagSeguro.Descricao);
        end;

        Memo1.Lines.Add(pagSeguro.StatusCode.ToString);
        Memo1.Lines.Add(pagSeguro.ResponseMessage);
        Memo1.Lines.Add('------------------------------');
    finally
        pagSeguro.DisposeOf;
    end;
end;

procedure TForm1.rectComprarClick(Sender: TObject);
var
    pagSeguro: TPagSeguro;
begin
    try
        pagSeguro := TPagSeguro.Create;
        pagSeguro.Token := '44107E46493D4FB899EFF0C00CF670B7';
        pagSeguro.Ambiente := HOMOLOGACAO;

        // Pedido...
        pagSeguro.Identificador := 'PED123';
        pagSeguro.Descricao := 'Mensalidade Software';
        pagSeguro.ValorTotal := 990; // R$ 9,90
        pagSeguro.NomeNaFatura := '99Coders';

        // Cartao...
        pagSeguro.NumeroCartao := edtCartao.Text;
        pagSeguro.MesExpiracao := edtMes.Text.ToInteger;
        pagSeguro.AnoExpiracao := edtAno.Text.ToInteger;
        pagSeguro.CodigoVerificacao := edtVerif.Text;
        pagSeguro.NomeTitular := edtNome.Text;
        pagSeguro.URLNotificacao := 'http://23.22.2.201/notificacoes';

        if pagSeguro.CobrarCartao then
        begin
            Memo1.Lines.Add(pagSeguro.CobrancaId);
            Memo1.Lines.Add(pagSeguro.CobrancaStatusString);
            edtFatura.Text := pagSeguro.CobrancaId;
        end;

        Memo1.Lines.Add(pagSeguro.StatusCode.ToString);
        Memo1.Lines.Add(pagSeguro.ResponseMessage);
        Memo1.Lines.Add('------------------------------');
    finally
        pagSeguro.DisposeOf;
    end;
end;

end.
