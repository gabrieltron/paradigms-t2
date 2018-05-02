import OperacoesComuns
import Cliente
import Produto
import Venda
import System.Exit
import Control.DeepSeq
import Control.Exception

escolherAdicionar :: IO ()
escolherAdicionar = do
	print ("Escolha o registo a que deseja adicionar")
	print ("1- Cliente")
	print ("2- Produto")
	print ("3- Venda")
	adicionarString <- getLine
	let adicionar = (read adicionarString::Int)
	if (adicionar == 1) then
		adicionarCliente
	else
		if (adicionar == 2) then
			adicionarProduto
		else
			registrarVenda

escolherEditar :: IO ()
escolherEditar = do
	print ("Escolha o registro que deseja alterar")
	print ("1- Cliente")
	print ("2- Produto")
	alterarString <- getLine
	let alterar = (read alterarString::Int)
	if (alterar == 1) then
		alterarCliente
	else
		alterarProduto

escolherRemover :: IO ()
escolherRemover = do
	print ("Escolha o registro que deseja remover")
	print ("1- Cliente")
	print ("2- Produto")
	removerString <- getLine
	let remover = (read removerString::Int)
	if (remover == 1) then
		removerCliente
	else
		removerProduto

escolherExibir :: IO ()
escolherExibir = do
	print ("Escolha qual relatorio deseja exibir")
	print ("1- Cliente")
	print ("2- Produto")
	print ("3- Vendas de um cliente")
	print ("4 - Exibir vendas de um periodo")
	print ("5 - Exibir mais vendidos de um periodo")
	exibirString <- getLine
	let exibir = (read exibirString :: Int)
	if (exibir == 1) then
		exibirCliente
	else
		if (exibir == 2) then
			exibirProduto
		else 
			if (exibir == 3) then
				exibirVendasCliente
			else
				if (exibir == 4) then
					exibirVendasPeriodo
				else
					exibirMaisVendidos


main = do
	-- cria db caso nao existam
	appendFile "cliente.db" ""
	appendFile "produto.db" ""
	appendFile "venda.db" ""
	appendFile "itemvenda.db" ""

	print ("Digite a acao desejada.")
	print ("1- Adicionar registro")
	print ("2- Alterar registro")
	print ("3- Excluir registro")
	print ("4- Exibir relatorio")

	escolhaString <- getLine
	let escolha = (read escolhaString::Int)
	if (escolha == 1) then do
		escolherAdicionar
	else
		if (escolha == 2) then do
			escolherEditar
		else 
			if (escolha == 3) then do
				escolherRemover
			else
				escolherExibir
