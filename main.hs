import OperacoesComuns
import Cliente
import Produto
import System.Exit
import Control.DeepSeq
import Control.Exception

pedirItens :: IO ()
pedirItens = do
	print ("Digite o codigo do produto")
	codigoProduto <- getLine
	arquivoProdutos <- readFile "arquivoProdutos.db"
	let produtos = lines arquivoProdutos
	evaluate (force arquivoProdutos)
	let registroProduto = buscarRegistro produtos 0 codigoProduto
	if (registroProduto == []) then do
		print ("Produto nao existente")
		-- TODO aqui tem que remover o arquivo temporario
		exitSuccess
	else
		print ("Escolha a quantidade")
	quantidadeInicialString <- getLine

	-- garante que a quantidade não excederá o estoque mesmo entre vendas
	appendFile "" "vendatemp"
	arquivoVendaTemp <- readFile "vendatemp"
	let vendasTemp = lines arquivoVendaTemp
	evaluate (force arquivoVendaTemp)
	let quantidadeInicial = (read quantidadeInicialString::Int)
	let registroItem = buscarRegistro vendasTemp 0 codigoProduto 
	if (registroItem == []) then
		let quantidade = quantidadeInicial
	else
		let quantidade = quantidadeInicial + (read (pegarAtributo 1 registroItem)::Int)

	-- agora efetivamente checa se a quantidade não excede o estoque
	let estoque = read (pegarAtributo registroProduto 2)::Int
	if (quantidade > estoque) then
		print ("Quantidade excede o estoque")
		-- TODO aqui tem que remover o arquivo temporario
		exitSuccess
	else
		print ("Digite o desconto aplicado (0 para nenhum)")

	descontoString <- getLine
	let desconto = (read descontoString::Float)
	if ((desconto > 10) || (desconto < 0)) then
		print ("Desconto deve estrar entre 0% e 10%")
		-- TODO excluir o arquivo
		exitSuccess
	

registrarVenda :: IO ()
registrarVenda = do
	arquivoClientes <- readFile "cliente.db"
	let clientes = lines arquivoClientes
	print ("Digite o codigo do cliente")
	codigoCliente <- getLine
	if (buscarRegistro clientes 0 codigoCliente == []) then do
		print ("Cliente nao encontrado")
		exitSuccess
	else
		pedirItens

main = do
	print ("Digite a acao desejada.")
	print ("1- Adicionar registro")
	print ("2- Alterar registro")
	print ("3- Excluir registro")
	escolhaString <- getLine
	let escolha = (read escolhaString::Int)
	if (escolha == 1) then do
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
	else
		if (escolha == 2) then do
			print ("Escolha o registro que deseja alterar")
			print ("1- Cliente")
			print ("2- Produto")
			alterarString <- getLine
			let alterar = (read alterarString::Int)
			if (alterar == 1) then
				alterarCliente
			else
				alterarProduto
		else do
			print ("Escolha o registro que deseja remover")
			print ("1- Cliente")
			print ("2- Produto")
			removerString <- getLine
			let remover = (read removerString::Int)
			if (remover == 1) then
				removerCliente
			else
				removerProduto
