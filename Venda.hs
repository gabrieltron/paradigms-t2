module Venda (registrarVenda, exibirVendasCliente) where

import Control.DeepSeq
import Control.Exception
import System.Exit
import OperacoesComuns
import Produto

registrarVenda :: IO ()
registrarVenda = do
	arquivoClientes <- readFile "cliente.db"
	let clientes = lines arquivoClientes
	print ("Digite o codigo do cliente")
	codigoCliente <- getLine
	if (buscarRegistro clientes 0 codigoCliente == []) then do
		print ("Cliente nao encontrado")
	else do
		arquivoVendas <- readFile "venda.db"
		let vendas = lines arquivoVendas
		evaluate (force arquivoVendas)
		let codigoVenda = novaId vendas
		-- registra os itens na venda
		pedirItens codigoVenda codigoCliente
		print ("Venda concluida.")

pedirItens :: String -> String -> IO ()
pedirItens codigoVenda codigoCliente = do
	print ("Digite o codigo do produto")
	codigoProduto <- getLine
	arquivoProdutos <- readFile "produto.db"
	let produtos = lines arquivoProdutos
	evaluate (force arquivoProdutos)
	let registroProduto = buscarRegistro produtos 0 codigoProduto
	if (registroProduto == []) then do
		print ("Produto nao existente")
		print ("Venda encerrada")
		exitSuccess
	else
		return ()

	-- checa se a quantidade não excede o estoque
	print ("Escolha a quantidade")
	quantidadeString <- getLine
	let quantidade = (read quantidadeString::Int)
	let estoque = (read (pegarAtributo registroProduto 2)::Int)
	if (quantidade > estoque) then do
		print ("Quantidade excede o estoque")
		print ("Venda encerrada")
		exitSuccess
	else
		return ()

	-- calcula desconto
	print ("Digite o desconto aplicado (0 para nenhum)")
	descontoString <- getLine
	let desconto = arredondarFloat (read descontoString::Float)
	if ((desconto > 10) || (desconto < 0)) then do
		print ("Desconto deve estrar entre 0% e 10%")
		print ("Venda encerrada")
		exitSuccess
	else
		return ()
	let precoUnitario = read (pegarAtributo registroProduto 3)::Float
	let precoSemDesconto = precoUnitario * (fromIntegral quantidade)
	let precoComDesconto = arredondarFloat (precoSemDesconto * ((100-desconto) / 100))

	-- faz alterações necessárias em arquivos
	let nomeProduto = pegarAtributo registroProduto 1
	let novoEstoque = show (estoque - quantidade)
	escreverAlteracaoProduto codigoProduto nomeProduto novoEstoque (show precoUnitario)  -- Atualiza o produto com o novo estoque
	-- coloca um novo item no itemvenda
	itemVendaArquivo <- readFile "itemvenda.db"
	let itemVenda = lines itemVendaArquivo
	evaluate (force itemVendaArquivo)
	let codigoItem = novaId (buscarNRegistros itemVenda 1 codigoVenda)
	appendFile "itemvenda.db" (""++codigoItem++","++codigoVenda++","++codigoProduto++","++(show precoUnitario)++","++(show desconto)++","++(show quantidade)++","++(show precoComDesconto)++"\n")
	-- atualiza arquivo de vendas
	processarVenda codigoVenda codigoCliente precoComDesconto

	-- oferece para registrar um novo item na venda
	print ("Venda registrada. Deseja incluir outro item na venda? [s/n]")
	outroItem <- getLine
	if (outroItem == "s") then
		pedirItens codigoVenda codigoCliente
	else
		return ()

processarVenda :: String -> String -> Float -> IO ()
processarVenda codigoVenda codigoCliente valor = do
	arquivoVenda <- readFile "venda.db"
	let vendas = lines arquivoVenda
	evaluate (force arquivoVenda)
	let venda = buscarRegistro vendas 0 codigoVenda
	let total = if (venda == []) then valor else valor + (read (pegarAtributo venda 5)::Float)
	(ano, mes, dia) <- date
	let novoRegistro = (""++codigoVenda++","++codigoCliente++","++(show dia)++","++(show mes)++","++(show ano)++","++(show total))
	if (venda == []) then  -- se o registro em venda ainda não existe, cria
		appendFile "venda.db" (novoRegistro++"\n")
	else do -- se não atualiza o existente
		let vendasAtualizadas = alterarRegistro vendas novoRegistro codigoVenda
		writeFile "venda.db" (unlines vendasAtualizadas)

valorTotal :: [String] -> Float
valorTotal [] = 0
valorTotal (a:b) = (read (pegarAtributo (quebrarString ',' a) 6)::Float)+valorTotal b

exibirVendasCliente :: IO ()
exibirVendasCliente = do
	arquivoVendas <- readFile "venda.db"
	let vendas = lines arquivoVendas
	print ("Informe o codigo do cliente")
	cliente <- getLine
	let vendasCliente = buscarNRegistros vendas 1 cliente
	print ("#Codigo venda, codigo cliente, dia, mes ,ano, total")
	imprimirVendas vendasCliente

imprimirVendas :: [String] -> IO ()
imprimirVendas [] = return ()
imprimirVendas (a:b) = do
	print (a)
	imprimirVendas (b)