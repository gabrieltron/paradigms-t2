module Venda (registrarVenda) where

import Control.DeepSeq
import Control.Exception
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
		pedirItens codigoVenda

		-- registra a venda
		itemVendaArquivo <- readFile "itemvenda.bd"
		let itemVenda = lines itemVendaArquivo
		evaluate (force itemVendaArquivo)
		let itensVendaAtual = buscarRegistro itemVenda 1 codigoVenda
		let total = valorTotal itensVendaAtual
		(ano, mes, dia) <- date
		appendFile "venda.db" (""++codigoVenda++","++codigoCliente++","++(show dia)++","++(show mes)++","++(show ano)++","++(show total))
		print ("Venda concluida.")

pedirItens :: String -> IO ()
pedirItens codigoVenda = do
	print ("Digite o codigo do produto")
	codigoProduto <- getLine
	arquivoProdutos <- readFile "arquivoProdutos.db"
	let produtos = lines arquivoProdutos
	evaluate (force arquivoProdutos)
	let registroProduto = buscarRegistro produtos 0 codigoProduto
	if (registroProduto == []) then do
		print ("Produto nao existente")
		return ()
	else
		print (" ")

	-- checa se a quantidade não excede o estoque
	print ("Escolha a quantidade")
	quantidadeString <- getLine
	let quantidade = (read quantidadeString::Int)
	let estoque = (read (pegarAtributo registroProduto 2)::Int)
	if (quantidade > estoque) then do
		print ("Quantidade excede o estoque")
		return ()
	else
		print (" ")

	-- calcula desconto
	print ("Digite o desconto aplicado (0 para nenhum)")
	descontoString <- getLine
	let desconto = arredondarFloat (read descontoString::Float)
	if ((desconto > 10) || (desconto < 0)) then do
		print ("Desconto deve estrar entre 0% e 10%")
		return ()
	else
		print (" ")
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
	let codigoItem = novaId (buscarRegistro itemVenda 1 codigoVenda)
	appendFile "itemvenda.db" (""++codigoItem++","++codigoVenda++","++codigoProduto++","++(show precoUnitario)++","++(show desconto)++","++(show quantidade)++","++(show precoComDesconto))

	-- oferece para registrar um novo item na venda
	print ("Item registrado. Deseja incluir outro item na venda? [s/n]")
	outroItem <- getLine
	if (outroItem == "s") then
		pedirItens codigoVenda
	else
		print (" ")

valorTotal :: [String] -> Float
valorTotal [] = 0
valorTotal (a:b) = (read (pegarAtributo (quebrarString ',' a) 6)::Float)+valorTotal b
