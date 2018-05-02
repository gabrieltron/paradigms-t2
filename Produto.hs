module Produto (adicionarProduto, alterarProduto, removerProduto,
				escreverAlteracaoProduto, exibirProduto, exibirMaisVendidos) where

import OperacoesComuns
import Control.DeepSeq
import Control.Exception
import Data.List
import Data.Time.Calendar

adicionarProduto :: IO ()
adicionarProduto = do
	arquivo <- readFile "produto.db"
	let produtos = lines arquivo
	let codigo = novaId produtos
	print ("Digite o nome")
	nome <- getLine
	print ("Digite a quantidade em estoque")
	quantidade <- getLine
	print ("Digite o preco unitario ou por Kilo")
	preco <- getLine
	let precoArredondado = (show (arredondarFloat (read preco::Float)))

	evaluate (force arquivo)
	appendFile "produto.db" (""++codigo++","++nome++","++quantidade++","++precoArredondado++"\n")

alterarProduto :: IO ()
alterarProduto = do
	print ("Digite o id a ser alterado")
	id <- getLine
	print ("Digite o novo nome")
	nome <- getLine
	print ("Digite a nova quantidade em estoque")
	quantidade <- getLine
	print ("Digite o novo preco unitario ou por Kilo")
	preco <- getLine
	escreverAlteracaoProduto id nome quantidade preco

escreverAlteracaoProduto :: String -> String -> String -> String -> IO ()
escreverAlteracaoProduto id nome quantidade preco = do
	arquivo <- readFile "produto.db"
	let produtos = lines arquivo
	evaluate (force arquivo)
	let novoRegistro = (""++id++","++nome++","++quantidade++","++preco)
	let produtosAlterado = alterarRegistro produtos novoRegistro id
	writeFile "produto.db" (unlines produtosAlterado)


removerProduto :: IO ()
removerProduto = do
	arquivoProdutos <- readFile "produto.db"
	arquivoItemVendas <- readFile "itemvenda.db"
	let produtos = lines arquivoProdutos
	let itemVendas = lines arquivoItemVendas
	print ("Digite o id a ser excluido")
	id <- getLine
	if (produtoSemVendas itemVendas id) then do
		let produtosAlterado = removerRegistro produtos id
		evaluate (force arquivoProdutos)
		writeFile "produto.db" (unlines produtosAlterado)
	else
		print ("Esse produto possui vendas. Nao e possivel excluir")

produtoSemVendas :: [String] -> String -> Bool
produtoSemVendas [] _ = True
produtoSemVendas (a:b) id | (pegarAtributo (quebrarString ',' a) 2) == id = False
					  | otherwise = produtoSemVendas b id

exibirMaisVendidos :: IO ()
exibirMaisVendidos = do
	arquivoProdutos <- readFile "produto.db"
	let produtos = lines arquivoProdutos
	arquivoItemVendas <- readFile "itemvenda.db"
	let itemVendas = lines arquivoItemVendas
	arquivoVendas <- readFile "venda.db"
	let vendas = lines arquivoVendas
	print ("Digite a data inicial (DD/MM/YYYY)")
	dataInicialString <- getLine
	let dataInicio = criarData dataInicialString
	print ("Digite a data final (DD/MM/YYYY)")
	dataFinalString <- getLine
	let dataFim = criarData dataFinalString

	let informacoes = pegarInfomacoesPeriodo produtos itemVendas vendas dataInicio dataFim
	let informacoesCrescente = sortBy (\x y-> compare (read ((quebrarString ',' x)!!4)::Float) (read ((quebrarString ',' y)!!4)::Float)) informacoes
	let informacoesDecrescente = inverterLista informacoesCrescente
	print ("#Codigo, nome, quantidade vendida, quantidade vendas em que esta presente, faturamento total, media de preco por unidade")
	imprimir informacoesDecrescente

pegarInfomacoesPeriodo :: [String] -> [String] -> [String] -> Day -> Day -> [String]
pegarInfomacoesPeriodo [] _ _ _ _ = []
pegarInfomacoesPeriodo (a:b) itemVendas todasVendas dataInicio dataFim = do
	let produto = quebrarString ',' a
	let codigo = pegarAtributo produto 0
	let vendasIndividuais = buscarNRegistros itemVendas 2 codigo
	let vendas = buscarVendasEmPeriodo vendasIndividuais todasVendas dataInicio dataFim
	if (vendas /= []) then do
		let faturamentoTotal = calcularFaturamentoTotal vendas
		let quantidadeVendida = calcularQuantidadeVendida vendas
		let mediaFloat = faturamentoTotal / (fromInteger quantidadeVendida)
		let media = arredondarFloat mediaFloat
		let vendasIndividuais = calcularVendasIndividuais vendas []
		let informacoes = codigo ++ "," ++ (pegarAtributo produto 1) ++ "," ++ (show quantidadeVendida::String) ++ ","  ++
			(show vendasIndividuais::String) ++ "," ++ (show faturamentoTotal::String) ++ "," ++ (show media::String)
		informacoes:pegarInfomacoesPeriodo b itemVendas todasVendas dataInicio dataFim
	else
		pegarInfomacoesPeriodo b itemVendas todasVendas dataInicio dataFim

calcularVendasIndividuais :: [String] -> [Int] -> Int
calcularVendasIndividuais [] _ = 0
calcularVendasIndividuais (a:b) historico = do
	let venda = quebrarString ',' a
	let codigoVenda = (read (pegarAtributo venda 1)::Int)
	if (not (estaEm historico codigoVenda)) then do
		let novoHistorico = codigoVenda:historico
		1+calcularVendasIndividuais b novoHistorico
	else
		calcularVendasIndividuais b historico


estaEm :: [Int] -> Int -> Bool
estaEm [] _ = False
estaEm (a:b) valor = if (a == valor) then
						True
					 else
					 	estaEm b valor

calcularQuantidadeVendida :: [String] -> Integer
calcularQuantidadeVendida [] = 0
calcularQuantidadeVendida (a:b) = do
	let venda = quebrarString ',' a
	let quantidadeVendida = (read (pegarAtributo venda 5)::Integer)
	quantidadeVendida + calcularQuantidadeVendida b

calcularFaturamentoTotal :: [String] -> Float
calcularFaturamentoTotal [] = 0
calcularFaturamentoTotal (a:b) = do
	let venda = quebrarString ',' a
	let faturamento = (read (pegarAtributo venda 6)::Float)
	faturamento + calcularFaturamentoTotal b


buscarVendasEmPeriodo :: [String] -> [String] -> Day -> Day -> [String]
buscarVendasEmPeriodo [] _ _ _ = []
buscarVendasEmPeriodo (a:b) todasVendas dataInicio dataFim = do
	let vendaProduto = quebrarString ',' a
	let codigoVendaProduto = pegarAtributo vendaProduto 1
	let venda = buscarRegistro todasVendas 0 codigoVendaProduto
	let diaVenda = pegarAtributo venda 2
	let mesVenda = pegarAtributo venda 3
	let anoVenda = pegarAtributo venda 4
	let dataVenda = criarData (diaVenda ++ "/" ++ mesVenda ++ "/" ++ anoVenda)
	if (((diffDays dataVenda dataInicio) >= 0) && ((diffDays dataFim dataVenda) >= 0)) then
		a:buscarVendasEmPeriodo b todasVendas dataInicio dataFim
	else
		buscarVendasEmPeriodo b todasVendas dataInicio dataFim

exibirProduto :: IO ()
exibirProduto = do
	print ("#Codigo, nome, estoque, preco unitario ou kilo")
	arquivoProdutos <- readFile "produto.db"
	let produtos = lines arquivoProdutos
	imprimir produtos

