module Cliente (adicionarCliente, alterarCliente, removerCliente,
				exibirCliente, exibirMaisCompraram) where

import Control.DeepSeq
import Control.Exception
import Data.Time.Calendar
import Data.List
import Data.Char

import OperacoesComuns

adicionarCliente :: IO ()
adicionarCliente = do
	clientesFile <- readFile "cliente.db"
	let clientes = lines clientesFile
	let codigo = novaId clientes
	print ("Digite o nome")
	nome <- getLine
	print ("Digite a cidade")
	cidade <- getLine
	print ("Digite a idade")
	idade <- getLine
	print ("Digite o sexo")
	sexo <- getLine

	evaluate (force clientesFile)
	appendFile "cliente.db" (""++codigo++","++nome++","++cidade++","++idade++","++sexo++"\n")

alterarCliente :: IO ()
alterarCliente = do
	arquivo <- readFile "cliente.db"
	let clientes = lines arquivo
	print ("Digite o id a ser alterado")
	id <- getLine
	print ("Digite um novo nome")
	nome <- getLine
	print ("Digite um novo cidade")
	cidade <- getLine
	print ("Digite um novo idade")
	idade <- getLine
	print ("Digite um novo sexo")
	sexo <- getLine
	let novoRegistro = (""++id++","++nome++","++cidade++","++idade++","++sexo)
	let clientesAlterado = alterarRegistro clientes novoRegistro id 
	evaluate (force arquivo)
	writeFile "cliente.db" (unlines clientesAlterado)

removerCliente :: IO ()
removerCliente = do
	arquivoClientes <- readFile "cliente.db"
	arquivoVendas <- readFile "venda.db"
	let clientes = lines arquivoClientes
	let vendas = lines arquivoVendas
	print ("Digite o id a ser excluido")
	id <- getLine
	if (clienteSemVendas vendas id) then do
		let clientesAlterado = removerRegistro clientes id
		evaluate (force arquivoClientes)
		writeFile "cliente.db" (unlines clientesAlterado)
	else
		print ("Esse cliente possui vendas. Nao e possivel excluir")

clienteSemVendas :: [String] -> String -> Bool
clienteSemVendas [] _ = True
clienteSemVendas (a:b) id | (pegarAtributo (quebrarString ',' a) 1) == id = False
					  | otherwise = clienteSemVendas b id

exibirCliente :: IO ()
exibirCliente = do
	print ("#Codigo, nome, cidade, idade, sexo")
	arquivoClientes <- readFile "cliente.db"
	let clientes = lines arquivoClientes
	imprimirClientes clientes

imprimirClientes :: [String] -> IO ()
imprimirClientes [] = return ()
imprimirClientes (a:b) = do
	let registro = quebrarString ',' a
	let codigo = pegarAtributo registro 0
	let nome = pegarAtributo registro 1
	let cidade = pegarAtributo registro 2
	let idade = pegarAtributo registro 3
	let tempSexo = pegarAtributo registro 4
	let sexo = if ((toUpper (tempSexo!!0)) == 'F') then "Feminino" else "Masculino"
	print (codigo++","++nome++","++cidade++","++idade++","++sexo)
	imprimirClientes b

exibirMaisCompraram :: IO ()
exibirMaisCompraram = do
	arquivoClientes <- readFile "cliente.db"
	let clientes = lines arquivoClientes
	arquivoVendas <- readFile "venda.db"
	let vendas = lines arquivoVendas
	print ("Digite a data inicial (DD/MM/YYYY)")
	dataInicialString <- getLine
	let dataInicio = criarData dataInicialString
	print ("Digite a data final (DD/MM/YYYY)")
	dataFinalString <- getLine
	let dataFim = criarData dataFinalString

	let informacoes = pegarInformacoesMaisCompram clientes vendas dataInicio dataFim
	let informacoesCrescente = sortBy (\x y-> compare (read ((quebrarString ',' x)!!2)::Float) (read ((quebrarString ',' y)!!2)::Float)) informacoes
	let informacoesDecrescente = inverterLista informacoesCrescente
	print ("#Codigo, nome, faturamento total")
	imprimir informacoesDecrescente

pegarInformacoesMaisCompram :: [String] -> [String] -> Day -> Day -> [String]
pegarInformacoesMaisCompram [] _ _ _ = []
pegarInformacoesMaisCompram (a:b) vendas dataInicio dataFim = do
	let cliente = quebrarString ',' a
	let codigo = pegarAtributo cliente 0
	let vendasCliente = buscarNRegistros vendas 1 codigo
	let vendasNoPeriodo = filtrarDataVendas vendasCliente dataInicio dataFim
	if (vendasNoPeriodo /= []) then do
		let nome = pegarAtributo cliente 1
		let faturamentoTotal = calcularFaturamentoTotal vendasNoPeriodo
		let informacoes = codigo ++ "," ++ nome ++ "," ++ (show faturamentoTotal)
		informacoes:pegarInformacoesMaisCompram b vendas dataInicio dataFim
	else
		pegarInformacoesMaisCompram b vendas dataInicio dataFim

calcularFaturamentoTotal :: [String] -> Float
calcularFaturamentoTotal [] = 0
calcularFaturamentoTotal (a:b) = do
	let venda = quebrarString ',' a
	let faturamento = (read (pegarAtributo venda 5)::Float)
	faturamento+calcularFaturamentoTotal b

filtrarDataVendas :: [String] -> Day -> Day -> [String]
filtrarDataVendas [] _ _ = []
filtrarDataVendas (a:b) dataInicio dataFim = do
	let venda = quebrarString ',' a
	let diaVenda = (read (pegarAtributo venda 2)::Int)
	let mesVenda = (read (pegarAtributo venda 3)::Int)
	let anoVenda = (read (pegarAtributo venda 4)::Integer)
	let dataVenda = fromGregorian anoVenda mesVenda diaVenda 

	if (((diffDays dataVenda dataInicio) >= 0) && ((diffDays dataFim dataVenda) >= 0)) then
		a:filtrarDataVendas b dataInicio dataFim
	else
		filtrarDataVendas b dataInicio dataFim