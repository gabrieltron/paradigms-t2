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
	print ("6 - Exibir cliente que mais gastou em um periodo")
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
					if (exibir == 5) then
						exibirMaisVendidos
					else
						exibirMaisCompraram

verificarCoerencia :: IO ()
verificarCoerencia = do
	arquivoClientes <- readFile "cliente.db"
	let clientes = lines arquivoClientes
	arquivoProdutos <- readFile "produto.db"
	let produtos = lines arquivoProdutos
	arquivoVendas <- readFile "venda.db"
	let vendas = lines arquivoVendas
	arquivoItemVendas <- readFile "itemvenda.db"
	let itemVendas = lines arquivoItemVendas

	let vendasSemCliente = verificarCoerenciaVendas vendas clientes produtos itemVendas
	let itemSemVendas = verificaProdutoItems itemVendas produtos
	let valorIncorreto = verificarValorVendas vendas itemVendas
	let incosistencias = vendasSemCliente ++ itemSemVendas ++ valorIncorreto
	if (incosistencias /= []) then
		imprimir incosistencias
	else
		print ("Sem inconsistencias")

verificarValorVendas :: [String] -> [String] -> [String]
verificarValorVendas [] _ = []
verificarValorVendas (a:b) itemVendas = do
	let venda = quebrarString ',' a
	let codigoVenda = pegarAtributo venda 0
	let valorRegistrado = read (pegarAtributo venda 5)::Float
	let itemsVenda = buscarNRegistros itemVendas 1 codigoVenda
	let valor = calcularValorVenda itemsVenda
	if (valor /= -1) then
		if (valorRegistrado == valor) then
			verificarValorVendas b itemVendas
		else
			("Erro no valor total da venda "++codigoVenda):verificarValorVendas b itemVendas
	else
		("Erro em itens da venda "++codigoVenda):verificarValorVendas b itemVendas

calcularValorVenda :: [String] -> Float
calcularValorVenda [] = 0
calcularValorVenda (a:b) = do
	let item = quebrarString ',' a
	let precoUnitario = read (pegarAtributo item 3)::Float
	let desconto = read (pegarAtributo item 4)::Float
	let quantidade = read (pegarAtributo item 5)::Float
	let totalRegistrado = read (pegarAtributo item 6)::Float
	let total = (precoUnitario * quantidade) * ((100-desconto) / 100)
	let totalArredondado = arredondarFloat total
	if (totalRegistrado == totalArredondado) then
		totalRegistrado + calcularValorVenda b
	else
		-1


verificarCoerenciaVendas :: [String] -> [String] -> [String] -> [String] -> [String]
verificarCoerenciaVendas [] _ _ _ = []
verificarCoerenciaVendas (a:b) clientes produtos itemvendas = do
	let venda = quebrarString ',' a
	let codigoVenda = pegarAtributo venda 0
	let codigoClienteVenda = pegarAtributo venda 1
	let clienteVenda = buscarRegistro clientes 0 codigoClienteVenda
	if (clienteVenda /= []) then
		verificarCoerenciaVendas b clientes produtos itemvendas
	else
		("Inconsistencia na venda "++codigoVenda++". Cliente nao existe")
			:verificarCoerenciaVendas b clientes produtos itemvendas

verificaProdutoItems :: [String] -> [String] -> [String]
verificaProdutoItems [] _ = []
verificaProdutoItems (a:b) produtos = do
	let itemVenda = quebrarString ',' a
	let codigoItemVenda = pegarAtributo itemVenda 0
	let codigoVenda = pegarAtributo itemVenda 1
	let codigoProduto = pegarAtributo itemVenda 2
	let produto = buscarRegistro produtos 0 codigoProduto
	if (produto == []) then
		("Inconsistencia na venda "++codigoVenda++". Produto do item "++codigoItemVenda++" nao existe")
			:verificaProdutoItems b produtos
	else
		verificaProdutoItems b produtos

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
	print ("5- Verificar coerencia do bd")

	escolhaString <- getLine
	let escolha = (read escolhaString::Int)
	if (escolha == 1) then
		escolherAdicionar
	else
		if (escolha == 2) then
			escolherEditar
		else 
			if (escolha == 3) then
				escolherRemover
			else
				if (escolha == 4) then
					escolherExibir
				else
					verificarCoerencia